source("multitable-preprocessing.R")
source("extras.R")
library(ade4)
library(ggplot2)
library(ggrepel)
library(viridis)
library(gridExtra)
## We need:
## - A data frame with language variables (statis_df_all)
## - A data frame with the instrumental variables
## - A data frame with other demographic variables (for plotting)

statis_df_all = statis_and_demographic$statis
demographics_df_all = statis_and_demographic$demographic
aphasics = which(demographics_df_all$type == "aphasia")
statis_df_aphasics = statis_df_all[aphasics,]
demographics_df_aphasics = demographics_df_all[aphasics,]
non_excluded = !(demographics_df_aphasics$Group %in% excluded_aphasias)
demographics_df_aphasics = demographics_df_aphasics[non_excluded,]
statis_df_aphasics = statis_df_aphasics[non_excluded,]
demographics_df_aphasics$Group = droplevels(demographics_df_aphasics$Group)
instrumental_variables = c("SpontSp.Score.for.AQ",
                           "AudVbl.Comp.Score.for.AQ",
                           "Rep.Score.for.AQ", "Naming.Score.for.AQ")
## compute Aphasia AQ for NA values
computed_aphasia_aq = 2 * rowSums(demographics_df_aphasics[,instrumental_variables])
na_vals_for_AQ = which(is.na(demographics_df_aphasics$Aphasia.AQ))
demographics_df_aphasics$Aphasia.AQ[na_vals_for_AQ] = computed_aphasia_aq[na_vals_for_AQ]
demographics_df_aphasics$severity = make_aq_bins(demographics_df_aphasics$Aphasia.AQ)

## make an ordered factor for the aphasia types
demographics_df_aphasics$group_for_plotting =
    factor(group_map[as.character(demographics_df_aphasics$Group)],
           levels = group_map, ordered = TRUE)
demographics_df_aphasics$task_for_plotting =
    factor(task_map_ordered_2[as.character(demographics_df_aphasics$task)],
           levels = task_map_ordered_2, ordered = TRUE)
AQ_lin_comb = c(rep(1,4), rep(0,  6))

X = statis_df_aphasics
Z = demographics_df_aphasics[,instrumental_variables]
cc = complete.cases(Z)
X = as.matrix(statis_df_aphasics[cc,])
Z = as.matrix(demographics_df_aphasics[cc,instrumental_variables])
X_no_AQ = apply(X, 2, function(x) resid(lm(x ~ rowSums(Z))))
## Table 3: summary of the predictor variables
Z_mean_and_sd = apply(Z, 2, function(x) c(mean(x), sd(x)))
Z_summary_formatted = data.frame(Variable = colnames(Z_mean_and_sd),
                                 round(t(Z_mean_and_sd), digits = 2))
colnames(Z_summary_formatted)[2:3] = c("Mean", "SD")
cat(tex_formatted_table(Z_summary_formatted, colnames = colnames(Z_summary_formatted),
                        alignment = "l l l l l"))


M = model.matrix(~ 0 + Group, demographics_df_aphasics[cc,])
Madd = model.matrix(~ 0 + Group + task, demographics_df_aphasics[cc,])
Zadd = model.matrix(~ 0 + Z + task, demographics_df_aphasics[cc,])
Mint = model.matrix(~ 0 + Group * task, demographics_df_aphasics[cc,])
Zint = model.matrix(~ 0 + Z * task, demographics_df_aphasics[cc,])
AQadd = model.matrix(~ 0 + Aphasia.AQ + task, demographics_df_aphasics[cc,])
pca_iv = function(X, Z, k) {
    X = scale(X, center = TRUE, scale = FALSE)
    Z = scale(Z, center = TRUE, scale = FALSE)
    Gamma = t(Z) %*% Z
    Theta = t(X) %*% Z
    B = MASS::ginv(Gamma) %*% t(Theta)
    Xhat = Z %*% B
    P = adaptiveGPCA::gpca(Xhat, Q = diag(rep(1, ncol(Xhat))), k = 2)$V
    Xhatproj = Xhat %*% P
    Xproj = X %*% P
    BP = B %*% P
    R2 = 1 - sum((X - Xhat)^2) / sum(X^2)
    resid = X - Xhat
    P = data.frame(P)
    P$VarNames = colnames(X)
    BP = data.frame(BP)
    BP$VarNames = colnames(Z)
    return(list(R2 = R2, resid = resid, Xproj = Xproj, Xhatproj = Xhatproj, BP = BP, B = B,  P = P, Z = Z))
}

f_test = function(m1, m2) {
    rss1 = sum(m1$resid^2)
    rss2 = sum(m2$resid^2)
    ## number of samples is number of elements in the matrix we are predicting
    n = nrow(m1$Xproj) * ncol(m1$Xproj)
    ## number of coefficients is the size of B
    p1 = nrow(m1$BP) * nrow(m2$P)
    p2 = nrow(m2$BP) * nrow(m2$P)
    num = (rss1 - rss2) / (p2 - p1)
    denom = rss2 / (n - p2)
    F = num / denom
    pval = 1 - pf(q = F, df1 = p2 - p1, df2 =  n - p2)
    return(list(F = F, pval = pval))
}

pca_iv(X, Z, k = 2)$R2
pca_iv(X, M, k = 2)$R2
pca_iv(X, Zadd, k = 2)$R2
pca_iv(X, Madd, k = 2)$R2
pca_iv(X, AQadd, k = 2)$R2
pca_iv(X, Zint, k = 2)$R2
pca_iv(X, Mint, k = 2)$R2
plot(pca_iv(X, Zadd, k = 2)$resid ~ pca_iv(X, Madd, k = 2)$resid)
## f test for AQ additive model vs Z additive model: pval of .02
f_test(pca_iv(X, AQadd, k = 2), pca_iv(X, Zadd, k = 2))
## additive model vs. interaction model: pval 1 in both cases
f_test(pca_iv(X, Zadd, k = 2), pca_iv(X, Zint, k = 2))
f_test(pca_iv(X, Madd, k = 2), pca_iv(X, Mint, k = 2))
## only score vs. score + task: pval = 0
f_test(pca_iv(X, Z, k = 2), pca_iv(X, Zadd, k = 2))
f_test(pca_iv(X, M, k = 2), pca_iv(X, Madd, k = 2))

## we like the additive model the best
pca_iv_z_add = pca_iv(X, Zadd, k = 2)
pca_iv_z_add_no_aq = pca_iv(X_no_AQ, Zadd, k = 2)
## facet by task, color by aphasia type
p_samples_1 = ggplot(data.frame(pca_iv_z_add$Xhatproj, demographics_df_aphasics[cc,])) +
    geom_point(aes(x = Axis1, y = Axis2, color = group_for_plotting), size = .5, alpha = .5) +
    stat_ellipse(aes(x = Axis1, y = Axis2, color = group_for_plotting)) + 
    facet_wrap(~ task_for_plotting) + scale_color_viridis_d("") +
    xlab("Axis 1") + ylab("Axis 2")
## facet by aphasia type, color by task
p_samples_2 = ggplot(data.frame(pca_iv_z_add$Xhatproj, demographics_df_aphasics[cc,])) +
    geom_point(aes(x = Axis1, y = Axis2, color = task_for_plotting), size = .5, alpha = .5) +
    stat_ellipse(aes(x = Axis1, y = Axis2, color = task_for_plotting)) +
    facet_wrap(~ group_for_plotting) +
    scale_color_viridis_d("") + xlab("Axis 1") + ylab("Axis 2")
## facet by severity, color by task
ggplot(data.frame(pca_iv_z_add$Xhatproj, demographics_df_aphasics[cc,])) +
    geom_point(aes(x = Axis1, y = Axis2, color = task)) + facet_wrap(~ severity)
## facet by task, color by AQ
ggplot(data.frame(pca_iv_z_add$Xhatproj, demographics_df_aphasics[cc,])) +
    geom_point(aes(x = Axis1, y = Axis2, color = severity)) +
    facet_wrap(~ task) +
    scale_color_viridis_d()


add_aq_lin_comb = function(aq_lin_comb, BP) {
    AQ_proj = AQ_lin_comb %*% as.matrix(pca_iv_z_add$BP[,c("Axis1", "Axis2")])
    BP = rbind(BP, data.frame(Axis1 = AQ_proj[1], Axis2 = AQ_proj[2], VarNames = "AQ"))
    return(BP)
}
## check with brie that this is the right combination
BP_with_AQ = add_aq_lin_comb(aq_lin_comb = c(rep(1,4), rep(0,6)), pca_iv_z_add$BP)
task_score_indices = 5:10
ggplot(BP_with_AQ[task_score_indices,]) + 
    geom_segment(aes(x = 0, y = 0, xend = Axis1, yend = Axis2)) +
    geom_text_repel(aes(x = Axis1, y = Axis2, label = VarNames)) +
    coord_fixed() + xlab("Axis 1") + ylab("Axis 2")
iv_plot = ggplot(pca_iv_z_add$BP[-task_score_indices,]) + 
    geom_segment(aes(x = 0, y = 0, xend = Axis1, yend = Axis2)) +
    geom_label(aes(x = Axis1, y = Axis2, label = iv_map[as.character(VarNames)]),
               lineheight=.9,size=2.5,label.padding=unit(.1,"lines")) +
    coord_fixed() +
    scale_x_continuous(limits = c(-.06, .12)) + xlab("Axis 1") + ylab("Axis 2")

variable_plot = ggplot(pca_iv_z_add$P) + 
    geom_segment(aes(x = 0, y = 0, xend = Axis1, yend = Axis2)) +
    geom_label_repel(aes(x = Axis1, y = Axis2, label = variable_name_map[as.character(VarNames)]),
                     min.segment.length=.1,segment.size = .2,
                     segment.color="darkgray",
                     lineheight=.9,size=2.5,label.padding=unit(.1,"lines")) +
    coord_fixed() +
    scale_x_continuous(limits = c(-.25, .7), breaks = seq(-.2,1,by=.2)) +
    xlab("Axis 1") + ylab("Axis 2")

if(OLD_VERSION) {
    pdf("fig-pca-iv-from-scores.pdf", width = 10, height = 8)
    grid.arrange(p_samples_1, variable_plot, p_samples_2, iv_plot,
                 ncol = 2, widths = c(1, .6), heights = c(1,1))
    dev.off()
} else {
    pdf("fig-pca-iv-from-scores-new.pdf", width = 10, height = 8)
    grid.arrange(p_samples_1, variable_plot, p_samples_2, iv_plot,
                 ncol = 2, widths = c(1, .6), heights = c(1,1))
    dev.off()
}

## Table something, regression coefficients
regression_coefficients = t(round(pca_iv_z_add$B, digits = 3))
regression_tab = data.frame(ResponseVar = colnames(pca_iv_z_add$B), regression_coefficients)
colnames(regression_tab)[2:ncol(regression_tab)] = colnames(Zadd)
alignment = paste(rep('l', ncol(regression_tab)), collapse = "")
cat(tex_formatted_table(regression_tab, colnames(regression_tab), alignment))

## same thing with AQ regressed out

p_samples_1 = ggplot(data.frame(pca_iv_z_add_no_aq$Xhatproj, demographics_df_aphasics[cc,])) +
    geom_point(aes(x = Axis1, y = Axis2, color = group_for_plotting), size = .5, alpha = .5) +
    stat_ellipse(aes(x = Axis1, y = Axis2, color = group_for_plotting)) + 
    facet_wrap(~ task_for_plotting) + scale_color_viridis_d("")
## facet by aphasia type, color by task
p_samples_2 = ggplot(data.frame(pca_iv_z_add_no_aq$Xhatproj, demographics_df_aphasics[cc,])) +
    geom_point(aes(x = Axis1, y = Axis2, color = task_for_plotting), size = .5, alpha = .5) +
    stat_ellipse(aes(x = Axis1, y = Axis2, color = task_for_plotting)) +
    facet_wrap(~ group_for_plotting) +
    scale_color_viridis_d("")
add_aq_lin_comb = function(aq_lin_comb, BP) {
    AQ_proj = AQ_lin_comb %*% as.matrix(pca_iv_z_add_no_aq$BP[,c("Axis1", "Axis2")])
    BP = rbind(BP, data.frame(Axis1 = AQ_proj[1], Axis2 = AQ_proj[2], VarNames = "AQ"))
    return(BP)
}
## check with brie that this is the right combination
BP_with_AQ = add_aq_lin_comb(aq_lin_comb = c(rep(1,4), rep(0,6)), pca_iv_z_add_no_aq$BP)
task_score_indices = 5:10
iv_plot = ggplot(pca_iv_z_add_no_aq$BP[-task_score_indices,]) + 
    geom_segment(aes(x = 0, y = 0, xend = Axis1, yend = Axis2)) +
    geom_label(aes(x = Axis1, y = Axis2, label = iv_map[as.character(VarNames)]),
               lineheight=.9,size=2.5,label.padding=unit(.1,"lines")) +
    coord_fixed()

variable_plot = ggplot(pca_iv_z_add_no_aq$P) + 
    geom_segment(aes(x = 0, y = 0, xend = Axis1, yend = Axis2)) +
    geom_label(aes(x = Axis1, y = Axis2, label = variable_name_map[as.character(VarNames)]),
               lineheight=.9,size=2.5,label.padding=unit(.1,"lines")) +
    coord_fixed()
grid.arrange(p_samples_1, variable_plot, p_samples_2, iv_plot,
             ncol = 2, widths = c(1, .6), heights = c(1,1))

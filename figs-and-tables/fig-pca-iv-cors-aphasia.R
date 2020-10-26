source("multitable-preprocessing.R")
source("extras.R")
library(ade4)
library(ggplot2)
library(ggrepel)
library(viridis)
library(gridExtra)

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
X = statis_df_aphasics
Z = demographics_df_aphasics[,instrumental_variables]
cc = complete.cases(Z)
X = as.matrix(statis_df_aphasics[cc,])
Z = as.matrix(demographics_df_aphasics[cc,instrumental_variables])
Zadd = model.matrix(~ 0 + Z + task, demographics_df_aphasics[cc,])
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
pca_iv_z_add = pca_iv(X, Zadd, k = 2)
sample_plotting_df = data.frame(pca_iv_z_add$Xproj, demographics_df_aphasics[cc,])

## Compute correlations within each combination of task and severity
correlation_df_severity = sample_plotting_df %>%
    subset(!is.na(severity)) %>%
    group_by_at(c("severity", "task")) %>%
    summarise(axis_correlation = cor(Axis1, Axis2))
correlation_df_group = sample_plotting_df %>%
    subset(!is.na(severity)) %>%
    group_by_at(c("Group", "task")) %>%
    summarise(axis_correlation = cor(Axis1, Axis2))

dot_group = ggplot(correlation_df_group) +
    geom_point(aes(x = task, color = Group, shape = Group, y = axis_correlation)) +
    ylab("Correlation between\nBCA axes in subgroup") +
    scale_color_discrete("") + scale_shape("") +
    ylim(c(-.6, .6)) + xlab("") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
dot_severity = ggplot(correlation_df_severity) +
    geom_point(aes(x = task, color = severity, shape = severity, y = axis_correlation)) +
    ylab("Correlation between\nBCA axes in subgroup") +
    scale_color_viridis_d("") + scale_shape("") +
    ylim(c(-.6, .6)) + xlab("") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

## center all of the classes and plot ellipses
centered_df_severity = sample_plotting_df %>%
    subset(!is.na(severity)) %>%
    group_by_at(c("severity", "task")) %>%
    mutate(Axis1_centered = Axis1 - mean(Axis1), Axis2_centered = Axis2 - mean(Axis2))
centered_df_group = sample_plotting_df %>%
    subset(!is.na(Group)) %>%
    group_by_at(c("Group", "task")) %>%
    mutate(Axis1_centered = Axis1 - mean(Axis1), Axis2_centered = Axis2 - mean(Axis2))


ellipse_group = ggplot(centered_df_group)+
    stat_ellipse(aes(x = Axis1_centered, y = Axis2_centered, color = Group)) +
    facet_wrap(~ task_for_plotting) + xlab("PCAIV1") + ylab("PCAIV2")
ellipse_severity = ggplot(centered_df_severity)+
    stat_ellipse(aes(x = Axis1_centered, y = Axis2_centered, color = severity)) +
    facet_wrap(~ task_for_plotting) + xlab("PCAIV1") + ylab("PCAIV2")

if(OLD_VERSION) {
    pdf("pca-iv-cors-aphasia.pdf", width=9, height = 5)
    grid.arrange(ellipse_group + theme(legend.position = "none"),
                 ellipse_severity + theme(legend.position = "none"),
                 dot_group + theme(legend.position = "left"),
                 dot_severity,
                 heights = c(1, .7))
    dev.off()
} else {
    pdf("pca-iv-cors-aphasia-new.pdf", width=9, height = 5)
    grid.arrange(ellipse_group + theme(legend.position = "none"),
                 ellipse_severity + theme(legend.position = "none"),
                 dot_group + theme(legend.position = "left"),
                 dot_severity,
                 heights = c(1, .7))
    dev.off()

}

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
demographics_df_all$severity = make_aq_bins(demographics_df_all$Aphasia.AQ)
levels(demographics_df_all$severity) = c(levels(demographics_df_all$severity), "Control")
demographics_df_all$severity[demographics_df_all$Group == "Control"] = "Control"
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

score_variable_pca = dudi.pca(Z, center = TRUE, scannf = FALSE, nf = 3)
ggplot(data.frame(score_variable_pca$li, demographics_df_aphasics[cc,])) +
    geom_point(aes(x = Axis2, y = Axis3, color = Group)) +
    scale_color_viridis_d() +
    coord_fixed()
## If you do a kmeans on the scores you get something that corresponds
## pretty well with the groups as defined, but not exactly
clustering = kmeans(scale(Z), centers = 5)
mclust::adjustedRandIndex(clustering$cluster, demographics_df_aphasics[cc,]$Group)
table(clustering$cluster, demographics_df_aphasics[cc,]$Group)

## clustering based on the language variables doesn't give us back the
## aphasia types at all
task_subset = which(demographics_df_aphasics[cc,]$task == "cinderella")
true_groups = demographics_df_aphasics[cc,]$Group[task_subset]
clustering_language = kmeans(scale(X[task_subset,]), centers = 5)
mclust::adjustedRandIndex(clustering_language$cluster, true_groups)
table(clustering_language$cluster, true_groups)

source("multitable-preprocessing.R")
source("extras.R")
library(ade4)
library(ggplot2)
library(ggrepel)
library(viridis)
library(gridExtra)
statis_df_all = statis_and_demographic$statis
demographics_df_all = statis_and_demographic$demographic
demographics_df_all$severity = make_aq_bins(demographics_df_all$Aphasia.AQ)
levels(demographics_df_all$severity)= c(levels(demographics_df_all$severity), "Control")
demographics_df_all$severity[demographics_df_all$Group == "Control"] = "Control"
aphasics = which(demographics_df_all$type == "aphasia")
statis_df_aphasics = statis_df_all[aphasics,]
demographics_df_aphasics = demographics_df_all[aphasics,]

task_and_subgroup = factor(paste(demographics_df_all$task, demographics_df_all$Group, sep = "_"))
task_and_subgroup[is.na(demographics_df_all$Group)] = NA
## there are too few transsensory here, so we remove them
small_group_indices = which(demographics_df_all$Group %in% excluded_aphasias)
between_all_subgroup = bca(dudi.pca(statis_df_all[-small_group_indices,],
                                         scannf = FALSE, nf = 2, scale = FALSE),
                                factor(task_and_subgroup[-small_group_indices]),
                           scannf = FALSE, nf = 2)
## flip first two axes so they aligns with the controls
between_all_subgroup$li[,1:2] = between_all_subgroup$li[,1:2] * -1
between_all_subgroup$l1[,1:2] = between_all_subgroup$l1[,1:2] * -1
between_all_subgroup$co[,1:2] = between_all_subgroup$co[,1:2] * -1
between_all_subgroup$c1[,1:2] = between_all_subgroup$c1[,1:2] * -1

task_and_subgroup_for_plotting = data.frame(t(sapply(rownames(between_all_subgroup$li), function(x)
    strsplit(x, "_")[[1]])))
colnames(task_and_subgroup_for_plotting) = c("task", "Group")
task_and_subgroup_for_plotting$task = factor(task_and_subgroup_for_plotting$task, levels = levels(demographics_df_all$task), ordered = TRUE)
samples_on_bca_axes = as.matrix(scale(statis_df_all[-small_group_indices,], scale = FALSE)) %*% as.matrix(between_all_subgroup$c1)
sample_plotting_df = data.frame(samples_on_bca_axes, demographics_df_all[-small_group_indices,])
sample_plotting_df = subset(sample_plotting_df, !(Group %in% excluded_aphasias))
sample_plotting_df$task = task_map[sample_plotting_df$task]

## Compute correlations within each combination of task and severity
correlation_df_severity = sample_plotting_df %>%
    subset(!is.na(severity)) %>%
    group_by_at(c("severity", "task")) %>%
    summarise(axis_correlation = cor(CS1, CS2))
correlation_df_group = sample_plotting_df %>%
    subset(!is.na(severity)) %>%
    group_by_at(c("Group", "task")) %>%
    summarise(axis_correlation = cor(CS1, CS2))

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
    mutate(CS1_centered = CS1 - mean(CS1), CS2_centered = CS2 - mean(CS2))
centered_df_group = sample_plotting_df %>%
    subset(!is.na(Group)) %>%
    group_by_at(c("Group", "task")) %>%
    mutate(CS1_centered = CS1 - mean(CS1), CS2_centered = CS2 - mean(CS2))


ellipse_group = ggplot(centered_df_group)+
    stat_ellipse(aes(x = CS1_centered, y = CS2_centered, color = Group)) +
    facet_wrap(~ task) + xlab("BCA1") + ylab("BCA2")
ellipse_severity = ggplot(centered_df_severity)+
    stat_ellipse(aes(x = CS1_centered, y = CS2_centered, color = severity)) +
    facet_wrap(~ task) + xlab("BCA1") + ylab("BCA2")

pdf("bca-cors-aphasia-and-control.pdf", width=9, height = 5)
grid.arrange(ellipse_group + theme(legend.position = "none"),
             ellipse_severity + theme(legend.position = "none"),
             dot_group + theme(legend.position = "left"),
             dot_severity,
             heights = c(1, .7))
dev.off()

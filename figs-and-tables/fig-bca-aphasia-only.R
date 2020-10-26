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
aphasics = which(demographics_df_all$type == "aphasia")
statis_df_aphasics = statis_df_all[aphasics,]
demographics_df_aphasics = demographics_df_all[aphasics,]
task_and_subgroup = factor(paste(demographics_df_aphasics$task, demographics_df_aphasics$Group, sep = "_"))
## there are too few transsensory here, so we remove them
small_group_indices = which(demographics_df_aphasics$Group %in% excluded_aphasias)
between_aphasics_subgroup = bca(dudi.pca(statis_df_aphasics[-small_group_indices,],
                                         scannf = FALSE, nf = 2, scale = FALSE),
                                factor(task_and_subgroup[-small_group_indices]),
                           scannf = FALSE, nf = 2)
## flip first two axes so they aligns with the controls
between_aphasics_subgroup$li[,1:2] = between_aphasics_subgroup$li[,1:2] * -1
between_aphasics_subgroup$l1[,1:2] = between_aphasics_subgroup$l1[,1:2] * -1
between_aphasics_subgroup$co[,1:2] = between_aphasics_subgroup$co[,1:2] * -1
between_aphasics_subgroup$c1[,1:2] = between_aphasics_subgroup$c1[,1:2] * -1

task_and_subgroup_for_plotting = data.frame(t(sapply(rownames(between_aphasics_subgroup$li), function(x)
    strsplit(x, "_")[[1]])))
colnames(task_and_subgroup_for_plotting) = c("task", "Group")
task_and_subgroup_for_plotting$task = factor(task_and_subgroup_for_plotting$task,
                                             levels = levels(demographics_df_aphasics$task),
                                             ordered = TRUE)
mean_plotting_df = data.frame(between_aphasics_subgroup$li, task_and_subgroup_for_plotting)
samples_on_bca_axes = as.matrix(scale(statis_df_all[-small_group_indices,], scale = FALSE)) %*% as.matrix(between_aphasics_subgroup$c1)
sample_plotting_df = data.frame(samples_on_bca_axes, demographics_df_all[-small_group_indices,])
sample_plotting_df = subset(sample_plotting_df, !(Group %in% excluded_aphasias))
mean_plotting_df = subset(mean_plotting_df, !(Group %in% excluded_aphasias))
variable_plotting_df = data.frame(between_aphasics_subgroup$co, variable = rownames(between_aphasics_subgroup$co))

mean_plotting_df_severity = data.frame(samples_on_bca_axes, demographics_df_all[-small_group_indices,]) %>%
    group_by(severity, task) %>%
    summarise(CS1.mean = mean(CS1), CS2.mean = mean(CS2))

variable_plot = ggplot(variable_plotting_df, aes(x = Comp1, y = Comp2, label = variable_name_map[variable])) +
    geom_segment(aes(xend = 0, yend = 0)) + geom_label(lineheight=.9,size=2.5,label.padding=unit(.1,"lines")) +
    coord_fixed() + xlab("Component 1") + ylab("Component 2") +
    scale_x_continuous(limits = c(-.2, .5))
sample_plot = ggplot(sample_plotting_df, aes(x = CS1, y = CS2, color = task)) +
    facet_wrap(~ Group) + stat_ellipse() + geom_point(size = .5, alpha = .5) +
    geom_point(aes(x = Axis1, y = Axis2, fill = task), data = mean_plotting_df, size = 3, shape = 23, color = "black") +
    coord_fixed() + make_axis_labels(between_aphasics_subgroup$eig) +
    scale_color_viridis_d("", labels = task_map) + scale_fill_viridis_d() + guides(color=guide_legend(""), fill = FALSE)
sample_plot_2 = ggplot(subset(sample_plotting_df, !is.na(severity)),
                       aes(x = CS1, y = CS2, color = task)) +
    facet_wrap(~ severity) + stat_ellipse() + geom_point(size = .5, alpha = .5) + 
    coord_fixed()+
    scale_color_viridis_d("", labels = task_map) + scale_fill_viridis_d("", labels = task_map) +
    make_axis_labels(between_aphasics_subgroup$eig) +
    geom_point(aes(x = CS1.mean, y = CS2.mean, fill = task),
               data = subset(mean_plotting_df_severity, !is.na(severity)), size = 3, shape = 23, color = "black")

pdf("bca-aphasia-only.pdf", width = 10, height = 8)
grid.arrange(sample_plot,
             variable_plot,
             sample_plot_2,
             ncol = 2, widths = c(1, .6), heights = c(1, 1))
dev.off()

source("multitable-preprocessing.R")
source("extras.R")
library(ade4)
library(ggplot2)
library(ggrepel)
library(viridis)
library(gridExtra)
statis_df_all = statis_and_demographic$statis
demographics_df_all = statis_and_demographic$demographic
controls = which(demographics_df_all$type == "control")
statis_df_controls = statis_df_all[controls,]
demographics_df_controls = demographics_df_all[controls,]

between_controls = bca(dudi.pca(statis_df_controls, scannf = FALSE, nf = 2, scale = FALSE),
                       factor(demographics_df_controls$task_and_type),
                       scannf = FALSE, nf = 2)
bca_proj_controls = as.matrix(scale(statis_df_controls, scale = FALSE)) %*%
    as.matrix(between_controls$c1)
bca_sample_plot = ggplot(data.frame(between_controls$li,
                  task = gsub("_.*", "", rownames(between_controls$li)), stringsAsFactors = FALSE),
       aes(x = Axis1, y = Axis2)) +
    geom_point(aes(x = CS1, y = CS2, color = task),
               data = data.frame(bca_proj_controls, demographics_df_controls)) +
    geom_label(aes(label = task_map[task]),lineheight=.9, size=3) + 
    coord_fixed() + make_axis_labels(between_controls$eig) +
    scale_color_viridis_d("Task", labels = task_map) + ylim(c(-2.5, 4.75)) + xlim(c(-3, 3.25))
bca_variable_plot = ggplot(data.frame(between_controls$co, variable = rownames(between_controls$co)),
       aes(x = Comp1, y = Comp2, label = variable_name_map[variable])) +
    geom_segment(aes(xend = 0, yend = 0)) +
    geom_label_repel(min.segment.length=.1,segment.size = .2,
                     segment.color="darkgray",
                     lineheight=.9,size=2.5,label.padding=unit(.1,"lines")) +
    coord_fixed() + xlab("Component 1") + ylab("Component 2")

no_axis_labels = theme(axis.text = element_blank(), axis.ticks=element_blank(), axis.title= element_blank())
if(OLD_VERSION) {
pdf("task-bca-controls.pdf", width = 8, height = 8)
bca_sample_plot + annotation_custom(ggplotGrob(bca_variable_plot + no_axis_labels), xmin = -2, xmax = 3, 
                                    ymin = 2.1, ymax = 4.85)
dev.off()
} else {
pdf("task-bca-controls-new.pdf", width = 8, height = 8)
bca_sample_plot + annotation_custom(ggplotGrob(bca_variable_plot + no_axis_labels), xmin = -2, xmax = 3, ymin = 3.1, ymax = 5.85) + ylim(c(-4, 5.5)) + xlim(c(-4.5, 4.5))
dev.off()
}

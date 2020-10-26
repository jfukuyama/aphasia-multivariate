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
aphasics = which(demographics_df_all$type == "aphasia")
statis_df_controls = statis_df_all[controls,]
demographics_df_controls = demographics_df_all[controls,]
statis_df_aphasics = statis_df_all[aphasics,]
demographics_df_aphasics = demographics_df_all[aphasics,]


statis_df_untransformed = exp(statis_and_demographic$statis_no_regressors) - .01
to_summarize = cbind(statis_df_untransformed,task = demographics_df_all$task, type = demographics_df_all$type)
split_by_task = split(to_summarize, to_summarize$task)

pvals = sapply(split_by_task, function(x) {
    to_remove = which(colnames(x) %in% c("task", "type"))
    if(length(unique(x$type)) == 1) {
        return(rep(NA, ncol(x) - 2))
    }
    apply(x[,-to_remove], 2, function(col) t.test(col ~ x$type)$p.value)
})
n_tests = sum(!is.na(pvals))
## bonferroni corrected
apply(pvals, 1:2, function(x) round(min(x * n_tests, 1), digits = 3))[,-2]
## uncorrected
apply(pvals, 1:2, round, digits = 3)[,-2]

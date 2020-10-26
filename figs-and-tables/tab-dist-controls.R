source("multitable-preprocessing.R")
source("extras.R")
source("boot-setup.R")
statis_df_all = statis_and_demographic$statis
demographics_df_all = statis_and_demographic$demographic
controls = which(demographics_df_all$type == "control")
aphasics = which(demographics_df_all$type == "aphasia")
statis_df_controls = statis_df_all[controls,]
demographics_df_controls = demographics_df_all[controls,]
statis_df_aphasics = statis_df_all[aphasics,]
demographics_df_aphasics = demographics_df_all[aphasics,]

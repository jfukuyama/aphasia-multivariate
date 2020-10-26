source("multitable-preprocessing.R")
source("extras.R")
statis_df_all = statis_and_demographic$statis
demographics_df_all = statis_and_demographic$demographic
excluded_indices = which(demographics_df_all$Group %in% excluded_aphasias)
statis_df_all = statis_df_all[-excluded_indices,]
demographics_df_all = demographics_df_all[-excluded_indices,]

## Supplemental table with demographics
all_participant_data = demographics_df_all
## WAB AQ, education, months post-stroke, type of aphasia distribution
## still TODO: type of aphasia
aphasia_demo_1 = all_participant_data %>%
    subset(type == "aphasia") %>%
    mutate(FREQ.raw = exp(FREQ.tokens) - 1) %>%
    group_by(task, type) %>%
    summarise_at(vars(c(Aphasia.AQ, Age, Years.of.Education, Aphasia.Duration, MLU.Words, FREQ.raw, Total.Utts)), get_mean_and_sd_as_string, digits = 1)

aphasia_demo_2 = all_participant_data %>%
    subset(type == "aphasia") %>%
    group_by(task, type) %>%
    do(cbind(get_table_as_string(.$Group, drop_extra = FALSE),
             get_table_as_string(.$Sex, drop_extra = TRUE),
             get_table_as_string(.$Apraxia.of.Speech, drop_extra = FALSE, name_prefix = "Apraxia"),
             get_table_as_string(.$Dysarthria, name_prefix = "Dysarthria"),
             get_table_as_string(.$Aphasia.Etiology, name_prefix = "Stroke"),
             n = length(unique(.$subject)))) %>%
    data.frame


aq_ed_duration_type = t(merge(aphasia_demo_1, aphasia_demo_2, by = c("task", "type")))
aq_ed_duration_type = cbind("var" = rownames(aq_ed_duration_type), aq_ed_duration_type)
aq_ed_duration_type = aq_ed_duration_type[-2,]

aq_ed_duration_type[-1,] %>%
    tex_formatted_table(colnames = get_task(aq_ed_duration_type["task",]), alignment = paste(rep("l", ncol(aq_ed_duration_type)), collapse = " ")) %>%
    cat



aphasia_demo_2 = all_participant_data %>%
    subset(type == "aphasia") %>%
    group_by(task, type) %>%
    do(cbind(get_table_as_string(.$Group, drop_extra = TRUE),
             get_table_as_string(.$Sex, drop_extra = TRUE),
             get_table_as_string(.$Apraxia.of.Speech, drop_extra = FALSE, name_prefix = "Apraxia"),
             get_table_as_string(.$Dysarthria, name_prefix = "Dysarthria"),
             get_table_as_string(.$Aphasia.Etiology, name_prefix = "Stroke"),
             n = length(unique(.$subject)))) %>%
    data.frame


## demographics for controls
control_demo_1 = all_participant_data %>%
    subset(type == "control") %>%
    mutate(FREQ.raw = exp(FREQ.tokens) - 1) %>%
    group_by(task, type) %>%
    summarise_at(vars(Years.of.Education, MMSE.raw.unadjusted.score, Age.at.Testing, MLU.Words, FREQ.raw, Total.Utts), get_mean_and_sd_as_string, digits = 1)

control_demo_2 = all_participant_data %>%
    subset(type == "control") %>%
    group_by(task, type) %>%
    do(cbind(get_table_as_string(.$Sex, drop_extra = TRUE),
             n = length(unique(.$subject))))
ed_mmse_age_sex = t(merge(control_demo_1, control_demo_2, by = c("task", "type")))
ed_mmse_age_sex = cbind("var" = rownames(ed_mmse_age_sex), ed_mmse_age_sex)
ed_mmse_age_sex = ed_mmse_age_sex[-2,]
ed_mmse_age_sex[-1,] %>%
    tex_formatted_table(colnames = get_task(ed_mmse_age_sex["task",]), alignment = paste(rep("l", ncol(ed_mmse_age_sex)), collapse = " ")) %>%
    cat


## t tests for differences in demographics
all_participant_data$SexFemale = all_participant_data$Sex == "female"
VARS = c("Years.of.Education", "Age.at.Testing", "SexFemale", "FREQ.tokens")
pvals = all_participant_data %>%
    split(.$task) %>%
    sapply(function(x) {
        if(length(unique(x$type)) == 1) {
            return(NA)
        } else {
            sapply(VARS, function(v) t.test(x[[as.character(v)]] ~ x$type)$p.value)
        }
    }) %>% data.frame

## supplemental plot with demographics broken down by aphasia type and task
VARS = c("Aphasia.AQ", "Age", "Years.of.Education", "Aphasia.Duration", "MLU.Words", "FREQ.tokens", "Total.Utts", "Sex", "Apraxia.of.Speech", "Dysarthria", "Aphasia.Etiology")
cases = complete.cases(all_participant_data[,VARS])
mm = model.matrix(as.formula(paste(c("~ 0", VARS), collapse = " + ")), data = all_participant_data)
mm_melted = melt(data.frame(mm,
                       Group = all_participant_data[cases,]$Group,
                       task = all_participant_data[cases,]$task),
                 id.vars = c("Group", "task"))
mm_melted = subset(mm_melted, !(variable %in% c("Sexmale", "Sex")))

pdf("fig-aphasia-subgroup-demographics-by-task.pdf", width = 8, height = 4)
ggplot(mm_melted) + stat_summary(aes(x = Group, color = task, y = value), size = .1) +
    facet_wrap(~ variable, scales = "free_y") + xlab("") + ylab("") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()

library(tidyverse)
## getting the neuropsych test results

neuropsych_res = read.csv("../demographics_data/neuropsych_test_aphasia.csv", stringsAsFactors = FALSE, na.strings = "U") %>%
    select(c("ParticipantID",
             "SpontSp.Score.for.AQ",
             "AudVbl.Comp.Score.for.AQ",
             "Rep.Score.for.AQ",
             "Naming.Score.for.AQ"))
subject = tolower(neuropsych_res$ParticipantID)
subject = sub(pattern = "a ?$", replacement = "", x = subject)
neuropsych_res$subject = subject


## Here we're making a set of data frames to use with statis or within-pca
file_names = list.files("../data")
files = lapply(paste("../data", file_names, sep = "/"),
               function(x) read.csv(x, na.strings = c("N/A", "."), stringsAsFactors = FALSE))
participant_data_variables = c("Participant.ID",
                               "Years.of.Education",
                               "Apraxia.of.Speech",
                               "Dysarthria",
                               "Aphasia.Duration",
                               "Age.at.Testing",
                               "MMSE.raw.unadjusted.score",
                               "Aphasia.Etiology")
neuropsych_variables = c(
                               "SpontSp.Score.for.AQ",
                               "AudVbl.Comp.Score.for.AQ",
                               "Rep.Score.for.AQ",
                               "Naming.Score.for.AQ")
get_columns_with_na = function(df, colnames) {
    n = nrow(df)
    name_exists = colnames %in% names(df)
    df_list = lapply(colnames, function(cn) {
        if(cn %in% names(df)) {
            return(df[[cn]])
        } else {
            return(rep(NA, n))
        }
    })
    names(df_list) = colnames
    return(as.data.frame(df_list))
}
participant_data = lapply(list.files("../demographics_data", full.names = TRUE),
                          function(x) {
                              df_all_columns = read.csv(x, na.strings = c("", "N/A", ".", "U"), stringsAsFactors = FALSE)
                              df_out = get_columns_with_na(df_all_columns, participant_data_variables)
                              })
participant_data = Reduce(rbind, participant_data)

##
subject = tolower(participant_data$Participant.ID)
subject = sub(pattern = "a ?$", replacement = "", x = subject)
## more cleaning
subject[subject == "tucson 22"] = "tucson22"
subject[subject == "wokniak03"] = "wozniak03"
participant_data$subject = subject
participant_data = participant_data[complete.cases(participant_data[,c("Participant.ID", "Years.of.Education")]),]
rownames(participant_data) = participant_data$subject
participant_data = merge(participant_data, neuropsych_res, by = "subject", all.x = TRUE, all.y = FALSE)

## different categories of variables
vars_to_normalize = c("Plurals",
    "Nouns",
    "Verbs",
    "Aux",
    "X3S",
    "X1S.3S",
    "PAST",
    "PASTP",
    "PRESP",
    "prep.",
    "adj.",
    "adv.",
    "conj.",
    "det.",
    "pro.",
    "X.open.class",
    "X.closed.class"#,
#    "Word.Errors"
    )
total_tokens = "FREQ.tokens"
vars_unnormalized = c(
    ##"MLU.Words"
)
demographic_vars = c("Age", "FREQ.tokens", "Aphasia.AQ", "Group", "Sex", "File", "ParticipantName", "MLU.Words", "Utt.Errors", "Total.Utts")
vars_to_regress_out = c("Age", "FREQ.tokens", "Years.of.Education")
lapply(files, function(x)
    sum(colnames(x) %in% c(vars_to_normalize, total_tokens, vars_unnormalized, demographic_vars)))

#' Takes an age_string formatted as years;months. and converts ot to a number
#' No error checking.
convert_age <- function(age_string) {
    age_split = strsplit(age_string, split = ";", fixed = TRUE)[[1]]
    year_and_month = as.numeric(age_split)
    age = year_and_month[1] + year_and_month[2] / 12
    return(age)
}

convert_age_vec <- function(age_string_vec) {
    sapply(age_string_vec, convert_age)
}

## make the composition part of the data frame

## OLD_VERSION = TRUE means the data that we used for submission
## OLD_VERSION = FALSE means that we normalize some of the variables differently
if(OLD_VERSION) {
    statis_df = Reduce(rbind, lapply(files, function(file) {
        freq_df = file[,vars_to_normalize]
        composition_df = sweep(freq_df, 1, file[,total_tokens], FUN = '/')
        composition_df[is.na(composition_df)] = 0
        composition_df_transformed = log(.01 + composition_df)
        extra_vars_df = file[,vars_unnormalized, drop=FALSE]
        statis_df = cbind(composition_df_transformed, extra_vars_df)
    }))
} else {
    statis_df = Reduce(rbind, lapply(files, function(file) {
        freq_df = file[,vars_to_normalize]
        composition_df = sweep(freq_df, 1, file[,total_tokens], FUN = '/')
        composition_df$Plurals = composition_df$Plurals / composition_df$Nouns
        composition_df$X3S = composition_df$X3S / (composition_df$Verbs + composition_df$Aux)
        composition_df$X1S.3S = composition_df$X1S.3S / (composition_df$Verbs + composition_df$Aux)
        composition_df$PAST = composition_df$PAST / (composition_df$Verbs + composition_df$Aux)
        composition_df$PASTP = composition_df$PASTP / (composition_df$Verbs + composition_df$Aux)
        composition_df$PRESP = composition_df$PRESP / (composition_df$Verbs + composition_df$Aux)
        composition_df[is.na(composition_df)] = 0
        composition_df_transformed = log(.01 + composition_df)
        extra_vars_df = file[,vars_unnormalized, drop=FALSE]
        statis_df = cbind(composition_df_transformed, extra_vars_df)
    }))
    
}
## make the demographics part of the data frame
demographics_df = Reduce(rbind, lapply(files, function(file) {
    demographics_as_list = lapply(demographic_vars, function(v) {
        if(is.null(file[[v]])) {
            return(NA)
        }
        return(file[[v]])
    })
    names(demographics_as_list) = demographic_vars
    return(data.frame(demographics_as_list))
}))
demographics_df$Age = convert_age_vec(as.character(demographics_df$Age))
demographics_df$FREQ.tokens = log(1 + demographics_df$FREQ.tokens)
demographics_df$subject = ifelse(is.na(demographics_df$File),
                                 as.character(demographics_df$ParticipantName),
                                 as.character(demographics_df$File))
## extract the task information from the file name
task_from_file_name = t(sapply(file_names, function(s) {
    s = sub(pattern = "\\.csv", replacement = "", s)
    string_split = strsplit(s, split = "_")[[1]]
    return(c(task = string_split[1], type = string_split[2], task_and_type = s))
}))

n_rows_per_file = sapply(files, nrow)
task_group_df = task_from_file_name[rep(1:nrow(task_from_file_name), n_rows_per_file),]
cc = complete.cases(statis_df)
statis_df = statis_df[cc,]
demographics_df = demographics_df[cc,]
task_group_df = task_group_df[cc,]
demographics_and_task = cbind(demographics_df, task_group_df)
demographics_and_task$Group = fct_collapse(demographics_and_task$Group, Control = c("Control", "control"))
demographics_and_task$Group = fct_recode(demographics_and_task$Group, NULL = "")
demographics_and_task$task = factor(demographics_and_task$task, levels = c("importantevent", "stroke", "cinderella", "cat", "window", "sandwich"), ordered = TRUE)
subject = tolower(demographics_and_task$subject)
demographics_and_task$subject = sub(pattern = "a?.(gem.cex|cha)$", replacement = "", x = subject)

unique(demographics_and_task$subject[!(demographics_and_task$subject %in% participant_data$subject)])
unique(participant_data$subject[!(participant_data$subject %in% demographics_and_task$subject)])
rownames(participant_data) = participant_data$subject
demographics_and_task = cbind(demographics_and_task, participant_data[demographics_and_task$subject, c(participant_data_variables, neuropsych_variables)])



## filter out the samples with 20 or fewer tokens
fewer_than_twenty = which(demographics_and_task$FREQ.tokens < log(20))
statis_df = statis_df[-fewer_than_twenty,]
demographics_and_task = demographics_and_task[-fewer_than_twenty,]


## project orthogonally to the things we want to regress out
Y = as.matrix(cbind(intercept = rep(1, nrow(demographics_and_task)), demographics_and_task[,vars_to_regress_out, drop=FALSE]))
## for the purposes of regressing out, set na's to be the mean of that column
Y_imputed = apply(Y, 2, function(x) {
    m = mean(x, na.rm = TRUE)
    x[is.na(x)] = m
    return(x)
})
statis_hat = statis_df - Y_imputed %*% solve(t(Y_imputed) %*% Y_imputed) %*% t(Y_imputed) %*% as.matrix(statis_df)


statis_and_demographic = list(statis = statis_hat, demographics = demographics_and_task, statis_no_regressors = statis_df)

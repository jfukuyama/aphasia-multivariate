n_boot = 1000
boot_quantiles = c(.025, .975)
resample_indices = function(subject_vec) {
    unique_subjects = unique(subject_vec)
    resampled_subjects = sample(unique_subjects, replace = TRUE)
    resampled_indices_list = lapply(resampled_subjects, function(subj) which(subject_vec == subj))
    resampled_indices = unlist(resampled_indices_list)
}
get_variance_explained_by_task = function(response, predictors, indices) {
    manova_out = manova(response[indices,] ~ 0 + task, data = predictors[indices,])
    sum(manova_out$fitted.values^2) / sum(response^2)
}
variance_explained_boot = replicate(n = n_boot, {
    get_variance_explained_by_task(response_controls,
                                   demographics_df_controls,
                                   resample_indices(demographics_df_controls$subject))
    })

tex_distance_table = function(mean_dist, lower_quantile, upper_quantile, labels, digits = 2) {
    dist_summary_array = array(dim = c(3, dim(mean_dist)))
    dist_summary_array[1,,] = mean_dist
    dist_summary_array[2,,] = lower_quantile
    dist_summary_array[3,,] = upper_quantile
    elements = apply(dist_summary_array, 2:3, function(x)
        sprintf("%.2f (%.2f, %.2f)", x[1], x[2], x[3]))
    elements[mean_dist == 0] = ""
    elements[upper.tri(elements)] = ""
    alignment = paste(c("l | ", rep("l ", length(labels))), collapse = "")
    header = paste("\\begin{tabular}{", alignment, "}", sep = "")
    topline = paste("& ", paste(labels[-length(labels)], collapse = "& "), "\\\\\\hline",
                    sep = "")
    elements_and_row_labels = cbind(labels, elements)
    rows = apply(elements_and_row_labels, 1, function(x)
        paste(paste(x, collapse = "& "), "\\\\", sep = ""))
    footer = "\\end{tabular}"
    out = paste(c(header, topline, rows[-1], footer), collapse = "\n")
}

tex_formatted_table = function(tab, colnames, alignment) {
    header = paste("\\begin{tabular}{", alignment, "}", sep = "")
    topline = paste(paste(colnames, collapse = "&"), "\\\\\\hline", sepcollapse = "")
    rows = apply(tab, 1, function(x) paste(x, collapse = "&"))
    all_rows = paste(rows, collapse = "\\\\\n")
    footer = "\\end{tabular}"
    out = paste(header, topline, all_rows, footer, collapse = "\n")
    return(out)
}

get_mean_and_sd_as_string = function(x, digits = 2) {
    m = as.character(round(mean(x, na.rm = TRUE), digits = digits))
    s = as.character(round(sd(x, na.rm = TRUE), digits = digits))
    sprintf("%s (%s)", m, s)
}

get_table_as_string = function(x, digits = 2, drop_extra = FALSE, name_prefix = NULL) {
    x = factor(x)
    category_fractions = round(table(x) / length(x), digits = digits)
    if(drop_extra) {
        category_fractions = category_fractions[-length(category_fractions)]
    }
    out = as.data.frame(lapply(category_fractions, function(x) x))
    if(!is.null(name_prefix)) {
        names(out) = paste(name_prefix, names(out), sep = " ")
        
    }
    return(out)
}

variable_name_map = c(
    "Plurals" = "Plurals",
    "Nouns" = "Nouns",
    "Verbs" = "Verbs",
    "Aux" = "Auxiliary",
    "X3S" = "Third-person\nsingular",
    "X1S.3S" = "1st/3rd person\nidentical forms",
    "PAST" = "Past\ntense",
    "PASTP" = "Past\nparticiple",
    "PRESP" = "Present\nparticiple",
    "prep." = "Prepositions",
    "adj." = "Adjectives",
    "adv." = "Adverbs",
    "conj." = "Conjunctions",
    "det." = "Determiners",
    "pro." = "Pronouns",
    "X.open.class" = "Open-class\nwords",
    "X.closed.class" = "Closed-class\nwords"
)

task_map = c(
    "cat" = "Cat",
    "cinderella" = "Cinderella",
    "importantevent" = "Important\nEvent",
    "sandwich" = "Sandwich",
    "stroke" = "Stroke",
    "window" = "Window"   
)

task_map_ordered = c(
    "sandwich" = "Sandwich",
    "cat" = "Cat",
    "window" = "Window",   
    "cinderella" = "Cinderella",
    "importantevent" = "Important\nEvent",
    "stroke" = "Stroke"
)

task_map_ordered_2 = c(
    "importantevent" = "Important\nEvent",
    "stroke" = "Stroke",
    "cinderella" = "Cinderella",
    "cat" = "Cat",
    "window" = "Window",   
    "sandwich" = "Sandwich"
)


task_map_2 = function(value) {
    map = c(
    "cat" = "Cat",
    "cinderella" = "Cinderella",
    "importantevent" = "Important Event",
    "sandwich" = "Sandwich",
    "stroke" = "Stroke",
    "window" = "Window")
    lapply(value, function(v) map[as.character(v)])
}

iv_map = c(
    "ZAudVbl.Comp.Score.for.AQ" = "Aud/Vbl",
    "ZNaming.Score.for.AQ" = "Naming",
    "ZRep.Score.for.AQ" = "Rep",
    "ZSpontSp.Score.for.AQ" = "Spont",
    "AQ" = "AQ"
)

group_map = c(
    "Broca" = "Broca",
    "Wernicke" = "Wernicke",
    "Conduction" = "Conduction",
    "Anomic" = "Anomic",
    "NotAphasicByWAB" = "Not aphasic\nby WAB",
    "Control" = "Control"
)

make_axis_labels = function(evals, axes = 1:2) {
    variance_explained = evals / sum(evals) * 100
    axis_labels = list(xlab(sprintf("Axis %i, %.2f%%", axes[1], variance_explained[axes[1]])),
        ylab(sprintf("Axis %i, %.2f%%", axes[2], variance_explained[axes[2]])))
}


excluded_aphasias = c("TransSensory", "aphasia", "Global", NA, "TransMotor")
get_task = function(x) {
    out = task_map[x]
    ifelse(is.na(out), "", out)
}

#' Takes a vector of aphasia AQs, returns an ordered factor with bins corresponding to severity
make_aq_bins = function(aq) {
    binned_aq = cut(aq, breaks = c(0, 50, 75, 93.75, 100))
    binned_aq = factor(binned_aq, ordered = TRUE)
    levels(binned_aq) = c("Severe", "Moderate", "Mild", "Not aphasic\nby WAB")
    return(binned_aq)
}

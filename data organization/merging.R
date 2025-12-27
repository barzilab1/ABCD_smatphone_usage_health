# library(dplyr)
# library(lubridate)
# library(janitor)
# source("data organization/data_utility_fun.R")



merge_all_data <- function(..., tp_include = NULL){

  datasets = list(...)

  combined_datasets <- Reduce(\(x, y) { if(nrow(y) != 0) merge(x, y, all = T) else x }, datasets)

  if(!is.null(tp_include)){
    combined_datasets <- combined_datasets %>% filter(grepl(paste0(tp_include, collapse = "|"), session_id)) %>% droplevels()
  }
  combined_datasets = remove_empty(combined_datasets, "cols")

  if("ph_y_anthr_age" %in% colnames(combined_datasets)) {
    combined_datasets = get_BMI_percentiles(combined_datasets)
  }

  # if("kbi_gender" %in% colnames(combined_datasets)) {
  #   combined_datasets = get_geneder_minority(combined_datasets)
  # }

  saveRDS(combined_datasets, file = "data/data_long.rds")
  return(combined_datasets)

}

# @param max_variables For these variables, the max value across all time points will be used, and not the mean.
get_wide_data = function(data, max_variables = NULL){

  sep = "____"
  max_variables = c(max_variables, "parents_high_edu_br")
  output_to_print = matrix(nrow = 0, ncol = 3, dimnames = list(c(), c("Type", "variable name", "Timepoints")))

  variables_not_for_ever = c("participant_id", "session_id", "visit_date_br",
                             grep("(age|ddt)$", colnames(data), value = T),
                             "family_id_br", "site_br",
                             "sex_br", "sex", "age_br",
                             grep("race_|text_br$", colnames(data), value = T), "ethnicity_hisp_br",
                             "born_in_usa")

  # get the columns to work with
  variables_for_ever = setdiff(colnames(data), variables_not_for_ever)

  # create timepoint feature for the wide dataset
  data$timepoint = sub(".*-", "", data$session_id)
  data[, c("visit_date_br", "session_id")] = NULL


  data_wide = reshape(as.data.frame(data), direction = "wide", idvar = "participant_id", timevar = "timepoint", sep = sep,
                      v.names = c(variables_for_ever, grep("age(_br)?$", colnames(data), value = T) ,"site_br", "parents_high_edu_text_br")) # time-varying variables
  data_wide = remove_empty(data_wide)

  for (var_name in variables_for_ever) {

    # get relevant columns from wide
    cols_wide = grep(paste0("^",var_name, sep), colnames(data_wide), value = T)

    # if there is only one column, no need to update it
    if(length(cols_wide) == 1) {
      output_to_print = rbind(output_to_print, c("one timepoint", var_name, sub(".*__", "", cols_wide)))
      next
    }

    ## create the summary variable
    # 1. suicide is being handled differently
    if(grepl("^S(A|I)_.*y$", var_name)){
      output_to_print = rbind(output_to_print, c("binary [suicide]", var_name, paste(sub(".*__", "", cols_wide), collapse = " , ")))
      new_col_name = paste0(var_name, "_ever")
      data_wide[,new_col_name] = apply(data_wide[,cols_wide], 1, \(r) any(r == 1)*1)

      # 2. check if max is required
    }else if(!is.null(max_variables) & (var_name %in% max_variables)){
      # max
      output_to_print = rbind(output_to_print, c("max", var_name, paste(sub(".*__", "", cols_wide), collapse = " , ")))
      new_col_name = paste0(var_name, "_max")
      data_wide[,new_col_name] = apply(data_wide[,cols_wide], 1, function(r){
        if(all(is.na(r))) {return(NA)}
        max(r, na.rm = T)
      })
    }else{
      # check range
      col_range_segment = range(data[,var_name, drop = T] |> as.character() |> as.numeric(), na.rm = T)
      col_range = col_range_segment[2]-col_range_segment[1]
      if(col_range == 1 ){
        # 3. binary
        # in case all NA --> NA
        # in case at least 1 is col_range_segment[2] --> 1
        # combination of 0 and NA --> 0
        output_to_print = rbind(output_to_print, c("binary", var_name, paste(sub(".*__", "", cols_wide), collapse = " , ")))
        new_col_name = paste0(var_name, "_ever")
        data_wide[,new_col_name] = apply(data_wide[,cols_wide], 1, function(r){
          if(all(is.na(r))) {return(NA)}
          any(r == col_range_segment[2], na.rm = T)*1 # in case the binary is not in the range 0-1, it will be converted to such
        })
      }else if(col_range != 0){
        # 4. continues
        output_to_print = rbind(output_to_print, c("continues", var_name, paste(sub(".*__", "", cols_wide), collapse = " , ")))
        new_col_name = paste0(var_name, "_mean")
        data_wide[, new_col_name] = rowMeans(sapply(data_wide[, cols_wide], \(x) as.numeric(as.character(x))), na.rm = TRUE)
      }else{
        # 5. no range [col_range == 0], will be dropped out of data_wide
        output_to_print = rbind(output_to_print, c("no range", var_name, paste(sub(".*__", "", cols_wide), collapse = " , ")))
      }
    }

    # remove the time-varying variables
    # data_wide[,cols_wide] = NULL
  }

  data_wide <- data_wide %>%
    mutate(parents_high_edu_text_br_max = case_when(
      parents_high_edu_br_max <= 12 ~ "highschool_below",
      parents_high_edu_br_max <= 13 ~ "highschool_graduate",
      parents_high_edu_br_max < 16 ~ "post_highschooler_education",
      parents_high_edu_br_max == 16 ~ "bachelor",
      parents_high_edu_br_max > 16 ~ "master_above"
    ))

  # print(as.data.frame(output_to_print))

  saveRDS(data_wide, file = "data/data_wide.rds")
  return(data_wide)

}

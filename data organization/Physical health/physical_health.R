# library(dplyr)
# library(arrow)
# source("configurations/path_config.R")
# source("data organization/data_utility_fun.R")


get_physical_health <- function(files_list){

  datasets_list = list()

  ############################################################
  # Anthropometrics [Youth]
  ############################################################
  if("ph_y_anthr" %in% files_list){

    anthropometrics <- read_parquet(file.path(abcd_main_data_path, "ph_y_anthr.parquet"))
    anthropometrics <- anthropometrics[ , grep("part|session|waist|(age|mean)$", names(anthropometrics))]
    #note: all weight and height cleaning will be done with sex after merging

    datasets_list[["ph_y_anthr"]] = anthropometrics
  }


  #####################################################################
  # Pubertal Development Scale & Menstrual Cycle Survey History [Youth]
  #####################################################################
  if("ph_y_pds" %in% files_list){
    puberty_y <- read_parquet(file.path(abcd_main_data_path, "ph_y_pds.parquet"))

    puberty_y <- puberty_y[ , grep("part|session|(age|categ|mean)$", names(puberty_y))]

    puberty_y$male_y_late_or_post_puberty <- ifelse(puberty_y$ph_y_pds__m_categ >= 4, 1,0) |> as.factor()
    puberty_y$female_y_late_or_post_puberty <- ifelse(puberty_y$ph_y_pds__f_categ >=4, 1,0) |> as.factor()
    puberty_y$late_or_post_puberty_both_sexes <- coalesce(puberty_y$male_y_late_or_post_puberty, puberty_y$female_y_late_or_post_puberty)
    puberty_y$puberty_both_sexes <- coalesce(puberty_y$ph_y_pds__f_categ, puberty_y$ph_y_pds__m_categ)


    # notes to be consider:
    # 1. if participants reported a maximum score on the PDS for two years in a row, then the PDS was discontinued
    # -- should we copy the puberty stage to all subsequent timepoints?
    # 2. Years 2-5 PDS collection was impacted by the pandemic, with the largest proportion of missingness in Year 3.

    datasets_list[["ph_y_pds"]] = puberty_y
  }


  ############################################################
  # Munich Chronotype Questionnaire [Youth]
  ############################################################
  if("ph_y_mctq" %in% files_list){

    MCQ <- read_parquet(file.path(abcd_main_data_path, "ph_y_mctq.parquet"))

    MCQ <- MCQ %>%
      select(participant_id, session_id, ph_y_mctq__sleep_dur)

    MCQ <- MCQ %>%
      mutate(
        ## Sleep Duration (hours):
        sleep_duration_hrs = ph_y_mctq__sleep_dur,
        sleep_duration_hrs = case_when(sleep_duration_hrs < 0 | sleep_duration_hrs >= 24 ~ NA_real_, TRUE ~ as.numeric(sleep_duration_hrs)),
        lack_sleep = case_when(sleep_duration_hrs >= 8 ~ 0, sleep_duration_hrs < 8 ~ 1, TRUE ~ NA_real_) # normal >= 8h
      )


    datasets_list[["ph_y_mctq"]] = MCQ
  }


  ############################################################
  ############################################################

  # check that all files were loaded
  if(length(files_list) != length(datasets_list)){
    missing_files = setdiff(files_list, names(datasets_list) )
    print(paste0("The following files weren't loaded: ", paste(missing_files, collapse = " ; ")))
  }

  # combine data
  combined_dataframe <- Reduce(\(x, y) merge(x, y, all = T), datasets_list)

  return(combined_dataframe)

}

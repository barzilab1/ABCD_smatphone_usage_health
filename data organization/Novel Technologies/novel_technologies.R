# library(dplyr)
# library(fastDummies)
# library(arrow)
# library(janitor)
# library(stringr)
# library(purrr)
# source("configurations/path_config.R")


get_novel_technologies <- function(files_list){

  datasets_list = list()

  ############################################################
  # Screen Time Questionnaire (Youth)
  ###########################################################
  if("nt_y_stq" %in% files_list){
    stq <- read_parquet(file.path(abcd_main_data_path, "nt_y_stq.parquet"))
    stq <- stq %>%
      select(-matches("(_dtt|_adm|_nm|_dk)$"))

    factor_cols <- setdiff(names(select(stq, where(is.factor))), "session_id")

    stq <- stq %>% mutate(across(all_of(factor_cols), ~ as.numeric(as.character(.x))))

    cols_with_special <- names(stq)[purrr::map_lgl(stq, function(x) any(x %in% c(444, 777, 999), na.rm = TRUE))] #nt_y_stq__socmed_005
    cols_with_special <- setdiff(cols_with_special, "nt_y_stq__socmed_005")

    stq <- stq %>%
      mutate(across(all_of(cols_with_special),
                    ~ replace(.x, .x %in% c(444, 777, 999), NA_real_)))

    stq <- stq %>%
      remove_empty() %>%
      droplevels()

    stq <- stq %>%
      dummy_cols(., select_columns = "nt_y_stq__sleep_002", remove_selected_columns = T, ignore_na = T) %>% #nt_y_stq__sleep_002, What do you usually do with your phone when you are ready to go to sleep?
      # rename new columns
      rename(
        # 444=Not Applicable ; 1=Turn the phone off ; 2=Put the ringer on silent or vibrate ;
        # 3=Leave the ringer on ; 4=Put it outside of the room where I sleep ; 777=Refuse to answer
        nt_y_stq__sleep_002_turn_phone_off = nt_y_stq__sleep_002_1,
        nt_y_stq__sleep_002_phone_silent_vibrate = nt_y_stq__sleep_002_2,
        nt_y_stq__sleep_002_phone_ring_on = nt_y_stq__sleep_002_3,
        nt_y_stq__sleep_002_phone_outside_room = nt_y_stq__sleep_002_4
      )

    # combining hours and minutes variables
    # convert min to hr
    stq_combine_hr_min <- stq %>%
      mutate(across(contains("_min"), ~.x/60))
    # get hr and min variable names
    pair_vars <- data.frame(hr_vars = stq_combine_hr_min %>% select(contains("_hr")) %>% names) %>%
      mutate(min_vars = str_replace_all(hr_vars, "_hr", "_min")) %>%
      mutate(pair_hr_min_vars = str_replace_all(hr_vars, "_hr", "_pair_hr_min"))

    new_col_name <- pair_vars$pair_hr_min_vars
    hr_vars <- pair_vars$hr_vars
    min_vars <- pair_vars$min_vars

    # NA + NA -> NA
    # hr/min + NA -> hr/min
    for (i in seq_along(new_col_name)) {
        stq_combine_hr_min[[new_col_name[i]]] <-
            ifelse(
                is.na(stq_combine_hr_min[[hr_vars[i]]]) & is.na(stq_combine_hr_min[[min_vars[i]]]),
                NA_real_,
                rowSums(stq_combine_hr_min[, c(hr_vars[i], min_vars[i])], na.rm = TRUE)
            )
    }

    stq <- stq %>% inner_join(stq_combine_hr_min %>% select(participant_id, session_id, all_of(new_col_name)),
                             by = c("participant_id", "session_id"))


    # creating weighted sum for weekday and weekend variables
    pair_vars_wk <- data.frame(wkdy_vars = stq %>% select(contains("_wkdy")) %>% names) %>%
      mutate(wknd_vars = str_replace_all(wkdy_vars, "_wkdy", "_wknd")) %>%
      mutate(pair_wkdy_wknd_vars = str_replace_all(wkdy_vars, "_wkdy", "_pair_wk"))

    # Weighted sum on pair_vars_wk = (wkdy*5 + wknd*2)

    # Loop to create weighted mean + weighted sum
    for (i in seq_len(nrow(pair_vars_wk))) {

      wkdy <- pair_vars_wk$wkdy_vars[i]
      wknd <- pair_vars_wk$wknd_vars[i]


      # New column name for SUM
      new_sum <- paste0(pair_vars_wk$pair_wkdy_wknd_vars[i], "_weightedsum")

      # Compute
      stq[[new_sum]] <-
          ifelse(
              is.na(stq[[wkdy]]) & is.na(stq[[wknd]]), NA_real_,
              5 * ifelse(is.na(stq[[wkdy]]), 0, stq[[wkdy]]) +
              2 * ifelse(is.na(stq[[wknd]]), 0, stq[[wknd]])
          )
    }

    datasets_list[["nt_y_st"]] = stq
  }

  ############################################################
  # Screen Time Questionnaire [Parent]
  ###########################################################
  if("nt_p_yst" %in% files_list){
    stq_p <- read_parquet(file.path(abcd_main_data_path, "nt_p_yst.parquet"))

    stq_p <- stq_p %>%
      select(matches("participant|session|nt_p_yst_001___[1-6]$|nt_p_yst_001__0[23]")) %>%
      mutate(across(starts_with("nt_"), ~ as.numeric(as.character(.x)))) %>%
      # Only smartphone
      filter((nt_p_yst_001___1 == 1 & nt_p_yst_001__02 %in% c(1, 2)) | nt_p_yst_001___1 == 0) %>%
      dplyr::rename(smartphone_ownership = "nt_p_yst_001___1")

    stq_p$nt_p_yst_001__02[stq_p$nt_p_yst_001__02 == 777 | stq_p$nt_p_yst_001__02 == 999] <- NA
    stq_p <- droplevels(stq_p)

    datasets_list[["nt_p_yst"]] = stq_p
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

  # write_csv(combined_dataframe, "data/physical_health.csv", na = "")
  # return("data/physical_health.csv")
  return(combined_dataframe)

}

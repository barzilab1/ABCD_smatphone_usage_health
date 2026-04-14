# LIBRARY----
# library(readr)
# library(purrr)
# library(tidyr)
# library(stringr)
# library(dplyr)
# library(targets)
# source("data organization/data_utility_fun.R")

get_mod_data <- function(merged_data) {

  # Covariates for Aim 1----
  covars_1 <- c("age_br", "sex_br")

  covars_2 <- c(
    covars_1,
    "race_black", "ethnicity_hisp_br",
    "household_income", "parents_high_edu_br",
    "puberty_both_sexes", "fc_y_pm_mean",
    "nt_p_yst_001___2", "nt_p_yst_001___3",
    "nt_p_yst_001___4", "nt_p_yst_001___6"
  )

  names_covars <- c("depression_dx_y", "bmi_obesity", "lack_sleep")

  add_aim1_main <- c(
    depression_dx_y = "depression_dx_y_2y",
    bmi_obesity     = "bmi_obesity_3y",
    lack_sleep      = "sleep_duration_hrs_3y",
    mh_y_bpm_tscore = "scale(mh_y_bpm_tscore_3y)",
    bmi = "scale(bmi_2y)"
  )

  covariates_list_aim1_main <- make_cov_list(covars_1, covars_2, add_aim1_main, c(names_covars, "mh_y_bpm_tscore", "bmi"))

  add_aim1_sens <- add_aim1_main
  add_aim1_sens["depression_dx_y"] <- "bpm_T_bin_concern_3y"

  covariates_list_aim1_sens <- make_cov_list(covars_1, covars_2, add_aim1_sens, names_covars)

  # Covariates for Aim 2----
  add_aim2 <- c(
    depression_dx_y = "depression_dx_y_2y",
    bmi_obesity     = "bmi_obesity_3y",
    lack_sleep      = "sleep_duration_hrs_3y"
  )

  covariates_list_aim2_main <- make_cov_list(covars_1, covars_2, add_aim2, names_covars)

  add_aim2_sen <- c(
      depression_dx_y = "depression_dx_y_2y",
      bmi_obesity     = "bmi_obesity_2y",
      lack_sleep      = "sleep_duration_hrs_2y"
  )

  covariates_list_aim2_2y <- make_cov_list(covars_1, covars_2, add_aim2_sen, names_covars)
  
  # Create data for 2 aims----
  # filter kids with phone that isn't smartphone
  merged_data <- merged_data %>% filter((smartphone_ownership == 1 & nt_p_yst_001__02 %in% c(1, 2)) | smartphone_ownership == 0)
  merged_data <- merged_data %>% mutate(across(c(all_of(unique(unlist(covars_2))), nt_y_stq__sleep_002_phone_outside_room, nt_y_stq__sleep_002_phone_ring_on), ~ as.numeric(as.character(.x))))
  merged_data <- merged_data %>%
    # Rename for convenience
    rename(
      watch_stream_TV_shows_movies_smartph_wsum = "nt_y_stq__screen__pair_wk__pair_hr_min_001__01_weightedsum",
      single_player_video_games_smartph_wsum = "nt_y_stq__screen__pair_wk__pair_hr_min_003__01_weightedsum",
      multiplayer_video_games_smartph_wsum = "nt_y_stq__screen__pair_wk__pair_hr_min_004__01_weightedsum",
      text_cellphone_others_smartph_wsum = "nt_y_stq__screen__pair_wk__pair_hr_min_005__01_weightedsum",
      visit_social_media_apps_smartph_wsum = "nt_y_stq__screen__pair_wk__pair_hr_min_006__01_weightedsum",
      video_chat_not_for_school_smartph_wsum = "nt_y_stq__screen__pair_wk__pair_hr_min_008__01_weightedsum",
      schoolyear_total_school_related_work_smartph_wsum = "nt_y_stq__screen__pair_wk__school__pair_hr_min_002_weightedsum",
      schoolyear_total_smartph_wsum = "nt_y_stq__screen__pair_wk__tot__pair_hr_min_002_weightedsum"
    )


  merged_data$non_social_communication = row_sum_na_safe(merged_data[, c("text_cellphone_others_smartph_wsum", "video_chat_not_for_school_smartph_wsum")])
  merged_data$video_games = row_sum_na_safe(merged_data[, c("single_player_video_games_smartph_wsum", "multiplayer_video_games_smartph_wsum")])
  merged_data$total_smartph_sum_wsum = row_sum_na_safe(merged_data[, c("non_social_communication", "video_games",
                                                                          "watch_stream_TV_shows_movies_smartph_wsum",
                                                                          "visit_social_media_apps_smartph_wsum",
                                                                          "schoolyear_total_school_related_work_smartph_wsum")])
  saveRDS(merged_data, "data/merged_data_ext.rds")
  # Create subset
  no_sm_3y_ids <- merged_data %>% filter(session_id == "ses-03A" & smartphone_ownership == 0) %>% pull(participant_id)

  aim1_df <- merged_data %>%
    filter((session_id == "ses-04A") & (participant_id %in% no_sm_3y_ids) & (!is.na(smartphone_ownership)))

  aim1_df_2_3y <- merged_data %>%
    filter(session_id == "ses-03A") %>%
    select(participant_id, age_br_3y = age_br, household_income_3y = household_income, 
           parents_high_edu_br_3y = parents_high_edu_br,
           parents_high_edu_text_br_3y = parents_high_edu_text_br, parents_high_edu_br_master_above_3y = parents_high_edu_br_master_above,
           puberty_both_sexes_3y = puberty_both_sexes, late_or_post_puberty_both_sexes_3y = late_or_post_puberty_both_sexes,
           fc_y_pm_mean_3y = fc_y_pm_mean,
           sleep_duration_hrs_3y = sleep_duration_hrs,
           bmi_obesity_3y = bmi_obesity, lack_sleep_3y = lack_sleep, bpm_T_bin_concern_3y = bpm_T_bin_concern,
           mh_y_bpm_tscore_3y = mh_y_bpm_tscore, bmi_3y = bmi) %>%
    left_join(
      merged_data %>%
        filter(session_id == "ses-02A") %>%
        select(participant_id, age_br_2y = age_br, household_income_2y = household_income, parents_high_edu_br_2y = parents_high_edu_br,
               puberty_both_sexes_2y = puberty_both_sexes, late_or_post_puberty_both_sexes_2y = late_or_post_puberty_both_sexes,
               fc_y_pm_mean_2y = fc_y_pm_mean,
               sleep_duration_hrs_2y = sleep_duration_hrs,
               bmi_obesity_2y = bmi_obesity, lack_sleep_2y = lack_sleep, depression_dx_y_2y = depression_dx_y, bpm_T_bin_concern_2y = bpm_T_bin_concern,
               mh_y_bpm_tscore_2y = mh_y_bpm_tscore, bmi_2y = bmi)
    )

  aim1_df <- aim1_df %>% left_join(aim1_df_2_3y) %>%
    mutate(across(matches("_2y$|(?<!text_br)_3y$|_y$", perl = T), ~ as.numeric(as.character(.x))))

  # Create data for Aim 2
  ## Among kids who have smartphone at 4 year
  aim2_df <- merged_data %>% filter(session_id == "ses-04A" & smartphone_ownership == 1) %>%
    mutate(across(matches("_y$"), ~ as.numeric(as.character(.x))))

  # Another way we want to stratify the data is on time spent on different activities - create categories of time spent:
  smartphone_vars <- c(
    "watch_stream_TV_shows_movies_smartph_wsum",
    "single_player_video_games_smartph_wsum",
    "multiplayer_video_games_smartph_wsum",
    "text_cellphone_others_smartph_wsum",
    "visit_social_media_apps_smartph_wsum",
    "video_chat_not_for_school_smartph_wsum",
    "schoolyear_total_school_related_work_smartph_wsum",
    "schoolyear_total_smartph_wsum",
    "non_social_communication",
    "video_games"
  )

  aim2_df <- aim2_df %>%
    mutate(across(all_of(smartphone_vars), ~ .x / 7, .names = "{.col}_wmean_perday"))

  cut_levels <- c("0–2h", "2–5h", ">5h")

  make_time_category <- function(x) {
    case_when(
      x <= 2 ~ "0–2h",
      x <= 5 ~ "2–5h",
      x > 5  ~ ">5h",
      TRUE ~ NA_character_
    )
  }

  aim2_df <- aim2_df %>%
    mutate(across(ends_with("_wmean_perday"), ~ make_time_category(.x), .names = "{.col}_cat")) %>%
    mutate(across(ends_with("_cat"), ~ factor(as.character(.x), levels = cut_levels))) %>%
    mutate(across(ends_with("_cat"),~ relevel(.x, ref = cut_levels[1])))


  aim2_df_no3y <- aim2_df %>%
    filter(participant_id %in% no_sm_3y_ids) %>%
    left_join(aim1_df_2_3y)

  # data for comparison between included and not included
  df_compare <- merged_data %>%
    filter(session_id == "ses-04A") %>%
    mutate(included = ifelse(participant_id %in% aim1_df$participant_id, 1, ifelse(!is.na(smartphone_ownership), 0, NA))) %>%
    left_join(aim1_df_2_3y)


  saveRDS(aim1_df, "data/aim1_df.rds")
  saveRDS(aim1_df %>% select(participant_id) %>% pull(), "data/cohort_ids.rds")
  saveRDS(aim2_df, "data/aim2_df.rds")
  saveRDS(aim2_df_no3y, "data/aim2_df_no3y.rds")
  saveRDS(df_compare, "data/df_compare.rds")
  saveRDS(covariates_list_aim1_main, "data/covariates_list_aim1.rds")
  saveRDS(covariates_list_aim1_sens, "data/covariates_list_aim1_sens.rds")
  saveRDS(covariates_list_aim2_main, "data/covariates_list_aim2.rds")
  saveRDS(covariates_list_aim2_2y, "data/covariates_list_aim2_2y.rds")

  return(list(aim1_df = aim1_df, aim2_df_no3y = aim2_df_no3y))
}


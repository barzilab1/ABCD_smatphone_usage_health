# LIBRARY----
# library(readr)
# library(purrr)
# library(tidyr)
# library(stringr)
# library(dplyr)
# library(targets)
# library(mice)
# library(miceRanger)
# source("data organization/data_utility_fun.R")

get_fully_imp_data <- function(merged_data) {
  
  # Covariates 
  covars <- c("age_br", "sex_br",
              "race_black", "ethnicity_hisp_br",
              "household_income", "parents_high_edu_br",
              "puberty_both_sexes", "fc_y_pm_mean",
              "nt_p_yst_001___2", "nt_p_yst_001___3",
              "nt_p_yst_001___4", "nt_p_yst_001___6")
  
  merged_data <- readRDS("data/merged_data_ext.rds")
  cohort_ids <- readRDS("data/cohort_ids.rds")
  
  no_sm_2y_ids <- merged_data %>% filter(session_id == "ses-02A" & smartphone_ownership == 0) %>% pull(participant_id)
  no_sm_3y_ids <- merged_data %>% filter(session_id == "ses-03A" & smartphone_ownership == 0) %>% pull(participant_id)
  
  IVs<- c(
    "smartphone_ownership",
    "schoolyear_total_smartph_wsum",
    "nt_y_stq__screen__wkdy__tot__pair_hr_min_002",
    "nt_y_stq__screen__wknd__tot__pair_hr_min_002",
    "nt_y_stq__sleep_002_phone_outside_room",
    "watch_stream_TV_shows_movies_smartph_wsum", #streaming
    "single_player_video_games_smartph_wsum", # video games
    "multiplayer_video_games_smartph_wsum", # video games
    "text_cellphone_others_smartph_wsum", # texting
    "visit_social_media_apps_smartph_wsum", # visiting social media sites 
    "video_chat_not_for_school_smartph_wsum", # video-chatting 
    "schoolyear_total_school_related_work_smartph_wsum" # school-related work
  )
  
  merged_data_dep <- merged_data %>% filter(participant_id %in% cohort_ids) %>% select(participant_id, session_id, all_of(covars), all_of(IVs), depression_dx_y) %>% filter(session_id != "ses-03A")
  merged_data_obesity <- merged_data %>% filter(participant_id %in% cohort_ids) %>% select(participant_id, session_id, all_of(covars), all_of(IVs), bmi_obesity) %>% filter(session_id != "ses-03A")
  merged_data_sleep <- merged_data %>% filter(participant_id %in% cohort_ids) %>% select(participant_id, session_id, all_of(covars), all_of(IVs), sleep_duration_hrs)
  # For Elina: if we need sensitivity analyses for new covariates, then do new imputation separately for BPM at all time points
  # merged_data_bpm <- merged_data %>% filter(participant_id %in% cohort_ids) %>% select(participant_id, session_id, all_of(covars), all_of(IVs), bpm_t_...)
  
  set.seed(032426)
  merged_data_imp_dep <- miceRanger(merged_data_dep, m = 5, maxiter = 10, verbose = FALSE)
  merged_data_imp_obesity <- miceRanger(merged_data_obesity, m = 5, maxiter = 10, verbose = FALSE)
  merged_data_imp_sleep <- miceRanger(merged_data_sleep, m = 5, maxiter = 10, verbose = FALSE)
  saveRDS(merged_data_imp_dep, "data/merged_data_imp_dep_cohort.rds")
  saveRDS(merged_data_imp_obesity, "data/merged_data_imp_obesity_cohort.rds")
  saveRDS(merged_data_imp_sleep, "data/merged_data_imp_sleep_cohort.rds")
  
  merged_data_imp_dep <- readRDS("data/merged_data_imp_dep_cohort.rds")
  merged_data_imp_obesity <- readRDS("data/merged_data_imp_obesity_cohort.rds")
  merged_data_imp_sleep <- readRDS("data/merged_data_imp_sleep_cohort.rds")
  
  
  # ONLY USE IMPUTED baseline DVs, IVs and covariates
  merge_imputed_back <- function(imp_obj, original_df, vars_to_replace,
                                 covars, IVs,
                                 keep_03A_from_original = TRUE) {
    
    imputed_list <- completeData(imp_obj)
    
    vars_to_drop <- unique(c(vars_to_replace, covars), all_of(IVs)) # Remove DV, IV, and covariates from original data
    
    lapply(seq_along(imputed_list), function(i) {
      
      imp <- imputed_list[[i]]
      
      # 1. Start from original structure
      base_df <- original_df %>%
        dplyr::select(-dplyr::any_of(vars_to_drop))
      
      # 2. Imputed values
      if (keep_03A_from_original) {
        
        # dep / obesity case
        imp_use <- imp %>%
          dplyr::filter(session_id != "ses-03A")
        
      } else {
        
        # sleep case
        imp_use <- imp
      }
      
      # 3. Merge imputed back
      df <- base_df %>%
        dplyr::left_join(
          imp_use %>%
            dplyr::select(participant_id, session_id,
                          dplyr::all_of(vars_to_drop)),
          by = c("participant_id", "session_id")
        )
      
      # 4. Fix outcome source
      
      # 04A outcome ALWAYS from original
      df <- df %>%
        dplyr::left_join(
          original_df %>%
            dplyr::filter(session_id == "ses-04A") %>%
            dplyr::select(participant_id, session_id,
                          dplyr::all_of(vars_to_replace)) %>%
            dplyr::rename_with(~ paste0(.x, "_orig"), dplyr::all_of(vars_to_replace))
        ) %>% 
        dplyr::mutate(
          new_DV_col = dplyr::case_when(
            session_id == "ses-04A" ~ (.data[[paste0(vars_to_replace, "_orig")]]),
            TRUE ~ (.data[[vars_to_replace]])
          )
        ) %>% 
        dplyr::select(-dplyr::ends_with("_orig"), -all_of(vars_to_replace)) %>%
        dplyr::rename(!!vars_to_replace := new_DV_col)
      # 5. Imputation id
      df %>%
        dplyr::arrange(participant_id, session_id) %>%
        dplyr::mutate(imputation = i)
    })
  }
  
  
  dep_merged_list <- merge_imputed_back(
    imp_obj = merged_data_imp_dep,
    original_df = merged_data,
    vars_to_replace = "depression_dx_y",
    covars = covars,
    IVs = IVs,
    keep_03A_from_original = TRUE
  )
  
  obesity_merged_list <- merge_imputed_back(
    imp_obj = merged_data_imp_obesity,
    original_df = merged_data,
    vars_to_replace = "bmi_obesity",
    covars = covars,
    IVs = IVs,
    keep_03A_from_original = TRUE
  )
  
  sleep_merged_list <- merge_imputed_back(
    imp_obj = merged_data_imp_sleep,
    original_df = merged_data,
    vars_to_replace = "sleep_duration_hrs",
    covars = covars,
    IVs = IVs,
    keep_03A_from_original = FALSE
  )
  
  
  smartphone_vars <- c(
    "watch_stream_TV_shows_movies_smartph_wsum",
    "single_player_video_games_smartph_wsum",
    "multiplayer_video_games_smartph_wsum",
    "text_cellphone_others_smartph_wsum",
    "visit_social_media_apps_smartph_wsum",
    "video_chat_not_for_school_smartph_wsum",
    "schoolyear_total_school_related_work_smartph_wsum",
    "schoolyear_total_smartph_wsum"
  )
  
  cut_levels <- c("0–2h", "2–5h", ">5h")
  
  make_time_category <- function(x) {
    case_when(
      x <= 2 ~ "0–2h",
      x <= 5 ~ "2–5h",
      x > 5  ~ ">5h",
      TRUE ~ NA_character_
    )
  }
  
  create_aim_datasets <- function(df, no_sm_2y_ids, no_sm_3y_ids,
                                  smartphone_vars,
                                  merged_data = merged_data,
                                  cut_levels = c("0–2h","2–5h",">5h"),
                                  imputation_id = NULL,
                                  data_type) {
    
    # ---------- join source ----------
    ## This is baseline DVs data, df is imputed
    join_df <- switch(data_type,
                      sleep   = df %>% dplyr::filter(session_id=="ses-03A") %>%
                        dplyr::select(participant_id, sleep_duration_hrs_3y = sleep_duration_hrs),
                      dep     = df %>% dplyr::filter(session_id=="ses-02A") %>%
                        dplyr::select(participant_id, depression_dx_y_2y = depression_dx_y),
                      obesity = df %>% dplyr::filter(session_id=="ses-02A") %>%
                        dplyr::select(participant_id, bmi_obesity_2y = bmi_obesity),
                      stop("data_type must be 'sleep','dep','obesity'")
    )
    
    # ---------- Aim1 ----------
    aim1_df <- df %>%
      dplyr::filter(session_id=="ses-04A",
                    participant_id %in% no_sm_3y_ids,
                    !is.na(smartphone_ownership)) %>%
      select(-session_id) %>% 
      dplyr::left_join(join_df, by="participant_id") %>%
      dplyr::mutate(dplyr::across(dplyr::matches("_2y$|_3y$|_y$"),
                                  ~as.numeric(as.character(.x))))
    
    # ---------- Aim2 ----------
    aim2_df <- df %>%
      dplyr::filter(session_id=="ses-04A", smartphone_ownership==1) %>%
      dplyr::mutate(dplyr::across(dplyr::matches("_y$"),
                                  ~as.numeric(as.character(.x)))) %>%
      dplyr::mutate(dplyr::across(dplyr::all_of(smartphone_vars),
                                  ~./7, .names="{.col}_wmean_perday")) %>%
      dplyr::mutate(dplyr::across(dplyr::ends_with("_wmean_perday"),
                                  ~dplyr::case_when(.<=2~"0–2h", .<=5~"2–5h", .>5~">5h"),
                                  .names="{.col}_cat")) %>%
      dplyr::mutate(dplyr::across(dplyr::ends_with("_cat"),
                                  ~stats::relevel(factor(.x, levels=cut_levels),
                                                  ref=cut_levels[1])))
    
    # ---------- subsets ----------
    aim2_df_no2y <- aim2_df %>%
      dplyr::filter(participant_id %in% no_sm_2y_ids) %>%
      dplyr::left_join(join_df, by="participant_id")
    
    aim2_df_no3y <- aim2_df %>%
      dplyr::filter(participant_id %in% no_sm_3y_ids) %>%
      dplyr::left_join(join_df, by="participant_id")
    
    # ---------- add lack_sleep only for sleep ----------
    if (identical(data_type, "sleep")) {
      add_sleep <- \(d) dplyr::mutate(d, lack_sleep = as.numeric(sleep_duration_hrs < 8))
      aim1_df <- add_sleep(aim1_df)
      aim2_df <- add_sleep(aim2_df)
      aim2_df_no2y <- add_sleep(aim2_df_no2y)
      aim2_df_no3y <- add_sleep(aim2_df_no3y)
    }
    
    # ---------- imputation id ----------
    if (!is.null(imputation_id))
      aim1_df$imputation <- aim2_df$imputation <- 
      aim2_df_no2y$imputation <- aim2_df_no3y$imputation <- imputation_id
    
    list(aim1_df=aim1_df, aim2_df=aim2_df,
         aim2_df_no2y=aim2_df_no2y, aim2_df_no3y=aim2_df_no3y)
  }
  
  
  aim_dep_list <- lapply(seq_along(dep_merged_list), function(i) {
    create_aim_datasets(
      df = dep_merged_list[[i]],
      no_sm_2y_ids = no_sm_2y_ids,
      no_sm_3y_ids = no_sm_3y_ids,
      smartphone_vars = smartphone_vars,
      merged_data = merged_data,
      imputation_id = i,
      data_type = "dep"
    )
  })
  
  aim_obesity_list <- lapply(seq_along(obesity_merged_list), function(i) {
    create_aim_datasets(
      df = obesity_merged_list[[i]],
      no_sm_2y_ids = no_sm_2y_ids,
      no_sm_3y_ids = no_sm_3y_ids,
      smartphone_vars = smartphone_vars,
      merged_data = merged_data,
      imputation_id = i,
      data_type = "obesity"
    )
  })
  
  aim_sleep_list <- lapply(seq_along(sleep_merged_list), function(i) {
    create_aim_datasets(
      df = sleep_merged_list[[i]],
      no_sm_2y_ids = no_sm_2y_ids,
      no_sm_3y_ids = no_sm_3y_ids,
      smartphone_vars = smartphone_vars,
      merged_data = merged_data,
      imputation_id = i,
      data_type = "sleep"
    )
  })
  
  saveRDS(aim_dep_list, "data/aim_dep_list_non_imp_DV.rds")
  saveRDS(aim_obesity_list, "data/aim_obesity_list_non_imp_DV.rds")
  saveRDS(aim_sleep_list, "data/aim_sleep_list_non_imp_DV.rds")
  
  return(list(aim_dep_list = aim_dep_list, aim_obesity_list = aim_obesity_list, aim_sleep_list = aim_sleep_list))
}


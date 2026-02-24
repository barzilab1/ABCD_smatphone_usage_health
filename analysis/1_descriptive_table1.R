
library(dplyr)
library(tableone)
source("analysis/analysis_utility_fun.R")

aim1_df <- readRDS("data/aim1_df.rds")
aim2_df_no3y <- readRDS("data/aim2_df_no3y.rds")
df_compare <- readRDS("data/df_compare.rds")


aim1_df$race_other_new <- ifelse( aim1_df$race_other ==1 | aim1_df$race_haw_pac ==1 | aim1_df$race_aian ==1, 1, 0)
df_compare$race_other_new <- ifelse( df_compare$race_other ==1 | df_compare$race_haw_pac ==1 | df_compare$race_aian ==1, 1, 0)

variables <- c(
  "age_br", # Age at 4-year
  "sex_br", "race_multi", "race_white", "race_black", "race_asian", "race_haw_pac", "race_aian", "race_other", "race_other_new",
  "ethnicity_hisp_br", #"parents_high_edu_text_br", 
  # Covariates at 3-year
  "age_br_3y", "household_income_3y", "parents_high_edu_text_br_3y", "parents_high_edu_br_master_above_3y",
  "puberty_both_sexes_3y", "late_or_post_puberty_both_sexes_3y", "fc_y_pm_mean_3y",
  # Outcome at 3-year, depression and obesity at 2-year
  "bmi_obesity_3y", "lack_sleep_3y", "bpm_T_bin_concern_3y",
  "depression_dx_y_2y", "bmi_obesity_2y", "sleep_duration_hrs_3y"
)

factors <- c("sex_br", "race_multi", "race_white", "race_black", "race_asian", "race_haw_pac", "race_aian", "race_other", "race_other_new",
             "ethnicity_hisp_br", #"parents_high_edu_text_br", "parents_high_edu_br_master_above",
             "parents_high_edu_text_br_3y", "parents_high_edu_br_master_above_3y",
             "puberty_both_sexes_3y", "late_or_post_puberty_both_sexes_3y",
             "bmi_obesity_3y", "lack_sleep_3y", "bpm_T_bin_concern_3y",
             "depression_dx_y_2y", "bmi_obesity_2y")


# Table 1
CreateTableOne(data = aim1_df,
               vars = variables[variables %in% names(aim1_df)],
               strata = "smartphone_ownership",
               factorVars = factors,
               addOverall = T)
naniar::miss_var_summary(aim1_df %>% select(all_of(variables))) %>% View()

# SD of schoolyear_total_smartph_wsum
sd(aim2_df_no3y$schoolyear_total_smartph_wsum, na.rm = T)
mean(aim2_df_no3y$schoolyear_total_smartph_wsum, na.rm = T)


# eTable 1 - Comparing included and not included
CreateTableOne(data = df_compare, 
               vars = variables[variables %in% names(aim1_df)],
               strata = "included",
               factorVars = factors,
               addOverall = T)
naniar::miss_var_summary(df_compare %>% select(all_of(variables))) %>% View()

# eTable 8 - Descriptive statistics of specific smartphone usages
IVs_aim2 <- c(
  "watch_stream_TV_shows_movies_smartph_wsum", #streaming
  "single_player_video_games_smartph_wsum", # video games
  "multiplayer_video_games_smartph_wsum", # video games
  "text_cellphone_others_smartph_wsum", # texting
  "visit_social_media_apps_smartph_wsum", # visiting social media sites 
  "video_chat_not_for_school_smartph_wsum", # video-chatting 
  "schoolyear_total_school_related_work_smartph_wsum" # school-related work
)
etable8 <- aim2_df_no3y %>%
  summarise(
    across(
      all_of(IVs_aim2),
      list(
        mean = ~ mean(., na.rm = TRUE),
        sd = ~ sd(., na.rm = TRUE),
        pct_missing = ~ mean(is.na(.)) * 100
      ),
      .names = "{.col}__{.fn}"
    )
  ) %>%
  pivot_longer(
    everything(),
    names_to = c("variable", ".value"),
    names_sep = "__"
  ) %>%
  mutate(
    mean_sd = sprintf("%.2f (%.2f)", mean, sd),
    pct_missing = sprintf("%.2f", pct_missing)
  ) %>%
  select(variable, mean_sd, pct_missing)


etable8








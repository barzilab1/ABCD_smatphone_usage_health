
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
                factorVars = factors)




# SD of schoolyear_total_smartph_wsum
sd(aim2_df_no3y$schoolyear_total_smartph_wsum, na.rm = T)
mean(aim2_df_no3y$schoolyear_total_smartph_wsum, na.rm = T)


# eTable 1 - Comparing included and not included
CreateTableOne(data = df_compare,
                vars = variables[variables %in% names(aim1_df)],
                strata = "included",
               factorVars = factors)











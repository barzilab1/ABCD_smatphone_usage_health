library(dplyr)
library(purrr)
library(EValue)
source("analysis/analysis_utility_fun.R")

aim_dep_list <- readRDS("data/aim_dep_list_non_imp_DV.rds")
aim_obesity_list <- readRDS("data/aim_obesity_list_non_imp_DV.rds")
aim_sleep_list <- readRDS("data/aim_sleep_list_non_imp_DV.rds")
aim_bpm_list <- readRDS("data/aim_bpm_list_non_imp_DV.rds")

covariates_list_aim1 <- readRDS("data/covariates_list_aim1.rds")
covariates_list_aim1_sens <- readRDS("data/covariates_list_aim1_sens.rds")
covariates_list_aim2 <- readRDS("data/covariates_list_aim2.rds")
covariates_list_aim2_2y <- readRDS("data/covariates_list_aim2_2y.rds")

random_effects_s <- "(1 | site_br/family_id_br)"

# eTable 2----
for (i in 1:5) {
  run_write_models(
    data = aim_dep_list[[i]]$aim1_df,
    list_DVs = "depression_dx_y",
    list_covars = covariates_list_aim1$depression_dx_y,
    random_eff = random_effects_s,
    list_IVs = "smartphone_ownership",
    binary_DV = TRUE,
    # ext = "eTable2",
    ext = paste0("eTable2_weight_non_imputed_DV", i),
    CI_level = (1 - 0.05)
  )
  
  
  run_write_models(
    data = aim_obesity_list[[i]]$aim1_df,
    list_DVs = "bmi_obesity",
    list_covars = covariates_list_aim2_2y$bmi_obesity,
    random_eff = random_effects_s,
    list_IVs = "smartphone_ownership",
    binary_DV = TRUE,
    ext = paste0("eTable2_weight_non_imputed_DV", i),
    CI_level = (1 - 0.05)
  )
  
  
  run_write_models(
    data = aim_sleep_list[[i]]$aim1_df,
    list_DVs = "lack_sleep",
    list_covars = covariates_list_aim1$lack_sleep,
    random_eff = random_effects_s,
    list_IVs = "smartphone_ownership",
    binary_DV = TRUE,
    ext = paste0("eTable2_weight_non_imputed_DV", i),
    CI_level = (1 - 0.05)
  )
}

calculate_pool_MI_from_table("DV__depression_dx_y__eTable2_weight_non_imputed_DV")
calculate_pool_MI_from_table("DV__bmi_obesity__eTable2_weight_non_imputed_DV")
calculate_pool_MI_from_table("DV__lack_sleep__eTable2_weight_non_imputed_DV")



# eTable 3 and 4----
## Frequency of outcomes
get_outcome_table <- function(data, var_y, var_2y) {
  table(data[[var_y]][!is.na(data[[var_2y]])])
}

lapply(1:5, function(i) {
  df <- aim_dep_list[[i]]$aim2_df_no3y
  get_outcome_table(df, "depression_dx_y", "depression_dx_y_2y")
})


lapply(1:5, function(i) {
  df <- aim_obesity_list[[i]]$aim2_df_no3y
  get_outcome_table(df, "bmi_obesity", "bmi_obesity_2y")
})

lapply(1:5, function(i) {
  df <- aim_sleep_list[[i]]$aim2_df_no3y
  get_outcome_table(df, "lack_sleep", "sleep_duration_hrs_3y")
})


for (i in 1:5) {
  run_write_models(
    data = aim_dep_list[[i]]$aim2_df_no3y,
    list_DVs = "depression_dx_y",
    list_IVs = c(
      "scale(schoolyear_total_smartph_wsum)",
      "scale(schoolyear_total_smartph_wsum)*sex_br"
    ),
    list_covars = covariates_list_aim2$depression_dx_y,
    random_eff = random_effects_s,
    binary_DV = TRUE,
    ext = paste0("eTable3_4_covar_weight_non_imputed_DV",i),
    CI_level = (1 - 0.05)
  )
  
  # baseline from 2
  run_write_models(
    data = aim_obesity_list[[i]]$aim2_df_no3y,
    list_DVs = "bmi_obesity",
    list_IVs = c(
      "scale(schoolyear_total_smartph_wsum)",
      "scale(schoolyear_total_smartph_wsum)*sex_br"
    ),
    list_covars = covariates_list_aim2_2y$bmi_obesity,
    random_eff = random_effects_s,
    binary_DV = TRUE,
    ext = paste0("eTable3_4_covar_weight_non_imputed_DV",i),
    CI_level = (1 - 0.05)
  )
  
  
  run_write_models(
    data = aim_sleep_list[[i]]$aim2_df_no3y,
    list_DVs = "lack_sleep",
    list_IVs = c(
      "scale(schoolyear_total_smartph_wsum)",
      "scale(schoolyear_total_smartph_wsum)*sex_br"
    ),
    list_covars = covariates_list_aim2$lack_sleep,
    random_eff = random_effects_s,
    binary_DV = TRUE,
    ext = paste0("eTable3_4_covar_weight_non_imputed_DV",i),
    CI_level = (1 - 0.05)
  )
}

calculate_pool_MI_from_table("DV__depression_dx_y__eTable3_4_covar_weight_non_imputed_DV")
calculate_pool_MI_from_table("DV__bmi_obesity__eTable3_4_covar_weight_non_imputed_DV")
calculate_pool_MI_from_table("DV__lack_sleep__eTable3_4_covar_weight_non_imputed_DV")


# eTable 5----
## Models during weekend and weekday
for(i in 1:5) {
  run_write_models(
    data = aim_dep_list[[i]]$aim2_df_no3y,
    list_DVs = "depression_dx_y",
    list_IVs = c("scale(nt_y_stq__screen__wkdy__tot__pair_hr_min_002)"),
    list_covars = covariates_list_aim2$depression_dx_y,
    random_eff = random_effects_s,
    binary_DV = TRUE,
    ext = paste0("eTable5_weekday_MID",i),
    CI_level = (1 - 0.05)
  )
  
  run_write_models(
    data = aim_obesity_list[[i]]$aim2_df_no3y,
    list_DVs = "bmi_obesity",
    list_IVs = c("scale(nt_y_stq__screen__wkdy__tot__pair_hr_min_002)"),
    list_covars = covariates_list_aim2_2y$bmi_obesity,
    random_eff = random_effects_s,
    binary_DV = TRUE,
    ext = paste0("eTable5_weekday_MID",i),
    CI_level = (1 - 0.05)
  )
  
  run_write_models(
    data = aim_sleep_list[[i]]$aim2_df_no3y,
    list_DVs = "lack_sleep",
    list_IVs = c("scale(nt_y_stq__screen__wkdy__tot__pair_hr_min_002)"),
    list_covars = covariates_list_aim2$lack_sleep,
    random_eff = random_effects_s,
    binary_DV = TRUE,
    ext = paste0("eTable5_weekday_MID",i),
    CI_level = (1 - 0.05)
  )
  
  run_write_models(
    data = aim_dep_list[[i]]$aim2_df_no3y,
    list_DVs = "depression_dx_y",
    list_IVs = c("scale(nt_y_stq__screen__wknd__tot__pair_hr_min_002)"),
    list_covars = covariates_list_aim2$depression_dx_y,
    random_eff = random_effects_s,
    binary_DV = TRUE,
    ext = paste0("eTable5_weekend_MID",i),
    CI_level = (1 - 0.05)
  )
  
  run_write_models(
    data = aim_obesity_list[[i]]$aim2_df_no3y,
    list_DVs = "bmi_obesity",
    list_IVs = c("scale(nt_y_stq__screen__wknd__tot__pair_hr_min_002)"),
    list_covars = covariates_list_aim2_2y$bmi_obesity,
    random_eff = random_effects_s,
    binary_DV = TRUE,
    ext = paste0("eTable5_weekend_MID",i),
    CI_level = (1 - 0.05)
  )
  
  
  run_write_models(
    data = aim_sleep_list[[i]]$aim2_df_no3y,
    list_DVs = "lack_sleep",
    list_IVs = c("scale(nt_y_stq__screen__wknd__tot__pair_hr_min_002)"),
    list_covars = covariates_list_aim2$lack_sleep,
    random_eff = random_effects_s,
    binary_DV = TRUE,
    ext = paste0("eTable5_weekend_MID",i),
    CI_level = (1 - 0.05)
  )
}

calculate_pool_MI_from_table("DV__depression_dx_y__eTable5_weekday_MID")
calculate_pool_MI_from_table("DV__bmi_obesity__eTable5_weekday_MID")
calculate_pool_MI_from_table("DV__lack_sleep__eTable5_weekday_MID")
calculate_pool_MI_from_table("DV__depression_dx_y__eTable5_weekend_MID")
calculate_pool_MI_from_table("DV__bmi_obesity__eTable5_weekend_MID")
calculate_pool_MI_from_table("DV__lack_sleep__eTable5_weekend_MID")


# eTable 6 total smartphone time categories----
for (i in 1:5) {
  run_write_models(
    data = aim_dep_list[[i]]$aim2_df_no3y,
    list_DVs = "depression_dx_y",
    list_IVs = "schoolyear_total_smartph_wsum_wmean_perday_cat",
    list_covars = covariates_list_aim2$depression_dx_y,
    random_eff = random_effects_s,
    binary_DV = TRUE,
    ext = paste0("eTable6_covar_weight_non_imputed_DV",i),
    CI_level = (1 - 0.05)
  )
  
  run_write_models(
    data = aim_obesity_list[[i]]$aim2_df_no3y,
    list_DVs = "bmi_obesity",
    list_IVs = "schoolyear_total_smartph_wsum_wmean_perday_cat",
    list_covars = covariates_list_aim2_2y$bmi_obesity,
    random_eff = random_effects_s,
    binary_DV = TRUE,
    ext = paste0("eTable6_covar_weight_non_imputed_DV",i),
    CI_level = (1 - 0.05)
  )
  
  run_write_models(
    data = aim_sleep_list[[i]]$aim2_df_no3y,
    list_DVs = "lack_sleep",
    list_IVs = "schoolyear_total_smartph_wsum_wmean_perday_cat",
    list_covars = covariates_list_aim2$lack_sleep,
    random_eff = random_effects_s,
    binary_DV = TRUE,
    ext = paste0("eTable6_covar_weight_non_imputed_DV",i),
    CI_level = (1 - 0.05)
  )
}

calculate_pool_MI_from_table("DV__depression_dx_y__eTable6_covar_weight_non_imputed_DV")
calculate_pool_MI_from_table("DV__bmi_obesity__eTable6_covar_weight_non_imputed_DV")
calculate_pool_MI_from_table("DV__lack_sleep__eTable6_covar_weight_non_imputed_DV")


# Number of participants in each category of total smartphone time (using sleep datasets)----
# 1. Extract dataframes and bind them
list_of_dfs <- lapply(aim_sleep_list[1:5], function(x) x$aim2_df_no3y)
combined_imputations <- bind_rows(list_of_dfs, .id = "imputation_number")

# 2. Calculate the average count (Mean_N) first
pooled_results <- combined_imputations %>%
  group_by(imputation_number, schoolyear_total_smartph_wsum_wmean_perday_cat) %>%
  tally() %>%
  group_by(schoolyear_total_smartph_wsum_wmean_perday_cat) %>%
  dplyr::summarize(Mean_N = mean(n, na.rm = TRUE))

# 3. Now calculate the percentage based on the sum of those Mean_Ns
pooled_results <- pooled_results %>%
  mutate(
    Mean_N = Mean_N,
    Percentage = round((Mean_N / sum(Mean_N)) * 100, 4)
  )

print(as.data.frame(pooled_results))


# eTable 7----
for (i in 1:5) {
  run_write_models(
    data = aim_dep_list[[i]]$aim2_df_no3y,
    list_DVs = "depression_dx_y",
    list_IVs = c("nt_y_stq__sleep_002_phone_outside_room"),
    list_covars = covariates_list_aim2$depression_dx_y,
    random_eff = random_effects_s,
    binary_DV = TRUE,
    ext = paste0("eTable7_covar_weight_non_imputed_DV", i),
    CI_level = (1 - 0.05)
  )
  
  run_write_models(
    data = aim_obesity_list[[i]]$aim2_df_no3y,
    list_DVs = "bmi_obesity",
    list_IVs = c("nt_y_stq__sleep_002_phone_outside_room"),
    list_covars = covariates_list_aim2_2y$bmi_obesity,
    random_eff = random_effects_s,
    binary_DV = TRUE,
    ext = paste0("eTable7_covar_weight_non_imputed_DV", i),
    CI_level = (1 - 0.05)
  )
  
  run_write_models(
    data = aim_sleep_list[[i]]$aim2_df_no3y,
    list_DVs = "lack_sleep",
    list_IVs = c("nt_y_stq__sleep_002_phone_outside_room"),
    list_covars = covariates_list_aim2$lack_sleep,
    random_eff = random_effects_s,
    binary_DV = TRUE,
    ext = paste0("eTable7_covar_weight_non_imputed_DV", i),
    CI_level = (1 - 0.05)
  )
}

calculate_pool_MI_from_table("DV__depression_dx_y__eTable7_covar_weight_non_imputed_DV")
calculate_pool_MI_from_table("DV__bmi_obesity__eTable7_covar_weight_non_imputed_DV")
calculate_pool_MI_from_table("DV__lack_sleep__eTable7_covar_weight_non_imputed_DV")


aim_bpm_list <- readRDS("data/aim_bpm_list_non_imp_DV.rds")

# eTable 14 bpm as DV----
for(i in 1:5) {
  run_write_models(
    data = aim_bpm_list[[i]]$aim1_df,
    list_DVs = "bpm_T_bin_concern",
    list_covars = covariates_list_aim1$mh_y_bpm_tscore,
    random_eff = random_effects_s,
    list_IVs = "smartphone_ownership",
    binary_DV = TRUE,
    ext = paste0("BPM_con_eTable14_MID",i),
    CI_level = (1-0.05)
  )
}
calculate_pool_MI_from_table("DV__bpm_T_bin_concern__BPM_con_eTable14_MID")

#eTable 15 continues DVs----
for(i in 1:5) {
  run_write_models(
    data = aim_bpm_list[[i]]$aim1_df,
    list_DVs = "scale(mh_y_bpm_tscore)",
    list_covars = covariates_list_aim1_sens$depression_dx_y,
    random_eff = random_effects_s,
    list_IVs = "smartphone_ownership",
    binary_DV = F,
    ext = paste0("eTable15_MID",i),
    CI_level = (1 - 0.05)
  )
  
  run_write_models(
    data = aim_obesity_list[[i]]$aim1_df,
    list_DVs = "scale(bmi)",
    list_covars = covariates_list_aim2_2y$bmi_obesity,
    random_eff = random_effects_s,
    list_IVs = "smartphone_ownership",
    binary_DV = F,
    ext = paste0("eTable15_MID",i),
    CI_level = (1 - 0.05)
  )
  
  
  run_write_models(
    data = aim_sleep_list[[i]]$aim1_df,
    list_DVs = "scale(sleep_duration_hrs)",
    list_covars = covariates_list_aim1$lack_sleep,
    random_eff = random_effects_s,
    list_IVs = "smartphone_ownership",
    binary_DV = F,
    ext = paste0("eTable15_MID",i),
    CI_level = (1 - 0.05)
  )
}

calculate_pool_MI_from_table("DV__scale(mh_y_bpm_tscore)__eTable15_MID", is_OR = F)
calculate_pool_MI_from_table("DV__scale(bmi)__eTable15_MID", is_OR = F)
calculate_pool_MI_from_table("DV__scale(sleep_duration_hrs)__eTable15_MID", is_OR = F)


# eTable 17 # EValues----
aim2_df_no3y = readRDS("data/aim2_df_no3y.rds")
prop.table(table(aim2_df_no3y$depression_dx_y)) * 100 # rare
prop.table(table(aim2_df_no3y$lack_sleep)) * 100 # not rare
prop.table(table(aim2_df_no3y$bmi_obesity)) * 100 # not rare

print(as.data.frame(evalues.OR(1.22, 1.005, 1.8, rare = TRUE)) %>% 
  mutate(outcome = "Depression diagnosis", prevalence =  prop.table(table(aim2_df_no3y$depression_dx_y))[2] * 100) %>%  #1.22	1.005–1.8
  bind_rows(as.data.frame(evalues.OR(1.28, 1.12, 1.47, rare = FALSE)) %>% 
              mutate(outcome = "Insufficient Sleep", prevalence = prop.table(table(aim2_df_no3y$lack_sleep))[2] * 100)) %>% #1.28	1.12–1.47	
  bind_rows(as.data.frame(evalues.OR(1.34, 1.09, 1.65, rare = FALSE)) %>% 
              mutate(outcome = "Obesity", prevalence = prop.table(table(aim2_df_no3y$bmi_obesity))[2] * 100)) # 1.34	1.09–1.65
)


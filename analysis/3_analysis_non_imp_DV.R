library(dplyr)
source("analysis/analysis_utility_fun.R")

aim_dep_list <- readRDS("data/aim_dep_list_non_imp_DV.rds")
aim_obesity_list <- readRDS("data/aim_obesity_list_non_imp_DV.rds")
aim_sleep_list <- readRDS("data/aim_sleep_list_non_imp_DV.rds")

covariates_list_aim1 <- readRDS("data/covariates_list_aim1.rds")
covariates_list_aim1_sens <- readRDS("data/covariates_list_aim1_sens.rds")
covariates_list_aim2 <- readRDS("data/covariates_list_aim2.rds")
covariates_list_aim2_2y <- readRDS("data/covariates_list_aim2_2y.rds")


random_effects_s <- "(1 | site_br/family_id_br)"

IVs_aim2 <- c(
  "watch_stream_TV_shows_movies_smartph_wsum", #streaming
  "single_player_video_games_smartph_wsum", # video games
  "multiplayer_video_games_smartph_wsum", # video games
  "text_cellphone_others_smartph_wsum", # texting
  "visit_social_media_apps_smartph_wsum", # visiting social media sites 
  "video_chat_not_for_school_smartph_wsum", # video-chatting 
  "schoolyear_total_school_related_work_smartph_wsum" # school-related work
)

IVs_aim2 <- map_chr(IVs_aim2, ~ paste0("scale(", .x, ")"))


DVs <- c("depression_dx_y", "bmi_obesity", "lack_sleep")
IVs_aim2_sens2 <- paste(IVs_aim2, collapse = " + ")


## Imputation 1----
# eTable 2----
run_write_models(
  data = aim_dep_list[[1]]$aim1_df,
  list_DVs = "depression_dx_y",
  list_covars = covariates_list_aim1$depression_dx_y,
  random_eff = random_effects_s,
  list_IVs = "smartphone_ownership",
  binary_DV = TRUE,
  # ext = "eTable2",
  ext = "eTable2_covar_weight_non_imputed_DV_1",
  CI_level = (1-0.05)
)


run_write_models(
  data = aim_obesity_list[[1]]$aim1_df,
  list_DVs = "bmi_obesity",
  list_covars = covariates_list_aim2_2y$bmi_obesity,
  random_eff = random_effects_s,
  list_IVs = "smartphone_ownership",
  binary_DV = TRUE,
  ext = "eTable2_covary_obesity2y_covar_weight_non_imputed_DV_1",
  CI_level = (1-0.05)
)


run_write_models(
  data = aim_sleep_list[[1]]$aim1_df,
  list_DVs = "lack_sleep",
  list_covars = covariates_list_aim1$lack_sleep,
  random_eff = random_effects_s,
  list_IVs = "smartphone_ownership",
  binary_DV = TRUE,
  # ext = "eTable2",
  ext = "eTable2_covar_weight_non_imputed_DV_1",
  CI_level = (1-0.05)
)

# eTable 3 and 4----
run_write_models(
  data = aim_dep_list[[1]]$aim2_df_no3y,
  list_DVs = "depression_dx_y",
  list_IVs = c("scale(schoolyear_total_smartph_wsum)", "scale(schoolyear_total_smartph_wsum)*sex_br"),
  list_covars = covariates_list_aim2$depression_dx_y,
  random_eff = random_effects_s,
  binary_DV = TRUE,
  # ext = "eTable3_4",
  ext = "eTable3_4_covar_weight_non_imputed_DV_1",
  CI_level = (1-0.05)
)

# baseline from 2
run_write_models(
  data = aim_obesity_list[[1]]$aim2_df_no3y,
  list_DVs = "bmi_obesity",
  list_IVs = c("scale(schoolyear_total_smartph_wsum)", "scale(schoolyear_total_smartph_wsum)*sex_br"),
  list_covars = covariates_list_aim2_2y$bmi_obesity,
  random_eff = random_effects_s,
  binary_DV = TRUE,
  # ext = "eTable3_4",
  ext = "eTable3_4_covar_weight_non_imputed_DV_1",
  CI_level = (1-0.05)
)


run_write_models(
  data = aim_sleep_list[[1]]$aim2_df_no3y,
  list_DVs = "lack_sleep",
  list_IVs = c("scale(schoolyear_total_smartph_wsum)", "scale(schoolyear_total_smartph_wsum)*sex_br"),
  list_covars = covariates_list_aim2$lack_sleep,
  random_eff = random_effects_s,
  binary_DV = TRUE,
  # ext = "eTable3_4",
  ext = "eTable3_4_covar_weight_non_imputed_DV_1",
  CI_level = (1-0.05)
)

# eTable 6 smartphone time categories----
## Add models for obesity
run_write_models(
  data = aim_dep_list[[1]]$aim2_df_no3y,
  list_DVs = "depression_dx_y",
  list_IVs = "schoolyear_total_smartph_wsum_wmean_perday_cat",
  list_covars = covariates_list_aim2$depression_dx_y,
  random_eff = random_effects_s,
  binary_DV = TRUE,
  ext = "eTable6_covar_weight_non_imputed_DV_1",
  CI_level = (1-0.05)
)

run_write_models(
  data = aim_obesity_list[[1]]$aim2_df_no3y,
  list_DVs = "bmi_obesity",
  list_IVs = "schoolyear_total_smartph_wsum_wmean_perday_cat",
  list_covars = covariates_list_aim2_2y$bmi_obesity,
  random_eff = random_effects_s,
  binary_DV = TRUE,
  ext = "eTable6_covar_weight_non_imputed_DV_1",
  CI_level = (1-0.05)
)

run_write_models(
  data = aim_sleep_list[[1]]$aim2_df_no3y,
  list_DVs = "lack_sleep",
  list_IVs = "schoolyear_total_smartph_wsum_wmean_perday_cat",
  list_covars = covariates_list_aim2$lack_sleep,
  random_eff = random_effects_s,
  binary_DV = TRUE,
  ext = "eTable6_covar_weight_non_imputed_DV_1",
  CI_level = (1-0.05)
)


# eTable 7----
run_write_models(
  data = aim_dep_list[[1]]$aim2_df_no3y,
  list_DVs = "depression_dx_y",
  list_IVs = c("nt_y_stq__sleep_002_phone_outside_room"),
  list_covars = covariates_list_aim2$depression_dx_y,
  random_eff = random_effects_s,
  binary_DV = TRUE,
  ext = "eTable7_covar_weight_non_imputed_DV_1",
  CI_level = (1-0.05)
)

run_write_models(
  data = aim_obesity_list[[1]]$aim2_df_no3y,
  list_DVs = "bmi_obesity",
  list_IVs = c("nt_y_stq__sleep_002_phone_outside_room"),
  list_covars = covariates_list_aim2_2y$bmi_obesity,
  random_eff = random_effects_s,
  binary_DV = TRUE,
  ext = "eTable7_covar_weight_non_imputed_DV_1",
  CI_level = (1-0.05)
)


run_write_models(
  data = aim_sleep_list[[1]]$aim2_df_no3y,
  list_DVs = "lack_sleep",
  list_IVs = c("nt_y_stq__sleep_002_phone_outside_room"),
  list_covars = covariates_list_aim2$lack_sleep,
  random_eff = random_effects_s,
  binary_DV = TRUE,
  ext = "eTable7_covar_weight_non_imputed_DV_1",
  CI_level = (1-0.05)
)


## Imputation 2----
# eTable 2----
run_write_models(
  data = aim_dep_list[[2]]$aim1_df,
  list_DVs = "depression_dx_y",
  list_covars = covariates_list_aim1$depression_dx_y,
  random_eff = random_effects_s,
  list_IVs = "smartphone_ownership",
  binary_DV = TRUE,
  # ext = "eTable2",
  ext = "eTable2_covar_weight_non_imputed_DV_2",
  CI_level = (1-0.05)
)


run_write_models(
  data = aim_obesity_list[[2]]$aim1_df,
  list_DVs = "bmi_obesity",
  list_covars = covariates_list_aim2_2y$bmi_obesity,
  random_eff = random_effects_s,
  list_IVs = "smartphone_ownership",
  binary_DV = TRUE,
  # ext = "eTable2_covary_obesity2y",
  ext = "eTable2_covary_obesity2y_covar_weight_non_imputed_DV_2",
  CI_level = (1-0.05)
)


run_write_models(
  data = aim_sleep_list[[2]]$aim1_df,
  list_DVs = "lack_sleep",
  list_covars = covariates_list_aim1$lack_sleep,
  random_eff = random_effects_s,
  list_IVs = "smartphone_ownership",
  binary_DV = TRUE,
  # ext = "eTable2",
  ext = "eTable2_covar_weight_non_imputed_DV_2",
  CI_level = (1-0.05)
)

# eTable 3 and 4----
run_write_models(
  data = aim_dep_list[[2]]$aim2_df_no3y,
  list_DVs = "depression_dx_y",
  list_IVs = c("scale(schoolyear_total_smartph_wsum)", "scale(schoolyear_total_smartph_wsum)*sex_br"),
  list_covars = covariates_list_aim2$depression_dx_y,
  random_eff = random_effects_s,
  binary_DV = TRUE,
  # ext = "eTable3_4",
  ext = "eTable3_4_covar_weight_non_imputed_DV_2",
  CI_level = (1-0.05)
)

# baseline from 2
run_write_models(
  data = aim_obesity_list[[2]]$aim2_df_no3y,
  list_DVs = "bmi_obesity",
  list_IVs = c("scale(schoolyear_total_smartph_wsum)", "scale(schoolyear_total_smartph_wsum)*sex_br"),
  list_covars = covariates_list_aim2_2y$bmi_obesity,
  random_eff = random_effects_s,
  binary_DV = TRUE,
  # ext = "eTable3_4",
  ext = "eTable3_4_covar_weight_non_imputed_DV_2",
  CI_level = (1-0.05)
)


run_write_models(
  data = aim_sleep_list[[2]]$aim2_df_no3y,
  list_DVs = "lack_sleep",
  list_IVs = c("scale(schoolyear_total_smartph_wsum)", "scale(schoolyear_total_smartph_wsum)*sex_br"),
  list_covars = covariates_list_aim2$lack_sleep,
  random_eff = random_effects_s,
  binary_DV = TRUE,
  # ext = "eTable3_4",
  ext = "eTable3_4_covar_weight_non_imputed_DV_2",
  CI_level = (1-0.05)
)

# eTable 6 smartphone time categories----
run_write_models(
  data = aim_dep_list[[2]]$aim2_df_no3y,
  list_DVs = "depression_dx_y",
  list_IVs = "schoolyear_total_smartph_wsum_wmean_perday_cat",
  list_covars = covariates_list_aim2$depression_dx_y,
  random_eff = random_effects_s,
  binary_DV = TRUE,
  ext = "eTable6_covar_weight_non_imputed_DV_2",
  CI_level = (1-0.05)
)

run_write_models(
  data = aim_obesity_list[[2]]$aim2_df_no3y,
  list_DVs = "bmi_obesity",
  list_IVs = "schoolyear_total_smartph_wsum_wmean_perday_cat",
  list_covars = covariates_list_aim2_2y$bmi_obesity,
  random_eff = random_effects_s,
  binary_DV = TRUE,
  ext = "eTable6_covar_weight_non_imputed_DV_2",
  CI_level = (1-0.05)
)


run_write_models(
  data = aim_sleep_list[[2]]$aim2_df_no3y,
  list_DVs = "lack_sleep",
  list_IVs = "schoolyear_total_smartph_wsum_wmean_perday_cat",
  list_covars = covariates_list_aim2$lack_sleep,
  random_eff = random_effects_s,
  binary_DV = TRUE,
  ext = "eTable6_covar_weight_non_imputed_DV_2",
  CI_level = (1-0.05)
)


# eTable 7----
run_write_models(
  data = aim_dep_list[[2]]$aim2_df_no3y,
  list_DVs = "depression_dx_y",
  list_IVs = c("nt_y_stq__sleep_002_phone_outside_room"),
  list_covars = covariates_list_aim2$depression_dx_y,
  random_eff = random_effects_s,
  binary_DV = TRUE,
  ext = "eTable7_covar_weight_non_imputed_DV_2",
  CI_level = (1-0.05)
)

run_write_models(
  data = aim_obesity_list[[2]]$aim2_df_no3y,
  list_DVs = "bmi_obesity",
  list_IVs = c("nt_y_stq__sleep_002_phone_outside_room"),
  list_covars = covariates_list_aim2_2y$bmi_obesity,
  random_eff = random_effects_s,
  binary_DV = TRUE,
  ext = "eTable7_covar_weight_non_imputed_DV_2",
  CI_level = (1-0.05)
)

run_write_models(
  data = aim_sleep_list[[2]]$aim2_df_no3y,
  list_DVs = "lack_sleep",
  list_IVs = c("nt_y_stq__sleep_002_phone_outside_room"),
  list_covars = covariates_list_aim2$lack_sleep,
  random_eff = random_effects_s,
  binary_DV = TRUE,
  ext = "eTable7_covar_weight_non_imputed_DV_2",
  CI_level = (1-0.05)
)


## Imputation 3----
# eTable 2----
run_write_models(
  data = aim_dep_list[[3]]$aim1_df,
  list_DVs = "depression_dx_y",
  list_covars = covariates_list_aim1$depression_dx_y,
  random_eff = random_effects_s,
  list_IVs = "smartphone_ownership",
  binary_DV = TRUE,
  # ext = "eTable2",
  ext = "eTable2_covar_weight_non_imputed_DV_3",
  CI_level = (1-0.05)
)


run_write_models(
  data = aim_obesity_list[[3]]$aim1_df,
  list_DVs = "bmi_obesity",
  list_covars = covariates_list_aim2_2y$bmi_obesity,
  random_eff = random_effects_s,
  list_IVs = "smartphone_ownership",
  binary_DV = TRUE,
  # ext = "eTable2_covary_obesity2y",
  ext = "eTable2_covary_obesity2y_covar_weight_non_imputed_DV_3",
  CI_level = (1-0.05)
)


run_write_models(
  data = aim_sleep_list[[3]]$aim1_df,
  list_DVs = "lack_sleep",
  list_covars = covariates_list_aim1$lack_sleep,
  random_eff = random_effects_s,
  list_IVs = "smartphone_ownership",
  binary_DV = TRUE,
  # ext = "eTable2",
  ext = "eTable2_covar_weight_non_imputed_DV_3",
  CI_level = (1-0.05)
)

# eTable 3 and 4----
run_write_models(
  data = aim_dep_list[[3]]$aim2_df_no3y,
  list_DVs = "depression_dx_y",
  list_IVs = c("scale(schoolyear_total_smartph_wsum)", "scale(schoolyear_total_smartph_wsum)*sex_br"),
  list_covars = covariates_list_aim2$depression_dx_y,
  random_eff = random_effects_s,
  binary_DV = TRUE,
  # ext = "eTable3_4",
  ext = "eTable3_4_covar_weight_non_imputed_DV_3",
  CI_level = (1-0.05)
)

# baseline from 2
run_write_models(
  data = aim_obesity_list[[3]]$aim2_df_no3y,
  list_DVs = "bmi_obesity",
  list_IVs = c("scale(schoolyear_total_smartph_wsum)", "scale(schoolyear_total_smartph_wsum)*sex_br"),
  list_covars = covariates_list_aim2_2y$bmi_obesity,
  random_eff = random_effects_s,
  binary_DV = TRUE,
  # ext = "eTable3_4",
  ext = "eTable3_4_covar_weight_non_imputed_DV_3",
  CI_level = (1-0.05)
)


run_write_models(
  data = aim_sleep_list[[3]]$aim2_df_no3y,
  list_DVs = "lack_sleep",
  list_IVs = c("scale(schoolyear_total_smartph_wsum)", "scale(schoolyear_total_smartph_wsum)*sex_br"),
  list_covars = covariates_list_aim2$lack_sleep,
  random_eff = random_effects_s,
  binary_DV = TRUE,
  # ext = "eTable3_4",
  ext = "eTable3_4_covar_weight_non_imputed_DV_3",
  CI_level = (1-0.05)
)

# eTable 6 smartphone time categories----
run_write_models(
  data = aim_dep_list[[3]]$aim2_df_no3y,
  list_DVs = "depression_dx_y",
  list_IVs = "schoolyear_total_smartph_wsum_wmean_perday_cat",
  list_covars = covariates_list_aim2$depression_dx_y,
  random_eff = random_effects_s,
  binary_DV = TRUE,
  ext = "eTable6_covar_weight_non_imputed_DV_3",
  CI_level = (1-0.05)
)

run_write_models(
  data = aim_obesity_list[[3]]$aim2_df_no3y,
  list_DVs = "bmi_obesity",
  list_IVs = "schoolyear_total_smartph_wsum_wmean_perday_cat",
  list_covars = covariates_list_aim2_2y$bmi_obesity,
  random_eff = random_effects_s,
  binary_DV = TRUE,
  ext = "eTable6_covar_weight_non_imputed_DV_3",
  CI_level = (1-0.05)
)


run_write_models(
  data = aim_sleep_list[[3]]$aim2_df_no3y,
  list_DVs = "lack_sleep",
  list_IVs = "schoolyear_total_smartph_wsum_wmean_perday_cat",
  list_covars = covariates_list_aim2$lack_sleep,
  random_eff = random_effects_s,
  binary_DV = TRUE,
  ext = "eTable6_covar_weight_non_imputed_DV_3",
  CI_level = (1-0.05)
)


# eTable 7----
run_write_models(
  data = aim_dep_list[[3]]$aim2_df_no3y,
  list_DVs = "depression_dx_y",
  list_IVs = c("nt_y_stq__sleep_002_phone_outside_room"),
  list_covars = covariates_list_aim2$depression_dx_y,
  random_eff = random_effects_s,
  binary_DV = TRUE,
  ext = "eTable7_covar_weight_non_imputed_DV_3",
  CI_level = (1-0.05)
)

run_write_models(
  data = aim_obesity_list[[3]]$aim2_df_no3y,
  list_DVs = "bmi_obesity",
  list_IVs = c("nt_y_stq__sleep_002_phone_outside_room"),
  list_covars = covariates_list_aim2_2y$bmi_obesity,
  random_eff = random_effects_s,
  binary_DV = TRUE,
  ext = "eTable7_covar_weight_non_imputed_DV_3",
  CI_level = (1-0.05)
)

run_write_models(
  data = aim_sleep_list[[3]]$aim2_df_no3y,
  list_DVs = "lack_sleep",
  list_IVs = c("nt_y_stq__sleep_002_phone_outside_room"),
  list_covars = covariates_list_aim2$lack_sleep,
  random_eff = random_effects_s,
  binary_DV = TRUE,
  ext = "eTable7_covar_weight_non_imputed_DV_3",
  CI_level = (1-0.05)
)


## Imputation 4----
# eTable 2----
run_write_models(
  data = aim_dep_list[[4]]$aim1_df,
  list_DVs = "depression_dx_y",
  list_covars = covariates_list_aim1$depression_dx_y,
  random_eff = random_effects_s,
  list_IVs = "smartphone_ownership",
  binary_DV = TRUE,
  # ext = "eTable2",
  ext = "eTable2_covar_weight_non_imputed_DV_4",
  CI_level = (1-0.05)
)


run_write_models(
  data = aim_obesity_list[[4]]$aim1_df,
  list_DVs = "bmi_obesity",
  list_covars = covariates_list_aim2_2y$bmi_obesity,
  random_eff = random_effects_s,
  list_IVs = "smartphone_ownership",
  binary_DV = TRUE,
  # ext = "eTable2_covary_obesity2y",
  ext = "eTable2_covary_obesity2y_covar_weight_non_imputed_DV_4",
  CI_level = (1-0.05)
)


run_write_models(
  data = aim_sleep_list[[4]]$aim1_df,
  list_DVs = "lack_sleep",
  list_covars = covariates_list_aim1$lack_sleep,
  random_eff = random_effects_s,
  list_IVs = "smartphone_ownership",
  binary_DV = TRUE,
  # ext = "eTable2",
  ext = "eTable2_covar_weight_non_imputed_DV_4",
  CI_level = (1-0.05)
)

# eTable 3 and 4----
run_write_models(
  data = aim_dep_list[[4]]$aim2_df_no3y,
  list_DVs = "depression_dx_y",
  list_IVs = c("scale(schoolyear_total_smartph_wsum)", "scale(schoolyear_total_smartph_wsum)*sex_br"),
  list_covars = covariates_list_aim2$depression_dx_y,
  random_eff = random_effects_s,
  binary_DV = TRUE,
  # ext = "eTable3_4",
  ext = "eTable3_4_covar_weight_non_imputed_DV_4",
  CI_level = (1-0.05)
)

# baseline from 2
run_write_models(
  data = aim_obesity_list[[4]]$aim2_df_no3y,
  list_DVs = "bmi_obesity",
  list_IVs = c("scale(schoolyear_total_smartph_wsum)", "scale(schoolyear_total_smartph_wsum)*sex_br"),
  list_covars = covariates_list_aim2_2y$bmi_obesity,
  random_eff = random_effects_s,
  binary_DV = TRUE,
  # ext = "eTable3_4",
  ext = "eTable3_4_covar_weight_non_imputed_DV_4",
  CI_level = (1-0.05)
)


run_write_models(
  data = aim_sleep_list[[4]]$aim2_df_no3y,
  list_DVs = "lack_sleep",
  list_IVs = c("scale(schoolyear_total_smartph_wsum)", "scale(schoolyear_total_smartph_wsum)*sex_br"),
  list_covars = covariates_list_aim2$lack_sleep,
  random_eff = random_effects_s,
  binary_DV = TRUE,
  # ext = "eTable3_4",
  ext = "eTable3_4_covar_weight_non_imputed_DV_4",
  CI_level = (1-0.05)
)


# eTable 6 smartphone time categories----
run_write_models(
  data = aim_dep_list[[4]]$aim2_df_no3y,
  list_DVs = "depression_dx_y",
  list_IVs = "schoolyear_total_smartph_wsum_wmean_perday_cat",
  list_covars = covariates_list_aim2$depression_dx_y,
  random_eff = random_effects_s,
  binary_DV = TRUE,
  ext = "eTable6_covar_weight_non_imputed_DV_4",
  CI_level = (1-0.05)
)

run_write_models(
  data = aim_obesity_list[[4]]$aim2_df_no3y,
  list_DVs = "bmi_obesity",
  list_IVs = "schoolyear_total_smartph_wsum_wmean_perday_cat",
  list_covars = covariates_list_aim2_2y$bmi_obesity,
  random_eff = random_effects_s,
  binary_DV = TRUE,
  ext = "eTable6_covar_weight_non_imputed_DV_4",
  CI_level = (1-0.05)
)

run_write_models(
  data = aim_sleep_list[[4]]$aim2_df_no3y,
  list_DVs = "lack_sleep",
  list_IVs = "schoolyear_total_smartph_wsum_wmean_perday_cat",
  list_covars = covariates_list_aim2$lack_sleep,
  random_eff = random_effects_s,
  binary_DV = TRUE,
  ext = "eTable6_covar_weight_non_imputed_DV_4",
  CI_level = (1-0.05)
)


# eTable 7----
run_write_models(
  data = aim_dep_list[[4]]$aim2_df_no3y,
  list_DVs = "depression_dx_y",
  list_IVs = c("nt_y_stq__sleep_002_phone_outside_room"),
  list_covars = covariates_list_aim2$depression_dx_y,
  random_eff = random_effects_s,
  binary_DV = TRUE,
  ext = "eTable7_covar_weight_non_imputed_DV_4",
  CI_level = (1-0.05)
)

run_write_models(
  data = aim_obesity_list[[4]]$aim2_df_no3y,
  list_DVs = "bmi_obesity",
  list_IVs = c("nt_y_stq__sleep_002_phone_outside_room"),
  list_covars = covariates_list_aim2_2y$bmi_obesity,
  random_eff = random_effects_s,
  binary_DV = TRUE,
  ext = "eTable7_covar_weight_non_imputed_DV_4",
  CI_level = (1-0.05)
)
run_write_models(
  data = aim_sleep_list[[4]]$aim2_df_no3y,
  list_DVs = "lack_sleep",
  list_IVs = c("nt_y_stq__sleep_002_phone_outside_room"),
  list_covars = covariates_list_aim2$lack_sleep,
  random_eff = random_effects_s,
  binary_DV = TRUE,
  ext = "eTable7_covar_weight_non_imputed_DV_4",
  CI_level = (1-0.05)
)


## Imputation 5----
# eTable 2----
run_write_models(
  data = aim_dep_list[[5]]$aim1_df,
  list_DVs = "depression_dx_y",
  list_covars = covariates_list_aim1$depression_dx_y,
  random_eff = random_effects_s,
  list_IVs = "smartphone_ownership",
  binary_DV = TRUE,
  # ext = "eTable2",
  ext = "eTable2_covar_weight_non_imputed_DV_5",
  CI_level = (1-0.05)
)


run_write_models(
  data = aim_obesity_list[[5]]$aim1_df,
  list_DVs = "bmi_obesity",
  list_covars = covariates_list_aim2_2y$bmi_obesity,
  random_eff = random_effects_s,
  list_IVs = "smartphone_ownership",
  binary_DV = TRUE,
  # ext = "eTable2_covary_obesity2y",
  ext = "eTable2_covary_obesity2y_covar_weight_non_imputed_DV_5",
  CI_level = (1-0.05)
)


run_write_models(
  data = aim_sleep_list[[5]]$aim1_df,
  list_DVs = "lack_sleep",
  list_covars = covariates_list_aim1$lack_sleep,
  random_eff = random_effects_s,
  list_IVs = "smartphone_ownership",
  binary_DV = TRUE,
  # ext = "eTable2",
  ext = "eTable2_covar_weight_non_imputed_DV_5",
  CI_level = (1-0.05)
)

# eTable 3 and 4----
run_write_models(
  data = aim_dep_list[[5]]$aim2_df_no3y,
  list_DVs = "depression_dx_y",
  list_IVs = c("scale(schoolyear_total_smartph_wsum)", "scale(schoolyear_total_smartph_wsum)*sex_br"),
  list_covars = covariates_list_aim2$depression_dx_y,
  random_eff = random_effects_s,
  binary_DV = TRUE,
  # ext = "eTable3_4",
  ext = "eTable3_4_covar_weight_non_imputed_DV_5",
  CI_level = (1-0.05)
)

# baseline from 2
run_write_models(
  data = aim_obesity_list[[5]]$aim2_df_no3y,
  list_DVs = "bmi_obesity",
  list_IVs = c("scale(schoolyear_total_smartph_wsum)", "scale(schoolyear_total_smartph_wsum)*sex_br"),
  list_covars = covariates_list_aim2_2y$bmi_obesity,
  random_eff = random_effects_s,
  binary_DV = TRUE,
  # ext = "eTable3_4",
  ext = "eTable3_4_covar_weight_non_imputed_DV_5",
  CI_level = (1-0.05)
)


run_write_models(
  data = aim_sleep_list[[5]]$aim2_df_no3y,
  list_DVs = "lack_sleep",
  list_IVs = c("scale(schoolyear_total_smartph_wsum)", "scale(schoolyear_total_smartph_wsum)*sex_br"),
  list_covars = covariates_list_aim2$lack_sleep,
  random_eff = random_effects_s,
  binary_DV = TRUE,
  # ext = "eTable3_4",
  ext = "eTable3_4_covar_weight_non_imputed_DV_5",
  CI_level = (1-0.05)
)

# eTable 6 smartphone time categories----
run_write_models(
  data = aim_dep_list[[5]]$aim2_df_no3y,
  list_DVs = "depression_dx_y",
  list_IVs = "schoolyear_total_smartph_wsum_wmean_perday_cat",
  list_covars = covariates_list_aim2$depression_dx_y,
  random_eff = random_effects_s,
  binary_DV = TRUE,
  ext = "eTable6_covar_weight_non_imputed_DV_5",
  CI_level = (1-0.05)
)

run_write_models(
  data = aim_obesity_list[[5]]$aim2_df_no3y,
  list_DVs = "bmi_obesity",
  list_IVs = "schoolyear_total_smartph_wsum_wmean_perday_cat",
  list_covars = covariates_list_aim2_2y$bmi_obesity,
  random_eff = random_effects_s,
  binary_DV = TRUE,
  ext = "eTable6_covar_weight_non_imputed_DV_5",
  CI_level = (1-0.05)
)


run_write_models(
  data = aim_sleep_list[[5]]$aim2_df_no3y,
  list_DVs = "lack_sleep",
  list_IVs = "schoolyear_total_smartph_wsum_wmean_perday_cat",
  list_covars = covariates_list_aim2$lack_sleep,
  random_eff = random_effects_s,
  binary_DV = TRUE,
  ext = "eTable6_covar_weight_non_imputed_DV_5",
  CI_level = (1-0.05)
)


# eTable 7----
run_write_models(
  data = aim_dep_list[[5]]$aim2_df_no3y,
  list_DVs = "depression_dx_y",
  list_IVs = c("nt_y_stq__sleep_002_phone_outside_room"),
  list_covars = covariates_list_aim2$depression_dx_y,
  random_eff = random_effects_s,
  binary_DV = TRUE,
  ext = "eTable7_covar_weight_non_imputed_DV_5",
  CI_level = (1-0.05)
)


run_write_models(
  data = aim_obesity_list[[5]]$aim2_df_no3y,
  list_DVs = "bmi_obesity",
  list_IVs = c("nt_y_stq__sleep_002_phone_outside_room"),
  list_covars = covariates_list_aim2_2y$bmi_obesity,
  random_eff = random_effects_s,
  binary_DV = TRUE,
  ext = "eTable7_covar_weight_non_imputed_DV_5",
  CI_level = (1-0.05)
)

run_write_models(
  data = aim_sleep_list[[5]]$aim2_df_no3y,
  list_DVs = "lack_sleep",
  list_IVs = c("nt_y_stq__sleep_002_phone_outside_room"),
  list_covars = covariates_list_aim2$lack_sleep,
  random_eff = random_effects_s,
  binary_DV = TRUE,
  ext = "eTable7_covar_weight_non_imputed_DV_5",
  CI_level = (1-0.05)
)

# POOLED RESULTS----
## eTable 2----
input_etable2 <- data.frame(
  outcome = c("depression_dx_y", "bmi_obesity", "lack_sleep"),
  
  OR_1 = c(1.45, 1.02, 1.29),
  L_1  = c(0.98, 0.71, 1.03),
  U_1  = c(2.14, 1.46, 1.62),
  
  OR_2 = c(1.46, 0.92, 1.30),
  L_2  = c(0.99, 0.64, 1.03),
  U_2  = c(2.16, 1.32, 1.63),
  
  OR_3 = c(1.48, 0.98, 1.29),
  L_3  = c(1.00, 0.68, 1.03),
  U_3  = c(2.19, 1.41, 1.63),
  
  OR_4 = c(1.46, 0.92, 1.30),
  L_4  = c(0.99, 0.65, 1.03),
  U_4  = c(2.16, 1.32, 1.63),
  
  OR_5 = c(1.46, 1.00, 1.30),
  L_5  = c(0.99, 0.70, 1.03),
  U_5  = c(2.16, 1.44, 1.63)
)

pool_MI_from_table(input_etable2) %>% View()

## eTable 3----
input_etable3 <- data.frame(
  outcome = c("depression_dx_y", "bmi_obesity", "lack_sleep"),
  
  OR_1 = c(1.23, 1.37, 1.31),
  L_1  = c(1.01, 1.11, 1.14),
  U_1  = c(1.50, 1.70, 1.50),
  
  OR_2 = c(1.23, 1.48, 1.31),
  L_2  = c(1.00, 1.20, 1.14),
  U_2  = c(1.50, 1.81, 1.50),
  
  OR_3 = c(1.22, 1.48, 1.31),
  L_3  = c(1.00, 1.20, 1.14),
  U_3  = c(1.49, 1.83, 1.50),
  
  OR_4 = c(1.23, 1.46, 1.31),
  L_4  = c(1.01, 1.19, 1.14),
  U_4  = c(1.50, 1.80, 1.51),
  
  OR_5 = c(1.23, 1.49, 1.31),
  L_5  = c(1.01, 1.20, 1.14),
  U_5  = c(1.50, 1.84, 1.50)
)

pool_MI_from_table(input_etable3) %>% View()

## eTable 6.1----
input_table_6A <- data.frame(
  outcome = c("depression_dx_y", "bmi_obesity", "lack_sleep"),
  
  OR_1 = c(1.48, 1.17, 1.65),
  L_1  = c(0.93, 0.71, 1.22),
  U_1  = c(2.37, 1.93, 2.23),
  
  OR_2 = c(1.47, 1.20, 1.66),
  L_2  = c(0.92, 0.73, 1.23),
  U_2  = c(2.35, 1.97, 2.24),
  
  OR_3 = c(1.49, 1.22, 1.66),
  L_3  = c(0.93, 0.73, 1.23),
  U_3  = c(2.38, 2.02, 2.24),
  
  OR_4 = c(1.49, 1.12, 1.66),
  L_4  = c(0.93, 0.68, 1.23),
  U_4  = c(2.38, 1.84, 2.24),
  
  OR_5 = c(1.48, 1.26, 1.67),
  L_5  = c(0.92, 0.76, 1.23),
  U_5  = c(2.36, 2.08, 2.25)
)

pool_MI_from_table(input_table_6A) %>% View()

## eTable 6.2----
input_table_6B <- data.frame(
  outcome = c("depression_dx_y", "bmi_obesity", "lack_sleep"),
  
  OR_1 = c(2.30, 2.84, 2.08),
  L_1  = c(1.17, 1.45, 1.33),
  U_1  = c(4.49, 5.56, 3.26),
  
  OR_2 = c(2.29, 3.31, 2.08),
  L_2  = c(1.17, 1.70, 1.33),
  U_2  = c(4.47, 6.46, 3.27),
  
  OR_3 = c(2.24, 3.13, 2.08),
  L_3  = c(1.15, 1.57, 1.32),
  U_3  = c(4.38, 6.24, 3.26),
  
  OR_4 = c(2.26, 3.12, 2.09),
  L_4  = c(1.16, 1.59, 1.33),
  U_4  = c(4.42, 6.11, 3.28),
  
  OR_5 = c(2.29, 3.50, 2.07),
  L_5  = c(1.17, 1.78, 1.32),
  U_5  = c(4.47, 6.88, 3.26)
)

pool_MI_from_table(input_table_6B) %>% View()

## eTable 7----
input_table_7 <- data.frame(
    outcome = c("depression_dx_y", "bmi_obesity", "lack_sleep"),
    
    OR_1 = c(0.87, 0.82, 0.66),
    L_1  = c(0.52, 0.49, 0.48),
    U_1  = c(1.45, 1.37, 0.90),
    
    OR_2 = c(0.87, 0.84, 0.66),
    L_2  = c(0.53, 0.50, 0.48),
    U_2  = c(1.45, 1.41, 0.90),
    
    OR_3 = c(0.87, 0.92, 0.66),
    L_3  = c(0.52, 0.54, 0.48),
    U_3  = c(1.44, 1.57, 0.90),
    
    OR_4 = c(0.87, 0.83, 0.66),
    L_4  = c(0.53, 0.50, 0.48),
    U_4  = c(1.45, 1.40, 0.90),
    
    OR_5 = c(0.87, 0.87, 0.66),
    L_5  = c(0.52, 0.52, 0.48),
    U_5  = c(1.44, 1.47, 0.90)
  )

pool_MI_from_table(input_table_7) %>% View()

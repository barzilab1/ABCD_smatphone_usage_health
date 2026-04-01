
library(dplyr)
source("analysis/analysis_utility_fun.R")

aim_dep_list <- readRDS("data/aim_dep_list_fully_imputed.rds")
aim_obesity_list <- readRDS("data/aim_obesity_list_fully_imputed.rds")
aim_sleep_list <- readRDS("data/aim_sleep_list_fully_imputed.rds")

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
  ext = "eTable2_covar_weight_fully_imputed_1",
  CI_level = (1-0.05)
)


run_write_models(
  data = aim_obesity_list[[1]]$aim1_df,
  list_DVs = "bmi_obesity",
  list_covars = covariates_list_aim2_2y$bmi_obesity,
  random_eff = random_effects_s,
  list_IVs = "smartphone_ownership",
  binary_DV = TRUE,
  # ext = "eTable2_covary_obesity2y",
  ext = "eTable2_covary_obesity2y_covar_weight_fully_imputed_1",
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
  ext = "eTable2_covar_weight_fully_imputed_1",
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
  ext = "eTable3_4_covar_weight_fully_imputed_1",
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
  ext = "eTable3_4_covar_weight_fully_imputed_1",
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
  ext = "eTable3_4_covar_weight_fully_imputed_1",
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
  ext = "eTable6_covar_weight_fully_imputed_1",
  CI_level = (1-0.05)
)

run_write_models(
  data = aim_obesity_list[[1]]$aim2_df_no3y,
  list_DVs = "bmi_obesity",
  list_IVs = "schoolyear_total_smartph_wsum_wmean_perday_cat",
  list_covars = covariates_list_aim2_2y$bmi_obesity,
  random_eff = random_effects_s,
  binary_DV = TRUE,
  ext = "eTable6_covar_weight_fully_imputed_1",
  CI_level = (1-0.05)
)

run_write_models(
  data = aim_sleep_list[[1]]$aim2_df_no3y,
  list_DVs = "lack_sleep",
  list_IVs = "schoolyear_total_smartph_wsum_wmean_perday_cat",
  list_covars = covariates_list_aim2$lack_sleep,
  random_eff = random_effects_s,
  binary_DV = TRUE,
  ext = "eTable6_covar_weight_fully_imputed_1",
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
  ext = "eTable7_covar_weight_fully_imputed_1",
  CI_level = (1-0.05)
)

run_write_models(
  data = aim_obesity_list[[1]]$aim2_df_no3y,
  list_DVs = "bmi_obesity",
  list_IVs = c("nt_y_stq__sleep_002_phone_outside_room"),
  list_covars = covariates_list_aim2_2y$bmi_obesity,
  random_eff = random_effects_s,
  binary_DV = TRUE,
  ext = "eTable7_covar_weight_fully_imputed_1",
  CI_level = (1-0.05)
)


run_write_models(
  data = aim_sleep_list[[1]]$aim2_df_no3y,
  list_DVs = "lack_sleep",
  list_IVs = c("nt_y_stq__sleep_002_phone_outside_room"),
  list_covars = covariates_list_aim2$lack_sleep,
  random_eff = random_effects_s,
  binary_DV = TRUE,
  ext = "eTable7_covar_weight_fully_imputed_1",
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
  ext = "eTable2_covar_weight_fully_imputed_2",
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
  ext = "eTable2_covary_obesity2y_covar_weight_fully_imputed_2",
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
  ext = "eTable2_covar_weight_fully_imputed_2",
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
  ext = "eTable3_4_covar_weight_fully_imputed_2",
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
  ext = "eTable3_4_covar_weight_fully_imputed_2",
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
  ext = "eTable3_4_covar_weight_fully_imputed_2",
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
  ext = "eTable6_covar_weight_fully_imputed_2",
  CI_level = (1-0.05)
)

run_write_models(
  data = aim_obesity_list[[2]]$aim2_df_no3y,
  list_DVs = "bmi_obesity",
  list_IVs = "schoolyear_total_smartph_wsum_wmean_perday_cat",
  list_covars = covariates_list_aim2_2y$bmi_obesity,
  random_eff = random_effects_s,
  binary_DV = TRUE,
  ext = "eTable6_covar_weight_fully_imputed_2",
  CI_level = (1-0.05)
)


run_write_models(
  data = aim_sleep_list[[2]]$aim2_df_no3y,
  list_DVs = "lack_sleep",
  list_IVs = "schoolyear_total_smartph_wsum_wmean_perday_cat",
  list_covars = covariates_list_aim2$lack_sleep,
  random_eff = random_effects_s,
  binary_DV = TRUE,
  ext = "eTable6_covar_weight_fully_imputed_2",
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
  ext = "eTable7_covar_weight_fully_imputed_2",
  CI_level = (1-0.05)
)

run_write_models(
  data = aim_obesity_list[[2]]$aim2_df_no3y,
  list_DVs = "bmi_obesity",
  list_IVs = c("nt_y_stq__sleep_002_phone_outside_room"),
  list_covars = covariates_list_aim2_2y$bmi_obesity,
  random_eff = random_effects_s,
  binary_DV = TRUE,
  ext = "eTable7_covar_weight_fully_imputed_2",
  CI_level = (1-0.05)
)

run_write_models(
  data = aim_sleep_list[[2]]$aim2_df_no3y,
  list_DVs = "lack_sleep",
  list_IVs = c("nt_y_stq__sleep_002_phone_outside_room"),
  list_covars = covariates_list_aim2$lack_sleep,
  random_eff = random_effects_s,
  binary_DV = TRUE,
  ext = "eTable7_covar_weight_fully_imputed_2",
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
  ext = "eTable2_covar_weight_fully_imputed_3",
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
  ext = "eTable2_covary_obesity2y_covar_weight_fully_imputed_3",
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
  ext = "eTable2_covar_weight_fully_imputed_3",
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
  ext = "eTable3_4_covar_weight_fully_imputed_3",
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
  ext = "eTable3_4_covar_weight_fully_imputed_3",
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
  ext = "eTable3_4_covar_weight_fully_imputed_3",
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
  ext = "eTable6_covar_weight_fully_imputed_3",
  CI_level = (1-0.05)
)

run_write_models(
  data = aim_obesity_list[[3]]$aim2_df_no3y,
  list_DVs = "bmi_obesity",
  list_IVs = "schoolyear_total_smartph_wsum_wmean_perday_cat",
  list_covars = covariates_list_aim2_2y$bmi_obesity,
  random_eff = random_effects_s,
  binary_DV = TRUE,
  ext = "eTable6_covar_weight_fully_imputed_3",
  CI_level = (1-0.05)
)


run_write_models(
  data = aim_sleep_list[[3]]$aim2_df_no3y,
  list_DVs = "lack_sleep",
  list_IVs = "schoolyear_total_smartph_wsum_wmean_perday_cat",
  list_covars = covariates_list_aim2$lack_sleep,
  random_eff = random_effects_s,
  binary_DV = TRUE,
  ext = "eTable6_covar_weight_fully_imputed_3",
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
  ext = "eTable7_covar_weight_fully_imputed_3",
  CI_level = (1-0.05)
)

run_write_models(
  data = aim_obesity_list[[3]]$aim2_df_no3y,
  list_DVs = "bmi_obesity",
  list_IVs = c("nt_y_stq__sleep_002_phone_outside_room"),
  list_covars = covariates_list_aim2_2y$bmi_obesity,
  random_eff = random_effects_s,
  binary_DV = TRUE,
  ext = "eTable7_covar_weight_fully_imputed_3",
  CI_level = (1-0.05)
)

run_write_models(
  data = aim_sleep_list[[3]]$aim2_df_no3y,
  list_DVs = "lack_sleep",
  list_IVs = c("nt_y_stq__sleep_002_phone_outside_room"),
  list_covars = covariates_list_aim2$lack_sleep,
  random_eff = random_effects_s,
  binary_DV = TRUE,
  ext = "eTable7_covar_weight_fully_imputed_3",
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
  ext = "eTable2_covar_weight_fully_imputed_4",
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
  ext = "eTable2_covary_obesity2y_covar_weight_fully_imputed_4",
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
  ext = "eTable2_covar_weight_fully_imputed_4",
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
  ext = "eTable3_4_covar_weight_fully_imputed_4",
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
  ext = "eTable3_4_covar_weight_fully_imputed_4",
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
  ext = "eTable3_4_covar_weight_fully_imputed_4",
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
  ext = "eTable6_covar_weight_fully_imputed_4",
  CI_level = (1-0.05)
)

run_write_models(
  data = aim_obesity_list[[4]]$aim2_df_no3y,
  list_DVs = "bmi_obesity",
  list_IVs = "schoolyear_total_smartph_wsum_wmean_perday_cat",
  list_covars = covariates_list_aim2_2y$bmi_obesity,
  random_eff = random_effects_s,
  binary_DV = TRUE,
  ext = "eTable6_covar_weight_fully_imputed_4",
  CI_level = (1-0.05)
)

run_write_models(
  data = aim_sleep_list[[4]]$aim2_df_no3y,
  list_DVs = "lack_sleep",
  list_IVs = "schoolyear_total_smartph_wsum_wmean_perday_cat",
  list_covars = covariates_list_aim2$lack_sleep,
  random_eff = random_effects_s,
  binary_DV = TRUE,
  ext = "eTable6_covar_weight_fully_imputed_4",
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
  ext = "eTable7_covar_weight_fully_imputed_4",
  CI_level = (1-0.05)
)

run_write_models(
  data = aim_obesity_list[[4]]$aim2_df_no3y,
  list_DVs = "bmi_obesity",
  list_IVs = c("nt_y_stq__sleep_002_phone_outside_room"),
  list_covars = covariates_list_aim2_2y$bmi_obesity,
  random_eff = random_effects_s,
  binary_DV = TRUE,
  ext = "eTable7_covar_weight_fully_imputed_4",
  CI_level = (1-0.05)
)
run_write_models(
  data = aim_sleep_list[[4]]$aim2_df_no3y,
  list_DVs = "lack_sleep",
  list_IVs = c("nt_y_stq__sleep_002_phone_outside_room"),
  list_covars = covariates_list_aim2$lack_sleep,
  random_eff = random_effects_s,
  binary_DV = TRUE,
  ext = "eTable7_covar_weight_fully_imputed_4",
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
  ext = "eTable2_covar_weight_fully_imputed_5",
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
  ext = "eTable2_covary_obesity2y_covar_weight_fully_imputed_5",
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
  ext = "eTable2_covar_weight_fully_imputed_5",
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
  ext = "eTable3_4_covar_weight_fully_imputed_5",
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
  ext = "eTable3_4_covar_weight_fully_imputed_5",
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
  ext = "eTable3_4_covar_weight_fully_imputed_5",
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
  ext = "eTable6_covar_weight_fully_imputed_5",
  CI_level = (1-0.05)
)

run_write_models(
  data = aim_obesity_list[[5]]$aim2_df_no3y,
  list_DVs = "bmi_obesity",
  list_IVs = "schoolyear_total_smartph_wsum_wmean_perday_cat",
  list_covars = covariates_list_aim2_2y$bmi_obesity,
  random_eff = random_effects_s,
  binary_DV = TRUE,
  ext = "eTable6_covar_weight_fully_imputed_5",
  CI_level = (1-0.05)
)


run_write_models(
  data = aim_sleep_list[[5]]$aim2_df_no3y,
  list_DVs = "lack_sleep",
  list_IVs = "schoolyear_total_smartph_wsum_wmean_perday_cat",
  list_covars = covariates_list_aim2$lack_sleep,
  random_eff = random_effects_s,
  binary_DV = TRUE,
  ext = "eTable6_covar_weight_fully_imputed_5",
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
  ext = "eTable7_covar_weight_fully_imputed_5",
  CI_level = (1-0.05)
)


run_write_models(
  data = aim_obesity_list[[5]]$aim2_df_no3y,
  list_DVs = "bmi_obesity",
  list_IVs = c("nt_y_stq__sleep_002_phone_outside_room"),
  list_covars = covariates_list_aim2_2y$bmi_obesity,
  random_eff = random_effects_s,
  binary_DV = TRUE,
  ext = "eTable7_covar_weight_fully_imputed_5",
  CI_level = (1-0.05)
)

run_write_models(
  data = aim_sleep_list[[5]]$aim2_df_no3y,
  list_DVs = "lack_sleep",
  list_IVs = c("nt_y_stq__sleep_002_phone_outside_room"),
  list_covars = covariates_list_aim2$lack_sleep,
  random_eff = random_effects_s,
  binary_DV = TRUE,
  ext = "eTable7_covar_weight_fully_imputed_5",
  CI_level = (1-0.05)
)


# POOLED RESULTS----
## eTable 2----
input_eTable2_full <- data.frame(
  
  outcome = c("depression_dx_y", "bmi_obesity", "lack_sleep"),
  
    OR_1 = c(1.46, 1.02, 1.30),
    L_1  = c(1.02, 0.73, 1.03),
    U_1  = c(2.10, 1.44, 1.63),
    
    OR_2 = c(1.53, 0.93, 1.28),
    L_2  = c(1.05, 0.66, 1.02),
    U_2  = c(2.22, 1.31, 1.61),
    
    OR_3 = c(1.57, 1.07, 1.29),
    L_3  = c(1.08, 0.76, 1.02),
    U_3  = c(2.29, 1.52, 1.62),
    
    OR_4 = c(1.42, 1.06, 1.29),
    L_4  = c(0.99, 0.75, 1.03),
    U_4  = c(2.04, 1.48, 1.63),
    
    OR_5 = c(1.45, 1.05, 1.30),
    L_5  = c(1.01, 0.75, 1.04),
    U_5  = c(2.09, 1.46, 1.63)
  )

pool_MI_from_table(input_eTable2_full) %>% View()

## eTable 3----
input_eTable3_full <- data.frame(
  
  outcome = c("depression_dx_y", "bmi_obesity", "lack_sleep"),
  
    OR_1 = c(1.21, 1.48, 1.28),
    L_1  = c(1.01, 1.23, 1.12),
    U_1  = c(1.45, 1.79, 1.47),
    
    OR_2 = c(1.19, 1.52, 1.31),
    L_2  = c(0.99, 1.27, 1.14),
    U_2  = c(1.43, 1.82, 1.50),
    
    OR_3 = c(1.26, 1.56, 1.29),
    L_3  = c(1.05, 1.30, 1.13),
    U_3  = c(1.50, 1.88, 1.48),
    
    OR_4 = c(1.22, 1.49, 1.29),
    L_4  = c(1.02, 1.25, 1.13),
    U_4  = c(1.46, 1.78, 1.47),
    
    OR_5 = c(1.20, 1.51, 1.31),
    L_5  = c(1.00, 1.26, 1.15),
    U_5  = c(1.44, 1.82, 1.50)
  )

pool_MI_from_table(input_eTable3_full) %>% View()

## eTable 6----
input_eTable6A_full <- data.frame(
  
  outcome = c("depression_dx_y", "bmi_obesity", "lack_sleep"),
    
    OR_1 = c(1.47, 1.12, 1.63),
    L_1  = c(0.95, 0.71, 1.22),
    U_1  = c(2.26, 1.77, 2.18),
    
    OR_2 = c(1.46, 1.29, 1.58),
    L_2  = c(0.95, 0.83, 1.18),
    U_2  = c(2.26, 2.02, 2.12),
    
    OR_3 = c(1.53, 1.28, 1.69),
    L_3  = c(0.98, 0.81, 1.26),
    U_3  = c(2.37, 2.03, 2.26),
    
    OR_4 = c(1.42, 1.22, 1.63),
    L_4  = c(0.91, 0.79, 1.22),
    U_4  = c(2.20, 1.89, 2.19),
    
    OR_5 = c(1.57, 1.17, 1.67),
    L_5  = c(1.02, 0.75, 1.25),
    U_5  = c(2.42, 1.83, 2.24)
  )

pool_MI_from_table(input_eTable6A_full) %>% View()

input_eTable6B_full <- data.frame(
  
  outcome = c("depression_dx_y", "bmi_obesity", "lack_sleep"),
    
    OR_1 = c(1.98, 3.51, 1.99),
    L_1  = c(1.08, 1.91, 1.29),
    U_1  = c(3.61, 6.42, 3.09),
    
    OR_2 = c(1.77, 3.39, 2.07),
    L_2  = c(0.96, 1.86, 1.34),
    U_2  = c(3.26, 6.19, 3.22),
    
    OR_3 = c(2.55, 4.02, 2.05),
    L_3  = c(1.42, 2.18, 1.31),
    U_3  = c(4.57, 7.41, 3.18),
    
    OR_4 = c(2.29, 3.22, 1.95),
    L_4  = c(1.28, 1.78, 1.26),
    U_4  = c(4.12, 5.79, 3.02),
    
    OR_5 = c(1.97, 3.72, 2.02),
    L_5  = c(1.07, 2.05, 1.30),
    U_5  = c(3.62, 6.72, 3.15)
)

pool_MI_from_table(input_eTable6B_full) %>% View()

## eTable 7----
input_eTable7_full <- data.frame(
  
  outcome = c("depression_dx_y", "bmi_obesity", "lack_sleep"),
    
    OR_1 = c(0.83, 0.89, 0.64),
    L_1  = c(0.52, 0.55, 0.47),
    U_1  = c(1.31, 1.45, 0.86),
    
    OR_2 = c(0.82, 0.96, 0.62),
    L_2  = c(0.52, 0.59, 0.46),
    U_2  = c(1.31, 1.55, 0.85),
    
    OR_3 = c(0.75, 0.96, 0.61),
    L_3  = c(0.47, 0.59, 0.45),
    U_3  = c(1.21, 1.58, 0.83),
    
    OR_4 = c(0.83, 0.84, 0.63),
    L_4  = c(0.52, 0.52, 0.46),
    U_4  = c(1.32, 1.35, 0.85),
    
    OR_5 = c(0.81, 0.89, 0.64),
    L_5  = c(0.51, 0.55, 0.47),
    U_5  = c(1.30, 1.43, 0.87)
)

pool_MI_from_table(input_eTable7_full) %>% View()




source("analysis/analysis_utility_fun_survey_weight.R")
source("analysis/analysis_utility_fun.R")

aim_dep_list_non_imp_DV <- readRDS("data/aim_dep_list_non_imp_DV.rds")
aim_obesity_list_non_imp_DV <- readRDS("data/aim_obesity_list_non_imp_DV.rds")
aim_sleep_list_non_imp_DV <- readRDS("data/aim_sleep_list_non_imp_DV.rds")

covariates_list_aim1 <- readRDS("data/covariates_list_aim1.rds")
covariates_list_aim1_sens <- readRDS("data/covariates_list_aim1_sens.rds")
covariates_list_aim2 <- readRDS("data/covariates_list_aim2.rds")
covariates_list_aim2_2y <- readRDS("data/covariates_list_aim2_2y.rds")

# IMPUTATION 1----
## eTable 2----
etable2_design_dep_imp1 <- svydesign(
  ids = ~site_br + family_id_br,
  weights = ~abcd_pps_weight,
  data = aim_dep_list_non_imp_DV[[1]]$aim1_df,
  nest = TRUE
)

etable2_design_obesity_imp1 <- svydesign(
  ids = ~site_br + family_id_br,
  weights = ~abcd_pps_weight,
  data = aim_obesity_list_non_imp_DV[[1]]$aim1_df,
  nest = TRUE
)

etable2_design_sleep_imp1 <- svydesign(
  ids = ~site_br + family_id_br,
  weights = ~abcd_pps_weight,
  data = aim_sleep_list_non_imp_DV[[1]]$aim1_df,
  nest = TRUE
)

run_write_models_svy(
  data = aim_dep_list_non_imp_DV[[1]]$aim1_df,
  design = etable2_design_dep_imp1,
  list_DVs = c("depression_dx_y"),
  list_covars = covariates_list_aim1$depression_dx_y[3],
  list_IVs = c("smartphone_ownership"),
  binary_DV = TRUE,
  ext = "eTable2_survey_weight_non_imp_DV_imp1"
)

run_write_models_svy(
  data = aim_obesity_list_non_imp_DV[[1]]$aim1_df,
  design = etable2_design_obesity_imp1,
  list_DVs = c("bmi_obesity"),
  list_covars = covariates_list_aim2_2y$bmi_obesity[3],
  list_IVs = c("smartphone_ownership"),
  binary_DV = TRUE,
  ext = "eTable2_covary_obesity2y_survey_weight_non_imp_DV_imp1"
)

run_write_models_svy(
  data = aim_sleep_list_non_imp_DV[[1]]$aim1_df,
  design = etable2_design_sleep_imp1,
  list_DVs = c("lack_sleep"),
  list_covars = covariates_list_aim1$lack_sleep[3],
  list_IVs = c("smartphone_ownership"),
  binary_DV = TRUE,
  ext = "eTable2_survey_weight_non_imp_DV_imp1"
)


## eTable 3 and 4----
etable34_design_dep_imp1 <- svydesign(
  ids = ~site_br + family_id_br,
  weights = ~abcd_pps_weight,
  data = aim_dep_list_non_imp_DV[[1]]$aim2_df_no3y,
  nest = TRUE
)

etable34_design_obesity_imp1 <- svydesign(
  ids = ~site_br + family_id_br,
  weights = ~abcd_pps_weight,
  data = aim_obesity_list_non_imp_DV[[1]]$aim2_df_no3y,
  nest = TRUE
)

etable34_design_sleep_imp1 <- svydesign(
  ids = ~site_br + family_id_br,
  weights = ~abcd_pps_weight,
  data = aim_sleep_list_non_imp_DV[[1]]$aim2_df_no3y,
  nest = TRUE
)


run_write_models_svy(
  data = aim_dep_list_non_imp_DV[[1]]$aim2_df_no3y,
  design = etable34_design_dep_imp1,
  list_DVs = c("depression_dx_y"),
  list_covars = covariates_list_aim2$depression_dx_y[3],
  list_IVs = c("scale(schoolyear_total_smartph_wsum)"),
  binary_DV = TRUE,
  ext = "eTable34_survey_weight_non_imp_DV_imp1"
)

### baseline from 2
run_write_models_svy(
  data = aim_obesity_list_non_imp_DV[[1]]$aim2_df_no3y,
  design = etable34_design_obesity_imp1,
  list_DVs = c("bmi_obesity"),
  list_covars = covariates_list_aim2_2y$bmi_obesity[3],
  list_IVs = c("scale(schoolyear_total_smartph_wsum)"),
  binary_DV = TRUE,
  ext = "eTable34_survey_weight_non_imp_DV_imp1"
)

run_write_models_svy(
  data = aim_sleep_list_non_imp_DV[[1]]$aim2_df_no3y,
  design = etable34_design_sleep_imp1,
  list_DVs = c("lack_sleep"),
  list_covars = covariates_list_aim2$lack_sleep[3],
  list_IVs = c("scale(schoolyear_total_smartph_wsum)"),
  binary_DV = TRUE,
  ext = "eTable34_survey_weight_non_imp_DV_imp1"
)


## eTable 6 smartphone time categories----
run_write_models_svy(
  data = aim_dep_list_non_imp_DV[[1]]$aim2_df_no3y,
  design = etable34_design_dep_imp1,
  list_DVs = c("depression_dx_y"),
  list_covars = covariates_list_aim2$depression_dx_y[3],
  list_IVs = c("schoolyear_total_smartph_wsum_wmean_perday_cat"),
  binary_DV = TRUE,
  ext = "eTable6_survey_weight_non_imp_DV_imp1"
)


run_write_models_svy(
  data = aim_obesity_list_non_imp_DV[[1]]$aim2_df_no3y,
  design = etable34_design_obesity_imp1,
  list_DVs = c("bmi_obesity"),
  list_covars = covariates_list_aim2_2y$bmi_obesity[3],
  list_IVs = c("schoolyear_total_smartph_wsum_wmean_perday_cat"),
  binary_DV = TRUE,
  ext = "eTable6_survey_weight_non_imp_DV_imp1"
)

run_write_models_svy(
  data = aim_sleep_list_non_imp_DV[[1]]$aim2_df_no3y,
  design = etable34_design_sleep_imp1,
  list_DVs = c("lack_sleep"),
  list_covars = covariates_list_aim2$lack_sleep[3],
  list_IVs = c("schoolyear_total_smartph_wsum_wmean_perday_cat"),
  binary_DV = TRUE,
  ext = "eTable6_survey_weight_non_imp_DV_imp1"
)


## eTable 7----
run_write_models_svy(
  data = aim_dep_list_non_imp_DV[[1]]$aim2_df_no3y,
  design = etable34_design_dep_imp1,
  list_DVs = c("depression_dx_y"),
  list_covars = covariates_list_aim2$depression_dx_y[3],
  list_IVs = c("nt_y_stq__sleep_002_phone_outside_room"),
  binary_DV = TRUE,
  ext = "eTable7_survey_weight_non_imp_DV_imp1"
)

run_write_models_svy(
  data = aim_obesity_list_non_imp_DV[[1]]$aim2_df_no3y,
  design = etable34_design_obesity_imp1,
  list_DVs = c("bmi_obesity"),
  list_covars = covariates_list_aim2_2y$bmi_obesity[3],
  list_IVs = c("nt_y_stq__sleep_002_phone_outside_room"),
  binary_DV = TRUE,
  ext = "eTable7_survey_weight_non_imp_DV_imp1"
)

run_write_models_svy(
  data = aim_sleep_list_non_imp_DV[[1]]$aim2_df_no3y,
  design = etable34_design_sleep_imp1,
  list_DVs = c("lack_sleep"),
  list_covars = covariates_list_aim2$lack_sleep[3],
  list_IVs = c("nt_y_stq__sleep_002_phone_outside_room"),
  binary_DV = TRUE,
  ext = "eTable7_survey_weight_non_imp_DV_imp1"
)

# IMPUTATION 2----
## eTable 2----
etable2_design_dep_imp2 <- svydesign(
  ids = ~site_br + family_id_br,
  weights = ~abcd_pps_weight,
  data = aim_dep_list_non_imp_DV[[2]]$aim1_df,
  nest = TRUE
)

etable2_design_obesity_imp2 <- svydesign(
  ids = ~site_br + family_id_br,
  weights = ~abcd_pps_weight,
  data = aim_obesity_list_non_imp_DV[[2]]$aim1_df,
  nest = TRUE
)

etable2_design_sleep_imp2 <- svydesign(
  ids = ~site_br + family_id_br,
  weights = ~abcd_pps_weight,
  data = aim_sleep_list_non_imp_DV[[2]]$aim1_df,
  nest = TRUE
)

run_write_models_svy(
  data = aim_dep_list_non_imp_DV[[2]]$aim1_df,
  design = etable2_design_dep_imp2,
  list_DVs = c("depression_dx_y"),
  list_covars = covariates_list_aim1$depression_dx_y[3],
  list_IVs = c("smartphone_ownership"),
  binary_DV = TRUE,
  ext = "eTable2_survey_weight_non_imp_DV_imp2"
)

run_write_models_svy(
  data = aim_obesity_list_non_imp_DV[[2]]$aim1_df,
  design = etable2_design_obesity_imp2,
  list_DVs = c("bmi_obesity"),
  list_covars = covariates_list_aim2_2y$bmi_obesity[3],
  list_IVs = c("smartphone_ownership"),
  binary_DV = TRUE,
  ext = "eTable2_covary_obesity2y_survey_weight_non_imp_DV_imp2"
)

run_write_models_svy(
  data = aim_sleep_list_non_imp_DV[[2]]$aim1_df,
  design = etable2_design_sleep_imp2,
  list_DVs = c("lack_sleep"),
  list_covars = covariates_list_aim1$lack_sleep[3],
  list_IVs = c("smartphone_ownership"),
  binary_DV = TRUE,
  ext = "eTable2_survey_weight_non_imp_DV_imp2"
)


## eTable 3 and 4----
etable34_design_dep_imp2 <- svydesign(
  ids = ~site_br + family_id_br,
  weights = ~abcd_pps_weight,
  data = aim_dep_list_non_imp_DV[[2]]$aim2_df_no3y,
  nest = TRUE
)

etable34_design_obesity_imp2 <- svydesign(
  ids = ~site_br + family_id_br,
  weights = ~abcd_pps_weight,
  data = aim_obesity_list_non_imp_DV[[2]]$aim2_df_no3y,
  nest = TRUE
)

etable34_design_sleep_imp2 <- svydesign(
  ids = ~site_br + family_id_br,
  weights = ~abcd_pps_weight,
  data = aim_sleep_list_non_imp_DV[[2]]$aim2_df_no3y,
  nest = TRUE
)


run_write_models_svy(
  data = aim_dep_list_non_imp_DV[[2]]$aim2_df_no3y,
  design = etable34_design_dep_imp2,
  list_DVs = c("depression_dx_y"),
  list_covars = covariates_list_aim2$depression_dx_y[3],
  list_IVs = c("scale(schoolyear_total_smartph_wsum)"),
  binary_DV = TRUE,
  ext = "eTable34_survey_weight_non_imp_DV_imp2"
)

### baseline from 2
run_write_models_svy(
  data = aim_obesity_list_non_imp_DV[[2]]$aim2_df_no3y,
  design = etable34_design_obesity_imp2,
  list_DVs = c("bmi_obesity"),
  list_covars = covariates_list_aim2_2y$bmi_obesity[3],
  list_IVs = c("scale(schoolyear_total_smartph_wsum)"),
  binary_DV = TRUE,
  ext = "eTable34_survey_weight_non_imp_DV_imp2"
)

run_write_models_svy(
  data = aim_sleep_list_non_imp_DV[[2]]$aim2_df_no3y,
  design = etable34_design_sleep_imp2,
  list_DVs = c("lack_sleep"),
  list_covars = covariates_list_aim2$lack_sleep[3],
  list_IVs = c("scale(schoolyear_total_smartph_wsum)"),
  binary_DV = TRUE,
  ext = "eTable34_survey_weight_non_imp_DV_imp2"
)


## eTable 6 smartphone time categories----
run_write_models_svy(
  data = aim_dep_list_non_imp_DV[[2]]$aim2_df_no3y,
  design = etable34_design_dep_imp2,
  list_DVs = c("depression_dx_y"),
  list_covars = covariates_list_aim2$depression_dx_y[3],
  list_IVs = c("schoolyear_total_smartph_wsum_wmean_perday_cat"),
  binary_DV = TRUE,
  ext = "eTable6_survey_weight_non_imp_DV_imp2"
)


run_write_models_svy(
  data = aim_obesity_list_non_imp_DV[[2]]$aim2_df_no3y,
  design = etable34_design_obesity_imp2,
  list_DVs = c("bmi_obesity"),
  list_covars = covariates_list_aim2_2y$bmi_obesity[3],
  list_IVs = c("schoolyear_total_smartph_wsum_wmean_perday_cat"),
  binary_DV = TRUE,
  ext = "eTable6_survey_weight_non_imp_DV_imp2"
)

run_write_models_svy(
  data = aim_sleep_list_non_imp_DV[[2]]$aim2_df_no3y,
  design = etable34_design_sleep_imp2,
  list_DVs = c("lack_sleep"),
  list_covars = covariates_list_aim2$lack_sleep[3],
  list_IVs = c("schoolyear_total_smartph_wsum_wmean_perday_cat"),
  binary_DV = TRUE,
  ext = "eTable6_survey_weight_non_imp_DV_imp2"
)


## eTable 7----
run_write_models_svy(
  data = aim_dep_list_non_imp_DV[[2]]$aim2_df_no3y,
  design = etable34_design_dep_imp2,
  list_DVs = c("depression_dx_y"),
  list_covars = covariates_list_aim2$depression_dx_y[3],
  list_IVs = c("nt_y_stq__sleep_002_phone_outside_room"),
  binary_DV = TRUE,
  ext = "eTable7_survey_weight_non_imp_DV_imp2"
)

run_write_models_svy(
  data = aim_obesity_list_non_imp_DV[[2]]$aim2_df_no3y,
  design = etable34_design_obesity_imp2,
  list_DVs = c("bmi_obesity"),
  list_covars = covariates_list_aim2_2y$bmi_obesity[3],
  list_IVs = c("nt_y_stq__sleep_002_phone_outside_room"),
  binary_DV = TRUE,
  ext = "eTable7_survey_weight_non_imp_DV_imp2"
)

run_write_models_svy(
  data = aim_sleep_list_non_imp_DV[[2]]$aim2_df_no3y,
  design = etable34_design_sleep_imp2,
  list_DVs = c("lack_sleep"),
  list_covars = covariates_list_aim2$lack_sleep[3],
  list_IVs = c("nt_y_stq__sleep_002_phone_outside_room"),
  binary_DV = TRUE,
  ext = "eTable7_survey_weight_non_imp_DV_imp2"
)

# IMPUTATION 3----
## eTable 2----
etable2_design_dep_imp3 <- svydesign(
  ids = ~site_br + family_id_br,
  weights = ~abcd_pps_weight,
  data = aim_dep_list_non_imp_DV[[3]]$aim1_df,
  nest = TRUE
)

etable2_design_obesity_imp3 <- svydesign(
  ids = ~site_br + family_id_br,
  weights = ~abcd_pps_weight,
  data = aim_obesity_list_non_imp_DV[[3]]$aim1_df,
  nest = TRUE
)

etable2_design_sleep_imp3 <- svydesign(
  ids = ~site_br + family_id_br,
  weights = ~abcd_pps_weight,
  data = aim_sleep_list_non_imp_DV[[3]]$aim1_df,
  nest = TRUE
)

run_write_models_svy(
  data = aim_dep_list_non_imp_DV[[3]]$aim1_df,
  design = etable2_design_dep_imp3,
  list_DVs = c("depression_dx_y"),
  list_covars = covariates_list_aim1$depression_dx_y[3],
  list_IVs = c("smartphone_ownership"),
  binary_DV = TRUE,
  ext = "eTable2_survey_weight_non_imp_DV_imp3"
)

run_write_models_svy(
  data = aim_obesity_list_non_imp_DV[[3]]$aim1_df,
  design = etable2_design_obesity_imp3,
  list_DVs = c("bmi_obesity"),
  list_covars = covariates_list_aim2_2y$bmi_obesity[3],
  list_IVs = c("smartphone_ownership"),
  binary_DV = TRUE,
  ext = "eTable2_covary_obesity2y_survey_weight_non_imp_DV_imp3"
)

run_write_models_svy(
  data = aim_sleep_list_non_imp_DV[[3]]$aim1_df,
  design = etable2_design_sleep_imp3,
  list_DVs = c("lack_sleep"),
  list_covars = covariates_list_aim1$lack_sleep[3],
  list_IVs = c("smartphone_ownership"),
  binary_DV = TRUE,
  ext = "eTable2_survey_weight_non_imp_DV_imp3"
)


## eTable 3 and 4----
etable34_design_dep_imp3 <- svydesign(
  ids = ~site_br + family_id_br,
  weights = ~abcd_pps_weight,
  data = aim_dep_list_non_imp_DV[[3]]$aim2_df_no3y,
  nest = TRUE
)

etable34_design_obesity_imp3 <- svydesign(
  ids = ~site_br + family_id_br,
  weights = ~abcd_pps_weight,
  data = aim_obesity_list_non_imp_DV[[3]]$aim2_df_no3y,
  nest = TRUE
)

etable34_design_sleep_imp3 <- svydesign(
  ids = ~site_br + family_id_br,
  weights = ~abcd_pps_weight,
  data = aim_sleep_list_non_imp_DV[[3]]$aim2_df_no3y,
  nest = TRUE
)


run_write_models_svy(
  data = aim_dep_list_non_imp_DV[[3]]$aim2_df_no3y,
  design = etable34_design_dep_imp3,
  list_DVs = c("depression_dx_y"),
  list_covars = covariates_list_aim2$depression_dx_y[3],
  list_IVs = c("scale(schoolyear_total_smartph_wsum)"),
  binary_DV = TRUE,
  ext = "eTable34_survey_weight_non_imp_DV_imp3"
)

### baseline from 2
run_write_models_svy(
  data = aim_obesity_list_non_imp_DV[[3]]$aim2_df_no3y,
  design = etable34_design_obesity_imp3,
  list_DVs = c("bmi_obesity"),
  list_covars = covariates_list_aim2_2y$bmi_obesity[3],
  list_IVs = c("scale(schoolyear_total_smartph_wsum)"),
  binary_DV = TRUE,
  ext = "eTable34_survey_weight_non_imp_DV_imp3"
)

run_write_models_svy(
  data = aim_sleep_list_non_imp_DV[[3]]$aim2_df_no3y,
  design = etable34_design_sleep_imp3,
  list_DVs = c("lack_sleep"),
  list_covars = covariates_list_aim2$lack_sleep[3],
  list_IVs = c("scale(schoolyear_total_smartph_wsum)"),
  binary_DV = TRUE,
  ext = "eTable34_survey_weight_non_imp_DV_imp3"
)


## eTable 6 smartphone time categories----
run_write_models_svy(
  data = aim_dep_list_non_imp_DV[[3]]$aim2_df_no3y,
  design = etable34_design_dep_imp3,
  list_DVs = c("depression_dx_y"),
  list_covars = covariates_list_aim2$depression_dx_y[3],
  list_IVs = c("schoolyear_total_smartph_wsum_wmean_perday_cat"),
  binary_DV = TRUE,
  ext = "eTable6_survey_weight_non_imp_DV_imp3"
)


run_write_models_svy(
  data = aim_obesity_list_non_imp_DV[[3]]$aim2_df_no3y,
  design = etable34_design_obesity_imp3,
  list_DVs = c("bmi_obesity"),
  list_covars = covariates_list_aim2_2y$bmi_obesity[3],
  list_IVs = c("schoolyear_total_smartph_wsum_wmean_perday_cat"),
  binary_DV = TRUE,
  ext = "eTable6_survey_weight_non_imp_DV_imp3"
)

run_write_models_svy(
  data = aim_sleep_list_non_imp_DV[[3]]$aim2_df_no3y,
  design = etable34_design_sleep_imp3,
  list_DVs = c("lack_sleep"),
  list_covars = covariates_list_aim2$lack_sleep[3],
  list_IVs = c("schoolyear_total_smartph_wsum_wmean_perday_cat"),
  binary_DV = TRUE,
  ext = "eTable6_survey_weight_non_imp_DV_imp3"
)


## eTable 7----
run_write_models_svy(
  data = aim_dep_list_non_imp_DV[[3]]$aim2_df_no3y,
  design = etable34_design_dep_imp3,
  list_DVs = c("depression_dx_y"),
  list_covars = covariates_list_aim2$depression_dx_y[3],
  list_IVs = c("nt_y_stq__sleep_002_phone_outside_room"),
  binary_DV = TRUE,
  ext = "eTable7_survey_weight_non_imp_DV_imp3"
)

run_write_models_svy(
  data = aim_obesity_list_non_imp_DV[[3]]$aim2_df_no3y,
  design = etable34_design_obesity_imp3,
  list_DVs = c("bmi_obesity"),
  list_covars = covariates_list_aim2_2y$bmi_obesity[3],
  list_IVs = c("nt_y_stq__sleep_002_phone_outside_room"),
  binary_DV = TRUE,
  ext = "eTable7_survey_weight_non_imp_DV_imp3"
)

run_write_models_svy(
  data = aim_sleep_list_non_imp_DV[[3]]$aim2_df_no3y,
  design = etable34_design_sleep_imp3,
  list_DVs = c("lack_sleep"),
  list_covars = covariates_list_aim2$lack_sleep[3],
  list_IVs = c("nt_y_stq__sleep_002_phone_outside_room"),
  binary_DV = TRUE,
  ext = "eTable7_survey_weight_non_imp_DV_imp3"
)

# IMPUTATION 4----
## eTable 2----
etable2_design_dep_imp4 <- svydesign(
  ids = ~site_br + family_id_br,
  weights = ~abcd_pps_weight,
  data = aim_dep_list_non_imp_DV[[4]]$aim1_df,
  nest = TRUE
)

etable2_design_obesity_imp4 <- svydesign(
  ids = ~site_br + family_id_br,
  weights = ~abcd_pps_weight,
  data = aim_obesity_list_non_imp_DV[[4]]$aim1_df,
  nest = TRUE
)

etable2_design_sleep_imp4 <- svydesign(
  ids = ~site_br + family_id_br,
  weights = ~abcd_pps_weight,
  data = aim_sleep_list_non_imp_DV[[4]]$aim1_df,
  nest = TRUE
)

run_write_models_svy(
  data = aim_dep_list_non_imp_DV[[4]]$aim1_df,
  design = etable2_design_dep_imp4,
  list_DVs = c("depression_dx_y"),
  list_covars = covariates_list_aim1$depression_dx_y[3],
  list_IVs = c("smartphone_ownership"),
  binary_DV = TRUE,
  ext = "eTable2_survey_weight_non_imp_DV_imp4"
)

run_write_models_svy(
  data = aim_obesity_list_non_imp_DV[[4]]$aim1_df,
  design = etable2_design_obesity_imp4,
  list_DVs = c("bmi_obesity"),
  list_covars = covariates_list_aim2_2y$bmi_obesity[3],
  list_IVs = c("smartphone_ownership"),
  binary_DV = TRUE,
  ext = "eTable2_covary_obesity2y_survey_weight_non_imp_DV_imp4"
)

run_write_models_svy(
  data = aim_sleep_list_non_imp_DV[[4]]$aim1_df,
  design = etable2_design_sleep_imp4,
  list_DVs = c("lack_sleep"),
  list_covars = covariates_list_aim1$lack_sleep[3],
  list_IVs = c("smartphone_ownership"),
  binary_DV = TRUE,
  ext = "eTable2_survey_weight_non_imp_DV_imp4"
)


## eTable 3 and 4----
etable34_design_dep_imp4 <- svydesign(
  ids = ~site_br + family_id_br,
  weights = ~abcd_pps_weight,
  data = aim_dep_list_non_imp_DV[[4]]$aim2_df_no3y,
  nest = TRUE
)

etable34_design_obesity_imp4 <- svydesign(
  ids = ~site_br + family_id_br,
  weights = ~abcd_pps_weight,
  data = aim_obesity_list_non_imp_DV[[4]]$aim2_df_no3y,
  nest = TRUE
)

etable34_design_sleep_imp4 <- svydesign(
  ids = ~site_br + family_id_br,
  weights = ~abcd_pps_weight,
  data = aim_sleep_list_non_imp_DV[[4]]$aim2_df_no3y,
  nest = TRUE
)


run_write_models_svy(
  data = aim_dep_list_non_imp_DV[[4]]$aim2_df_no3y,
  design = etable34_design_dep_imp4,
  list_DVs = c("depression_dx_y"),
  list_covars = covariates_list_aim2$depression_dx_y[3],
  list_IVs = c("scale(schoolyear_total_smartph_wsum)"),
  binary_DV = TRUE,
  ext = "eTable34_survey_weight_non_imp_DV_imp4"
)

### baseline from 2
run_write_models_svy(
  data = aim_obesity_list_non_imp_DV[[4]]$aim2_df_no3y,
  design = etable34_design_obesity_imp4,
  list_DVs = c("bmi_obesity"),
  list_covars = covariates_list_aim2_2y$bmi_obesity[3],
  list_IVs = c("scale(schoolyear_total_smartph_wsum)"),
  binary_DV = TRUE,
  ext = "eTable34_survey_weight_non_imp_DV_imp4"
)

run_write_models_svy(
  data = aim_sleep_list_non_imp_DV[[4]]$aim2_df_no3y,
  design = etable34_design_sleep_imp4,
  list_DVs = c("lack_sleep"),
  list_covars = covariates_list_aim2$lack_sleep[3],
  list_IVs = c("scale(schoolyear_total_smartph_wsum)"),
  binary_DV = TRUE,
  ext = "eTable34_survey_weight_non_imp_DV_imp4"
)


## eTable 6 smartphone time categories----
run_write_models_svy(
  data = aim_dep_list_non_imp_DV[[4]]$aim2_df_no3y,
  design = etable34_design_dep_imp4,
  list_DVs = c("depression_dx_y"),
  list_covars = covariates_list_aim2$depression_dx_y[3],
  list_IVs = c("schoolyear_total_smartph_wsum_wmean_perday_cat"),
  binary_DV = TRUE,
  ext = "eTable6_survey_weight_non_imp_DV_imp4"
)


run_write_models_svy(
  data = aim_obesity_list_non_imp_DV[[4]]$aim2_df_no3y,
  design = etable34_design_obesity_imp4,
  list_DVs = c("bmi_obesity"),
  list_covars = covariates_list_aim2_2y$bmi_obesity[3],
  list_IVs = c("schoolyear_total_smartph_wsum_wmean_perday_cat"),
  binary_DV = TRUE,
  ext = "eTable6_survey_weight_non_imp_DV_imp4"
)

run_write_models_svy(
  data = aim_sleep_list_non_imp_DV[[4]]$aim2_df_no3y,
  design = etable34_design_sleep_imp4,
  list_DVs = c("lack_sleep"),
  list_covars = covariates_list_aim2$lack_sleep[3],
  list_IVs = c("schoolyear_total_smartph_wsum_wmean_perday_cat"),
  binary_DV = TRUE,
  ext = "eTable6_survey_weight_non_imp_DV_imp4"
)


## eTable 7----
run_write_models_svy(
  data = aim_dep_list_non_imp_DV[[4]]$aim2_df_no3y,
  design = etable34_design_dep_imp4,
  list_DVs = c("depression_dx_y"),
  list_covars = covariates_list_aim2$depression_dx_y[3],
  list_IVs = c("nt_y_stq__sleep_002_phone_outside_room"),
  binary_DV = TRUE,
  ext = "eTable7_survey_weight_non_imp_DV_imp4"
)

run_write_models_svy(
  data = aim_obesity_list_non_imp_DV[[4]]$aim2_df_no3y,
  design = etable34_design_obesity_imp4,
  list_DVs = c("bmi_obesity"),
  list_covars = covariates_list_aim2_2y$bmi_obesity[3],
  list_IVs = c("nt_y_stq__sleep_002_phone_outside_room"),
  binary_DV = TRUE,
  ext = "eTable7_survey_weight_non_imp_DV_imp4"
)

run_write_models_svy(
  data = aim_sleep_list_non_imp_DV[[4]]$aim2_df_no3y,
  design = etable34_design_sleep_imp4,
  list_DVs = c("lack_sleep"),
  list_covars = covariates_list_aim2$lack_sleep[3],
  list_IVs = c("nt_y_stq__sleep_002_phone_outside_room"),
  binary_DV = TRUE,
  ext = "eTable7_survey_weight_non_imp_DV_imp4"
)


# IMPUTATION 5----
## eTable 2----
etable2_design_dep_imp5 <- svydesign(
  ids = ~site_br + family_id_br,
  weights = ~abcd_pps_weight,
  data = aim_dep_list_non_imp_DV[[5]]$aim1_df,
  nest = TRUE
)

etable2_design_obesity_imp5 <- svydesign(
  ids = ~site_br + family_id_br,
  weights = ~abcd_pps_weight,
  data = aim_obesity_list_non_imp_DV[[5]]$aim1_df,
  nest = TRUE
)

etable2_design_sleep_imp5 <- svydesign(
  ids = ~site_br + family_id_br,
  weights = ~abcd_pps_weight,
  data = aim_sleep_list_non_imp_DV[[5]]$aim1_df,
  nest = TRUE
)

run_write_models_svy(
  data = aim_dep_list_non_imp_DV[[5]]$aim1_df,
  design = etable2_design_dep_imp5,
  list_DVs = c("depression_dx_y"),
  list_covars = covariates_list_aim1$depression_dx_y[3],
  list_IVs = c("smartphone_ownership"),
  binary_DV = TRUE,
  ext = "eTable2_survey_weight_non_imp_DV_imp5"
)

run_write_models_svy(
  data = aim_obesity_list_non_imp_DV[[5]]$aim1_df,
  design = etable2_design_obesity_imp5,
  list_DVs = c("bmi_obesity"),
  list_covars = covariates_list_aim2_2y$bmi_obesity[3],
  list_IVs = c("smartphone_ownership"),
  binary_DV = TRUE,
  ext = "eTable2_covary_obesity2y_survey_weight_non_imp_DV_imp5"
)

run_write_models_svy(
  data = aim_sleep_list_non_imp_DV[[5]]$aim1_df,
  design = etable2_design_sleep_imp5,
  list_DVs = c("lack_sleep"),
  list_covars = covariates_list_aim1$lack_sleep[3],
  list_IVs = c("smartphone_ownership"),
  binary_DV = TRUE,
  ext = "eTable2_survey_weight_non_imp_DV_imp5"
)


## eTable 3 and 4----
etable34_design_dep_imp5 <- svydesign(
  ids = ~site_br + family_id_br,
  weights = ~abcd_pps_weight,
  data = aim_dep_list_non_imp_DV[[5]]$aim2_df_no3y,
  nest = TRUE
)

etable34_design_obesity_imp5 <- svydesign(
  ids = ~site_br + family_id_br,
  weights = ~abcd_pps_weight,
  data = aim_obesity_list_non_imp_DV[[5]]$aim2_df_no3y,
  nest = TRUE
)

etable34_design_sleep_imp5 <- svydesign(
  ids = ~site_br + family_id_br,
  weights = ~abcd_pps_weight,
  data = aim_sleep_list_non_imp_DV[[5]]$aim2_df_no3y,
  nest = TRUE
)


run_write_models_svy(
  data = aim_dep_list_non_imp_DV[[5]]$aim2_df_no3y,
  design = etable34_design_dep_imp5,
  list_DVs = c("depression_dx_y"),
  list_covars = covariates_list_aim2$depression_dx_y[3],
  list_IVs = c("scale(schoolyear_total_smartph_wsum)"),
  binary_DV = TRUE,
  ext = "eTable34_survey_weight_non_imp_DV_imp5"
)

### baseline from 2
run_write_models_svy(
  data = aim_obesity_list_non_imp_DV[[5]]$aim2_df_no3y,
  design = etable34_design_obesity_imp5,
  list_DVs = c("bmi_obesity"),
  list_covars = covariates_list_aim2_2y$bmi_obesity[3],
  list_IVs = c("scale(schoolyear_total_smartph_wsum)"),
  binary_DV = TRUE,
  ext = "eTable34_survey_weight_non_imp_DV_imp5"
)

run_write_models_svy(
  data = aim_sleep_list_non_imp_DV[[5]]$aim2_df_no3y,
  design = etable34_design_sleep_imp5,
  list_DVs = c("lack_sleep"),
  list_covars = covariates_list_aim2$lack_sleep[3],
  list_IVs = c("scale(schoolyear_total_smartph_wsum)"),
  binary_DV = TRUE,
  ext = "eTable34_survey_weight_non_imp_DV_imp5"
)


## eTable 6 smartphone time categories----
run_write_models_svy(
  data = aim_dep_list_non_imp_DV[[5]]$aim2_df_no3y,
  design = etable34_design_dep_imp5,
  list_DVs = c("depression_dx_y"),
  list_covars = covariates_list_aim2$depression_dx_y[3],
  list_IVs = c("schoolyear_total_smartph_wsum_wmean_perday_cat"),
  binary_DV = TRUE,
  ext = "eTable6_survey_weight_non_imp_DV_imp5"
)


run_write_models_svy(
  data = aim_obesity_list_non_imp_DV[[5]]$aim2_df_no3y,
  design = etable34_design_obesity_imp5,
  list_DVs = c("bmi_obesity"),
  list_covars = covariates_list_aim2_2y$bmi_obesity[3],
  list_IVs = c("schoolyear_total_smartph_wsum_wmean_perday_cat"),
  binary_DV = TRUE,
  ext = "eTable6_survey_weight_non_imp_DV_imp5"
)

run_write_models_svy(
  data = aim_sleep_list_non_imp_DV[[5]]$aim2_df_no3y,
  design = etable34_design_sleep_imp5,
  list_DVs = c("lack_sleep"),
  list_covars = covariates_list_aim2$lack_sleep[3],
  list_IVs = c("schoolyear_total_smartph_wsum_wmean_perday_cat"),
  binary_DV = TRUE,
  ext = "eTable6_survey_weight_non_imp_DV_imp5"
)


## eTable 7----
run_write_models_svy(
  data = aim_dep_list_non_imp_DV[[5]]$aim2_df_no3y,
  design = etable34_design_dep_imp5,
  list_DVs = c("depression_dx_y"),
  list_covars = covariates_list_aim2$depression_dx_y[3],
  list_IVs = c("nt_y_stq__sleep_002_phone_outside_room"),
  binary_DV = TRUE,
  ext = "eTable7_survey_weight_non_imp_DV_imp5"
)

run_write_models_svy(
  data = aim_obesity_list_non_imp_DV[[5]]$aim2_df_no3y,
  design = etable34_design_obesity_imp5,
  list_DVs = c("bmi_obesity"),
  list_covars = covariates_list_aim2_2y$bmi_obesity[3],
  list_IVs = c("nt_y_stq__sleep_002_phone_outside_room"),
  binary_DV = TRUE,
  ext = "eTable7_survey_weight_non_imp_DV_imp5"
)

run_write_models_svy(
  data = aim_sleep_list_non_imp_DV[[5]]$aim2_df_no3y,
  design = etable34_design_sleep_imp5,
  list_DVs = c("lack_sleep"),
  list_covars = covariates_list_aim2$lack_sleep[3],
  list_IVs = c("nt_y_stq__sleep_002_phone_outside_room"),
  binary_DV = TRUE,
  ext = "eTable7_survey_weight_non_imp_DV_imp5"
)


# POOL RESULTS----
## eTable 2----
input_table_2_svy <- data.frame(
    outcome = c("depression_dx_y", "bmi_obesity", "lack_sleep"),
    
    OR_1 = c(1.55, 0.94, 1.24),
    L_1  = c(0.79, 0.58, 0.89),
    U_1  = c(3.01, 1.52, 1.72),
    
    OR_2 = c(1.57, 0.86, 1.25),
    L_2  = c(0.79, 0.51, 0.90),
    U_2  = c(3.10, 1.44, 1.73),
    
    OR_3 = c(1.59, 0.95, 1.24),
    L_3  = c(0.81, 0.54, 0.90),
    U_3  = c(3.13, 1.67, 1.72),
    
    OR_4 = c(1.56, 0.86, 1.25),
    L_4  = c(0.81, 0.53, 0.90),
    U_4  = c(3.03, 1.39, 1.74),
    
    OR_5 = c(1.57, 0.92, 1.25),
    L_5  = c(0.80, 0.57, 0.90),
    U_5  = c(3.08, 1.48, 1.73)
  )

pool_MI_from_table(input_table_2_svy) %>% View()

## eTable 3----
input_table_3_svy <- data.frame(
  
  outcome = c("depression_dx_y", "bmi_obesity", "lack_sleep"),
  
    OR_1 = c(1.29, 1.52, 1.34),
    L_1  = c(1.00, 1.04, 1.14),
    U_1  = c(1.66, 2.23, 1.58),
    
    OR_2 = c(1.29, 1.59, 1.34),
    L_2  = c(1.00, 1.16, 1.15),
    U_2  = c(1.66, 2.19, 1.57),
    
    OR_3 = c(1.29, 1.63, 1.34),
    L_3  = c(1.01, 1.16, 1.14),
    U_3  = c(1.65, 2.29, 1.57),
    
    OR_4 = c(1.29, 1.58, 1.35),
    L_4  = c(1.00, 1.15, 1.16),
    U_4  = c(1.65, 2.18, 1.58),
    
    OR_5 = c(1.30, 1.64, 1.35),
    L_5  = c(1.01, 1.15, 1.16),
    U_5  = c(1.66, 2.35, 1.56)
  )

pool_MI_from_table(input_table_3_svy) %>% View()

## eTable 6----
input_table_6A_svy <- data.frame(
  
  outcome = c("depression_dx_y", "bmi_obesity", "lack_sleep"),
    
    OR_1 = c(1.62, 1.14, 1.63),
    L_1  = c(0.81, 0.65, 1.15),
    U_1  = c(3.24, 1.98, 2.32),
    
    OR_2 = c(1.61, 1.18, 1.63),
    L_2  = c(0.80, 0.62, 1.15),
    U_2  = c(3.24, 2.25, 2.32),
    
    OR_3 = c(1.64, 1.15, 1.64),
    L_3  = c(0.82, 0.63, 1.15),
    U_3  = c(3.26, 2.12, 2.33),
    
    OR_4 = c(1.63, 1.12, 1.65),
    L_4  = c(0.82, 0.62, 1.17),
    U_4  = c(3.24, 2.02, 2.33),
    
    OR_5 = c(1.62, 1.24, 1.65),
    L_5  = c(0.81, 0.64, 1.16),
    U_5  = c(3.22, 2.40, 2.35)
  )

pool_MI_from_table(input_table_6A_svy) %>% View()

input_table_6B_svy <- data.frame(
  
  outcome = c("depression_dx_y", "bmi_obesity", "lack_sleep"),
    
    OR_1 = c(2.99, 3.42, 2.07),
    L_1  = c(0.97, 1.36, 1.14),
    U_1  = c(9.25, 8.63, 3.76),
    
    OR_2 = c(3.02, 3.93, 2.07),
    L_2  = c(0.97, 1.61, 1.15),
    U_2  = c(9.40, 9.56, 3.76),
    
    OR_3 = c(2.97, 3.94, 2.06),
    L_3  = c(0.97, 1.62, 1.14),
    U_3  = c(9.05, 9.62, 3.72),
    
    OR_4 = c(2.94, 3.70, 2.09),
    L_4  = c(0.96, 1.49, 1.16),
    U_4  = c(9.00, 9.23, 3.78),
    
    OR_5 = c(3.00, 4.32, 2.07),
    L_5  = c(0.98, 1.73, 1.16),
    U_5  = c(9.16, 10.82, 3.69)
  )

pool_MI_from_table(input_table_6B_svy) %>% View()

## eTable 7----
input_table_7_svy <- data.frame(
  
    outcome = c("depression_dx_y", "bmi_obesity", "lack_sleep"),
    
    OR_1 = c(1.01, 0.72, 0.69),
    L_1  = c(0.51, 0.44, 0.43),
    U_1  = c(1.99, 1.18, 1.12),
    
    OR_2 = c(1.01, 0.75, 0.69),
    L_2  = c(0.51, 0.46, 0.43),
    U_2  = c(1.98, 1.23, 1.11),
    
    OR_3 = c(1.01, 0.81, 0.69),
    L_3  = c(0.52, 0.47, 0.43),
    U_3  = c(2.00, 1.40, 1.12),
    
    OR_4 = c(1.02, 0.75, 0.69),
    L_4  = c(0.52, 0.45, 0.43),
    U_4  = c(2.00, 1.25, 1.11),
    
    OR_5 = c(1.01, 0.75, 0.70),
    L_5  = c(0.52, 0.45, 0.43),
    U_5  = c(1.97, 1.23, 1.13)
)

pool_MI_from_table(input_table_7_svy) %>% View()

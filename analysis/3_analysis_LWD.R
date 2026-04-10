
library(dplyr)
source("analysis/analysis_utility_fun.R")

# Read data----
aim1_df <- readRDS("data/aim1_df.rds")
aim2_df_no3y <- readRDS("data/aim2_df_no3y.rds")
covariates_list_aim1 <- readRDS("data/covariates_list_aim1.rds")
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


# Descriptive----
## Correlation of 2 measures of total smartphone use
aim2_df_no3y %>%
  dplyr::select(schoolyear_total_smartph_wsum, total_smartph_sum_wsum) %>%
  cor(use = "pairwise.complete.obs")


missing = aim2_df_no3y %>%
    select(matches("_pair_hr_min|_wsum$|video_games|non_social_communication")) %>%
    select(where(~ !all(is.na(.)))) %>%
    summarise(
        across(
            everything(),
            list(
                N   = ~ sum(is.na(.)),
                Pct = ~ mean(is.na(.)) * 100
            )
        )
    ) %>%
    pivot_longer(
        everything(),
        names_to = c("variable", ".value"),
        names_sep = "_(?=[^_]+$)"
    )

write.csv(missing, "results/missingness_variables_6.1.csv", row.names = FALSE)


# eTable 9----
run_write_models(
  data = aim2_df_no3y,
  list_DVs = "depression_dx_y",
  list_IVs = IVs_aim2,
  list_covars = covariates_list_aim2$depression_dx_y,
  random_eff = random_effects_s,
  binary_DV = TRUE,
  ext = "eTable9",
  CI_level = (1-0.05)
)

run_write_models(
  data = aim2_df_no3y,
  list_DVs = "bmi_obesity",
  list_IVs = IVs_aim2,
  list_covars = covariates_list_aim2_2y$bmi_obesity,
  random_eff = random_effects_s,
  binary_DV = TRUE,
  ext = "eTable9",
  CI_level = (1-0.05)
)

run_write_models(
  data = aim2_df_no3y,
  list_DVs = "lack_sleep",
  list_IVs = IVs_aim2,
  list_covars = covariates_list_aim2$lack_sleep,
  random_eff = random_effects_s,
  binary_DV = TRUE,
  ext = "eTable9",
  CI_level = (1-0.05)
)

# eTable 10----
run_write_models(
  data = aim1_df,
  list_DVs = "depression_dx_y",
  list_covars = covariates_list_aim1$depression_dx_y,
  random_eff = random_effects_s,
  list_IVs = "smartphone_ownership",
  binary_DV = TRUE,
  ext = "eTable10",
  CI_level = (1-0.05)
)


run_write_models(
    data = aim1_df,
    list_DVs = "bmi_obesity",
    list_covars = covariates_list_aim2_2y$bmi_obesity,
    random_eff = random_effects_s,
    list_IVs = "smartphone_ownership",
    binary_DV = TRUE,
    ext = "eTable10",
    CI_level = (1-0.05)
)


run_write_models(
  data = aim1_df,
  list_DVs = "lack_sleep",
  list_covars = covariates_list_aim1$lack_sleep,
  random_eff = random_effects_s,
  list_IVs = "smartphone_ownership",
  binary_DV = TRUE,
  ext = "eTable10",
  CI_level = (1-0.05)
)


# eTable 11 and 12----
run_write_models(
  data = aim2_df_no3y,
  list_DVs = "depression_dx_y",
  list_IVs = c("scale(schoolyear_total_smartph_wsum)", "scale(schoolyear_total_smartph_wsum)*sex_br"),
  list_covars = covariates_list_aim2$depression_dx_y,
  random_eff = random_effects_s,
  binary_DV = TRUE,
  ext = "eTable11_12",
  CI_level = (1-0.05)
)


run_write_models(
  data = aim2_df_no3y,
  list_DVs = "lack_sleep",
  list_IVs = c("scale(schoolyear_total_smartph_wsum)", "scale(schoolyear_total_smartph_wsum)*sex_br"),
  list_covars = covariates_list_aim2$lack_sleep,
  random_eff = random_effects_s,
  binary_DV = TRUE,
  ext = "eTable11_12",
  CI_level = (1-0.05)
)


# eTable 13----
run_write_models(
  data = aim2_df_no3y,
  list_DVs = "depression_dx_y",
  list_IVs = c("nt_y_stq__sleep_002_phone_outside_room"),
  list_covars = covariates_list_aim2$depression_dx_y,
  random_eff = random_effects_s,
  binary_DV = TRUE,
  ext = "eTable13",
  CI_level = (1-0.05)
)


run_write_models(
  data = aim2_df_no3y,
  list_DVs = "lack_sleep",
  list_IVs = c("nt_y_stq__sleep_002_phone_outside_room"),
  list_covars = covariates_list_aim2$lack_sleep,
  random_eff = random_effects_s,
  binary_DV = TRUE,
  ext = "eTable13",
  CI_level = (1-0.05)
)




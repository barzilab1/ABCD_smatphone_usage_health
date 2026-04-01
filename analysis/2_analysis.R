
library(dplyr)
library(EValue)
source("analysis/analysis_utility_fun.R")

# Read data----
aim1_df <- readRDS("data/aim1_df.rds")
aim2_df_no2y <- readRDS("data/aim2_df_no2y.rds")
aim2_df_no3y <- readRDS("data/aim2_df_no3y.rds")
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

# eTable 2----
run_write_models(
  data = aim1_df,
  list_DVs = "depression_dx_y",
  list_covars = covariates_list_aim1$depression_dx_y,
  random_eff = random_effects_s,
  list_IVs = "smartphone_ownership",
  binary_DV = TRUE,
  ext = "eTable2",
  CI_level = (1-0.05)
)


run_write_models(
    data = aim1_df,
    list_DVs = "bmi_obesity",
    list_covars = covariates_list_aim2_2y$bmi_obesity,
    random_eff = random_effects_s,
    list_IVs = "smartphone_ownership",
    binary_DV = TRUE,
    ext = "eTable2_covary_obesity2y",
    CI_level = (1-0.05)
)


run_write_models(
  data = aim1_df,
  list_DVs = "lack_sleep",
  list_covars = covariates_list_aim1$lack_sleep,
  random_eff = random_effects_s,
  list_IVs = "smartphone_ownership",
  binary_DV = TRUE,
  ext = "eTable2",
  CI_level = (1-0.05)
)

# eTable 3 and 4----
run_write_models(
  data = aim2_df_no3y,
  list_DVs = "depression_dx_y",
  list_IVs = c("scale(schoolyear_total_smartph_wsum)", "scale(schoolyear_total_smartph_wsum)*sex_br"),
  list_covars = covariates_list_aim2$depression_dx_y,
  random_eff = random_effects_s,
  binary_DV = TRUE,
  ext = "eTable3_4",
  CI_level = (1-0.05)
)

# baseline from 2
run_write_models(
    data = aim2_df_no3y,
    list_DVs = "bmi_obesity",
    list_IVs = c("scale(schoolyear_total_smartph_wsum)", "scale(schoolyear_total_smartph_wsum)*sex_br"),
    list_covars = covariates_list_aim2_2y$bmi_obesity,
    random_eff = random_effects_s,
    binary_DV = TRUE,
    ext = "eTable3_4",
    CI_level = (1-0.05)
)


run_write_models(
  data = aim2_df_no3y,
  list_DVs = "lack_sleep",
  list_IVs = c("scale(schoolyear_total_smartph_wsum)", "scale(schoolyear_total_smartph_wsum)*sex_br"),
  list_covars = covariates_list_aim2$lack_sleep,
  random_eff = random_effects_s,
  binary_DV = TRUE,
  ext = "eTable3_4",
  CI_level = (1-0.05)
)

###eTable 3 and 4 GAM----
### random-effect smooths----
aim2_df_no3y <- as.data.frame(aim2_df_no3y)

aim2_df_no3y$site_br <- factor(aim2_df_no3y$site_br)
aim2_df_no3y$family_id_br <- factor(aim2_df_no3y$family_id_br)
aim2_df_no3y$sex_br <- factor(aim2_df_no3y$sex_br)
aim2_df_no3y$race_black <- factor(aim2_df_no3y$race_black)
aim2_df_no3y$ethnicity_hisp_br <- factor(aim2_df_no3y$ethnicity_hisp_br)

library(mgcv)
library(sjPlot)
#### method 1----

aim2_df_no3y$site_family <- interaction(aim2_df_no3y$site_br, aim2_df_no3y$family_id_br, drop = TRUE)

gam_dep_3 <- gam(
  depression_dx_y ~ 
    age_br + sex_br + race_black + ethnicity_hisp_br +
    household_income + parents_high_edu_br + puberty_both_sexes +
    fc_y_pm_mean + nt_p_yst_001___2 + nt_p_yst_001___3 +
    nt_p_yst_001___4 + nt_p_yst_001___6 +
    depression_dx_y_2y +
    s(schoolyear_total_smartph_wsum) +
    s(site_br, bs = "re") +
    s(site_family, bs = "re"),
  family = binomial(link = "logit"),
  data = aim2_df_no3y,
  method = "REML"
) # running this
tab_model(gam_dep_3)

#### method 2----
library(gamm4)
gamm4_dep_1 <- gamm4(
  depression_dx_y ~ 
    age_br + sex_br + race_black + ethnicity_hisp_br +
    household_income + parents_high_edu_br + puberty_both_sexes +
    fc_y_pm_mean +
    nt_p_yst_001___2 + nt_p_yst_001___3 +
    nt_p_yst_001___4 + nt_p_yst_001___6 + depression_dx_y_2y +
    s(schoolyear_total_smartph_wsum),
  random = ~ (1 | site_br/family_id_br),
  data   = aim2_df_no3y,
  family = binomial(link = "logit")
)

gamm4_dep_1.2 <- gamm4(
  depression_dx_y ~ 
    age_br + sex_br + race_black + ethnicity_hisp_br +
    household_income + parents_high_edu_br + puberty_both_sexes +
    fc_y_pm_mean +
    nt_p_yst_001___2 + nt_p_yst_001___3 +
    nt_p_yst_001___4 + nt_p_yst_001___6 + depression_dx_y_2y +
    s(schoolyear_total_smartph_wsum),
  random = ~ (1 | site_br/family_id_br),
  data   = aim2_df_no3y,
  family = binomial(link = "logit")
)


tab_model(gamm4_dep_1)


#### method 3----
library(mgcv)
library(nlme)  # required for gamm()

gam_dep_gamm <- gamm(
  depression_dx_y ~ 
    age_br + sex_br + race_black + ethnicity_hisp_br +
    household_income + parents_high_edu_br + puberty_both_sexes +
    fc_y_pm_mean + nt_p_yst_001___2 + nt_p_yst_001___3 +
    nt_p_yst_001___4 + nt_p_yst_001___6 + depression_dx_y_2y +
    s(schoolyear_total_smartph_wsum),  # nonlinear smartphone effect
  family = binomial(link = "logit"),
  data = aim2_df_no3y,
  random = list(
    site_br = ~1,           # site random intercept
    family_id_br = ~1       # family random intercept (nested via data structure)
  )
)


library(dplyr)

# Extract from GAM component (fixed effects + smooths)
gam_sum <- summary(gam_dep_gamm$gam)

# Create comprehensive OR table
or_table <- data.frame(
  term = rownames(gam_sum$p.table),
  OR = exp(gam_sum$p.table[, "Estimate"]),
  SE = gam_sum$p.table[, "Std. Error"],
  CI_low = exp(gam_sum$p.table[, "Estimate"] - 1.96 * gam_sum$p.table[, "Std. Error"]),
  CI_high = exp(gam_sum$p.table[, "Estimate"] + 1.96 * gam_sum$p.table[, "Std. Error"]),
  p_value = gam_sum$p.table[, "Pr(>|t|)"]
) %>%
  mutate(
    OR_95CI = sprintf("%.2f (%.2f-%.2f)", OR, CI_low, CI_high),
    p_formatted = case_when(
      p_value < 0.001 ~ "<0.001",
      TRUE ~ sprintf("%.3f", p_value)
    ),
    sig = case_when(
      p_value < 0.001 ~ "***",
      p_value < 0.01 ~ "**",
      p_value < 0.05 ~ "*",
      p_value < 0.10 ~ ".",
      TRUE ~ ""
    )
  ) %>%
  filter(!grepl("Intercept", term))  # Remove intercept for cleaner table

# Print full table
print(or_table[, c("term", "OR_95CI", "p_formatted", "sig")], row.names = FALSE)

# Smartphone smooth significance (separate F-test)
cat("\n=== Smartphone Smooth (s(schoolyear_total_smartph_wsum)) ===\n")
print(gam_sum$s.table)

## Plots----
### mothod 1-----
# Basic smooth plot (smartphone effect)
plot(gam_dep_3, select = 1, shade = TRUE, shade.col = "lightblue", 
     main = "Nonlinear Effect of Smartphone Use on Depression Odds",
     xlab = "Smartphone Hours (Total)", ylab = "Partial Effect (log-odds)")

# Plot from GAM component
plot(gamm4_dep_1$gam, select = 1, shade = TRUE, shade.col = "lightblue",
     main = "Smartphone Use Effect (gamm4)",
     xlab = "Smartphone Hours", ylab = "Partial Effect")

# Plot from GAM component  
plot(gam_dep_gamm$gam, select = 1, shade = TRUE, shade.col = "lightblue",
     main = "Smartphone Use Effect (gamm)",
     xlab = "Smartphone Hours", ylab = "Partial Effect")

summary(gam_dep_3)$s.table
summary(gamm4_dep_1$gam)$s.table
summary(gam_dep_gamm$gam)$s.table

# eTable 5----
## Models during weekend and weekday
run_write_models(
  data = aim2_df_no3y,
  list_DVs = "depression_dx_y",
  list_IVs = c("scale(nt_y_stq__screen__wkdy__tot__pair_hr_min_002)"),
  list_covars = covariates_list_aim2$depression_dx_y,
  random_eff = random_effects_s,
  binary_DV = TRUE,
  ext = "eTable5_weekday",
  CI_level = (1-0.05)
)

run_write_models(
  data = aim2_df_no3y,
  list_DVs = "bmi_obesity",
  list_IVs = c("scale(nt_y_stq__screen__wkdy__tot__pair_hr_min_002)"),
  list_covars = covariates_list_aim2_2y$bmi_obesity,
  random_eff = random_effects_s,
  binary_DV = TRUE,
  ext = "eTable5_weekday",
  CI_level = (1-0.05)
)

run_write_models(
  data = aim2_df_no3y,
  list_DVs = "lack_sleep",
  list_IVs = c("scale(nt_y_stq__screen__wkdy__tot__pair_hr_min_002)"),
  list_covars = covariates_list_aim2$lack_sleep,
  random_eff = random_effects_s,
  binary_DV = TRUE,
  ext = "eTable5_weekday",
  CI_level = (1-0.05)
)

run_write_models(
  data = aim2_df_no3y,
  list_DVs = "depression_dx_y",
  list_IVs = c("scale(nt_y_stq__screen__wknd__tot__pair_hr_min_002)"),
  list_covars = covariates_list_aim2$depression_dx_y,
  random_eff = random_effects_s,
  binary_DV = TRUE,
  ext = "eTable5_weekend",
  CI_level = (1-0.05)
)

run_write_models(
  data = aim2_df_no3y,
  list_DVs = "bmi_obesity",
  list_IVs = c("scale(nt_y_stq__screen__wknd__tot__pair_hr_min_002)"),
  list_covars = covariates_list_aim2_2y$bmi_obesity,
  random_eff = random_effects_s,
  binary_DV = TRUE,
  ext = "eTable5_weekend",
  CI_level = (1-0.05)
)


run_write_models(
  data = aim2_df_no3y,
  list_DVs = "lack_sleep",
  list_IVs = c("scale(nt_y_stq__screen__wknd__tot__pair_hr_min_002)"),
  list_covars = covariates_list_aim2$lack_sleep,
  random_eff = random_effects_s,
  binary_DV = TRUE,
  ext = "eTable5_weekend",
  CI_level = (1-0.05)
)


# eTable 6 smartphone time categories----
table(aim2_df_no3y$schoolyear_total_smartph_wsum_wmean_perday_cat, useNA = "ifany")
run_write_models(
    data = aim2_df_no3y,
    list_DVs = "depression_dx_y",
    list_IVs = "schoolyear_total_smartph_wsum_wmean_perday_cat",
    list_covars = covariates_list_aim2$depression_dx_y,
    random_eff = random_effects_s,
    binary_DV = TRUE,
    ext = "eTable6",
    CI_level = (1-0.05)
)

run_write_models(
  data = aim2_df_no3y,
  list_DVs = "bmi_obesity",
  list_IVs = "schoolyear_total_smartph_wsum_wmean_perday_cat",
  list_covars = covariates_list_aim2_2y$bmi_obesity,
  random_eff = random_effects_s,
  binary_DV = TRUE,
  ext = "eTable6",
  CI_level = (1-0.05)
)

run_write_models(
    data = aim2_df_no3y,
    list_DVs = "lack_sleep",
    list_IVs = "schoolyear_total_smartph_wsum_wmean_perday_cat",
    list_covars = covariates_list_aim2$lack_sleep,
    random_eff = random_effects_s,
    binary_DV = TRUE,
    ext = "eTable6",
    CI_level = (1-0.05)
)


# eTable 7----
run_write_models(
  data = aim2_df_no3y,
  list_DVs = "depression_dx_y",
  list_IVs = c("nt_y_stq__sleep_002_phone_outside_room"),
  list_covars = covariates_list_aim2$depression_dx_y,
  random_eff = random_effects_s,
  binary_DV = TRUE,
  ext = "eTable7",
  CI_level = (1-0.05)
)

run_write_models(
  data = aim2_df_no3y,
  list_DVs = "bmi_obesity",
  list_IVs = c("nt_y_stq__sleep_002_phone_outside_room"),
  list_covars = covariates_list_aim2_2y$bmi_obesity,
  random_eff = random_effects_s,
  binary_DV = TRUE,
  ext = "eTable7",
  CI_level = (1-0.05)
)


run_write_models(
  data = aim2_df_no3y,
  list_DVs = "lack_sleep",
  list_IVs = c("nt_y_stq__sleep_002_phone_outside_room"),
  list_covars = covariates_list_aim2$lack_sleep,
  random_eff = random_effects_s,
  binary_DV = TRUE,
  ext = "eTable7",
  CI_level = (1-0.05)
)

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
  list_covars = covariates_list_aim1_sens$depression_dx_y,
  random_eff = random_effects_s,
  list_IVs = "smartphone_ownership",
  binary_DV = TRUE,
  ext = "eTable10",
  CI_level = (1-0.05)
)

run_write_models(
    data = aim1_df,
    list_DVs = "bmi_obesity",
    list_covars = covariates_list_aim1$bmi_obesity,
    random_eff = random_effects_s,
    list_IVs = "smartphone_ownership",
    binary_DV = TRUE,
    ext = "eTable10",
    CI_level = (1-0.05)
)

# eTable 11----
run_write_models(
  data = aim1_df,
  list_DVs = "bpm_T_bin_concern",
  list_covars = covariates_list_aim1_sens$depression_dx_y,
  random_eff = random_effects_s,
  list_IVs = "smartphone_ownership",
  binary_DV = TRUE,
  ext = "eTable11",
  CI_level = (1-0.05)
)

# eTable 12----
run_write_models(
  data = aim1_df,
  list_DVs = "scale(mh_y_bpm_tscore)",
  list_covars = covariates_list_aim1$mh_y_bpm_tscore,
  random_eff = random_effects_s,
  list_IVs = "smartphone_ownership",
  binary_DV = F,
  ext = "eTable12",
  CI_level = (1-0.05)
)

run_write_models(
  data = aim1_df,
  list_DVs = "scale(bmi)",
  list_covars = covariates_list_aim1$bmi,
  random_eff = random_effects_s,
  list_IVs = "smartphone_ownership",
  binary_DV = F,
  ext = "eTable12",
  CI_level = (1-0.05)
)


run_write_models(
  data = aim1_df,
  list_DVs = "scale(sleep_duration_hrs)",
  list_covars = covariates_list_aim1$lack_sleep,
  random_eff = random_effects_s,
  list_IVs = "smartphone_ownership",
  binary_DV = F,
  ext = "eTable12",
  CI_level = (1-0.05)
)

# eTable 13 # Sensitivity 2 exclude 2y----
run_write_models(
  data = aim2_df_no2y,
  list_DVs = "depression_dx_y",
  list_IVs = "scale(schoolyear_total_smartph_wsum)",
  list_covars = covariates_list_aim2_2y$depression_dx_y,
  random_eff = random_effects_s,
  binary_DV = TRUE,
  ext = "eTable13",
  CI_level = (1-0.05)
)

run_write_models(
  data = aim2_df_no2y,
  list_DVs = "bmi_obesity",
  list_IVs = "scale(schoolyear_total_smartph_wsum)",
  list_covars = covariates_list_aim2_2y$bmi_obesity,
  random_eff = random_effects_s,
  binary_DV = TRUE,
  ext = "eTable13",
  CI_level = (1-0.05)
)

run_write_models(
  data = aim2_df_no2y,
  list_DVs = "lack_sleep",
  list_IVs = "scale(schoolyear_total_smartph_wsum)",
  list_covars = covariates_list_aim2_2y$lack_sleep,
  random_eff = random_effects_s,
  binary_DV = TRUE,
  ext = "eTable13",
  CI_level = (1-0.05)
)

# eTable 14 # EValues----
prop.table(table(aim2_df_no3y$depression_dx_y)) * 100 # rare
prop.table(table(aim2_df_no3y$lack_sleep)) * 100 # not rare

as.data.frame(evalues.OR(1.34, 1.07, 1.69, rare = TRUE)) %>% mutate(outcome = "Depression diagnosis") %>% # rare 1.34	1.07 – 1.69
  bind_rows(as.data.frame(evalues.OR(1.35, 1.15, 1.59, rare = FALSE)) %>% mutate(outcome = "Insufficient Sleep")) # not rare 1.35	1.15 - 1.5


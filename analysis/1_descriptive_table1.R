
library(dplyr)
library(tableone)
source("analysis/analysis_utility_fun.R")

aim1_df <- readRDS("data/aim1_df.rds")
aim2_df_no3y <- readRDS("data/aim2_df_no3y.rds")
df_compare <- readRDS("data/df_compare.rds")

# Need to check with Ran how does he wants to present the table 1 with 5 imputed datasets (mean/sd?)


aim1_df$race_other_new <- ifelse( aim1_df$race_other ==1 | aim1_df$race_haw_pac ==1 | aim1_df$race_aian ==1, 1, 0)
df_compare$race_other_new <- ifelse( df_compare$race_other ==1 | df_compare$race_haw_pac ==1 | df_compare$race_aian ==1, 1, 0)

variables <- c(
  "age_br", # Age at 4-year
  "sex_br", "race_multi", "race_white", "race_black", "race_asian", "race_haw_pac", "race_aian", "race_other", "race_other_new",
  "ethnicity_hisp_br", #"parents_high_edu_text_br", 
  "depression_dx_y", "bpm_T_bin_concern", "bmi_obesity", "lack_sleep",
  # Covariates at 3-year
  "age_br_3y", "household_income_3y", "parents_high_edu_text_br_3y", "parents_high_edu_br_master_above_3y",
  "puberty_both_sexes_3y", "late_or_post_puberty_both_sexes_3y", "fc_y_pm_mean_3y",
  # Outcome at 3-year, depression and obesity at 2-year
  "bmi_obesity_3y", "lack_sleep_3y", "bpm_T_bin_concern_3y",
  "depression_dx_y_2y", "bmi_obesity_2y", "sleep_duration_hrs_3y"
)

factors <- c("sex_br", "race_multi", "race_white", "race_black", "race_asian", "race_haw_pac", "race_aian", "race_other", "race_other_new",
             "ethnicity_hisp_br", #"parents_high_edu_text_br", "parents_high_edu_br_master_above",
             "depression_dx_y", "bpm_T_bin_concern", "bmi_obesity", "lack_sleep",
             "parents_high_edu_text_br_3y", "parents_high_edu_br_master_above_3y",
             "puberty_both_sexes_3y", "late_or_post_puberty_both_sexes_3y",
             "bmi_obesity_3y", "lack_sleep_3y", "bpm_T_bin_concern_3y",
             "depression_dx_y_2y", "bmi_obesity_2y")


# Table 1
CreateTableOne(data = aim1_df,
               vars = variables[variables %in% names(aim1_df)],
               strata = "smartphone_ownership",
               factorVars = factors, 
               includeNA = T,
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
               includeNA = T,
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

# Table 1 with survey weights # Might remove later
library(survey)

svy_design <- svydesign(
  ids = ~site_br + family_id_br,
  weights = ~abcd_pps_weight,
  data = aim1_df,
  nest = TRUE
)

table1_svy <- svyCreateTableOne(
  vars = variables,
  strata = "smartphone_ownership",
  data = svy_design,
  factorVars = factors,
  includeNA = TRUE,
  addOverall = TRUE
)

table1_svy_p <- svyCreateTableOne(
  vars = variables,
  strata = "smartphone_ownership",
  data = svy_design,
  factorVars = factors,
  includeNA = F,
  addOverall = TRUE
)


table1_print <- print(
  table1_svy,
  showAllLevels = TRUE,
  quote = FALSE,
  noSpaces = TRUE)

table1_p_print <- print(
  table1_svy_p,
  showAllLevels = TRUE,
  quote = FALSE,
  noSpaces = TRUE)

View(table1_p_print)


table1_df <- print(
  table1_svy,
  showAllLevels = TRUE,
  printToggle = FALSE,
  noSpaces = TRUE,
  test = TRUE,
  smd = TRUE
) %>%
  as.data.frame() %>%
  tibble::rownames_to_column("Variable")


# Descriptive numbers of imputed # May be removed later
aim_dep_list <- readRDS("data/aim_dep_list_non_imp_DV.rds")
aim_obesity_list <- readRDS("data/aim_obesity_list_non_imp_DV.rds")
aim_sleep_list <- readRDS("data/aim_sleep_list_non_imp_DV.rds")

library(dplyr)
library(purrr)
library(tidyr)

table1_MI_wide <- function(df_list,
                           vars_cont = NULL,
                           vars_bin  = NULL,
                           vars_cat  = NULL) {
  
  ## ---- Continuous variables ----
  cont_df <- map_dfr(vars_cont, function(v) {
    
    imp <- map_dfr(
      df_list,
      ~ summarise(.x,
                  mean = mean(.data[[v]], na.rm = TRUE),
                  sd   = sd(.data[[v]], na.rm = TRUE)),
      .id = "imp"
    ) %>% mutate(imp = as.integer(imp))
    
    mean_pool <- mean(imp$mean)
    sd_pool   <- mean(imp$sd)
    
    imp %>%
      pivot_longer(c(mean, sd),
                   names_to = "stat",
                   values_to = "value") %>%
      mutate(col = paste0(stat, "_imp", imp)) %>%
      select(col, value) %>%
      pivot_wider(names_from = col, values_from = value) %>%
      mutate(
        variable = v,
        level = NA_character_,
        type = "continuous",
        mean_pooled = mean_pool,
        sd_pooled   = sd_pool
      ) %>%
      relocate(variable, level, type)
  })
  
  
  ## ---- Binary variables (0/1) ----
  bin_df <- map_dfr(vars_bin, function(v) {
    
    imp <- map_dfr(
      df_list,
      ~ summarise(.x,
                  N   = sum(.data[[v]] == 1, na.rm = TRUE),
                  pct = mean(.data[[v]] == 1, na.rm = TRUE) * 100),
      .id = "imp"
    ) %>% mutate(imp = as.integer(imp))
    
    N_pool   <- round(mean(imp$N))
    pct_pool <- mean(imp$pct)
    
    imp %>%
      pivot_longer(c(N, pct),
                   names_to = "stat",
                   values_to = "value") %>%
      mutate(col = paste0(stat, "_imp", imp)) %>%
      select(col, value) %>%
      pivot_wider(names_from = col, values_from = value) %>%
      mutate(
        variable = v,
        level = "1",
        type = "binary",
        N_pooled   = N_pool,
        pct_pooled = pct_pool
      ) %>%
      relocate(variable, level, type)
  })
  
  
  ## ---- Categorical variables (≥2 levels) ----
  cat_df <- map_dfr(vars_cat, function(v) {
    
    levels_all <- df_list[[1]][[v]] |> na.omit() |> unique() |> sort()
    
    map_dfr(levels_all, function(lvl) {
      
      imp <- map_dfr(
        df_list,
        ~ summarise(.x,
                    N   = sum(.data[[v]] == lvl, na.rm = TRUE),
                    pct = mean(.data[[v]] == lvl, na.rm = TRUE) * 100),
        .id = "imp"
      ) %>% mutate(imp = as.integer(imp))
      
      N_pool   <- round(mean(imp$N))
      pct_pool <- mean(imp$pct)
      
      imp %>%
        pivot_longer(c(N, pct),
                     names_to = "stat",
                     values_to = "value") %>%
        mutate(col = paste0(stat, "_imp", imp)) %>%
        select(col, value) %>%
        pivot_wider(names_from = col, values_from = value) %>%
        mutate(
          variable = v,
          level = as.character(lvl),
          type = "categorical",
          N_pooled   = N_pool,
          pct_pooled = pct_pool
        ) %>%
        relocate(variable, level, type)
    })
  })
  
  bind_rows(cont_df, bin_df, cat_df)
}

aim_dep_dfs <- map(aim_dep_list, "aim1_df")
aim_obesity_dfs <- map(aim_obesity_list, "aim1_df")
aim_sleep_dfs <- map(aim_sleep_list, "aim1_df")

table1_dep <- table1_MI_wide(
  df_list   = aim_dep_dfs,
  vars_cont = c("schoolyear_total_smartph_wsum", "sleep_duration_hrs"),
  vars_bin  = c("bmi_obesity", "depression_dx_y", "lack_sleep"),
)

table1_obesity <- table1_MI_wide(
  df_list   = aim_dep_dfs,
  vars_cont = c("schoolyear_total_smartph_wsum"),
  vars_bin  = c("bmi_obesity")
)

table1_sleep <- table1_MI_wide(
  df_list   = aim_dep_dfs,
  vars_cont = c("schoolyear_total_smartph_wsum", "sleep_duration_hrs"),
  vars_bin  = c("lack_sleep")
)

aim_dep_dfs_2 <- map(aim_dep_list, "aim2_df_no3y")
aim_obesity_dfs_2 <- map(aim_obesity_list, "aim2_df_no3y")
aim_sleep_dfs_2 <- map(aim_sleep_list, "aim2_df_no3y")

table1_dep_2 <- table1_MI_wide(
  df_list   = aim_dep_dfs_2,
  vars_cat  = "schoolyear_total_school_related_work_smartph_wsum_wmean_perday_cat"
)

table1_obesity_2 <- table1_MI_wide(
  df_list   = aim_dep_dfs_2,
  vars_cat  = "schoolyear_total_school_related_work_smartph_wsum_wmean_perday_cat"
)

table1_sleep_2 <- table1_MI_wide(
  df_list   = aim_dep_dfs_2,
  vars_cat  = "schoolyear_total_school_related_work_smartph_wsum_wmean_perday_cat"
)


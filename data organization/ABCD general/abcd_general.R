# library(dplyr)
# library(arrow)
# source("configurations/path_config.R")
# source("data organization/data_utility_fun.R")

get_abcd_general <- function(){

  #########################################################
  ##################### demographics  #####################
  #########################################################
  demographics_df <- read_parquet(file.path(abcd_main_data_path, "ab_p_demo.parquet"))

  # ############ age - from dynamic table
  # demographics_set <- demographics_set %>% mutate(ab_p_demo_age_years_br = floor(ab_p_demo_age))


  ########### parents education
  # combine education across timepoints
  demographics_df <- demographics_df %>%
    mutate(ab_p_demo__edu__slf_temp = coalesce(as.numeric(as.character(ab_p_demo__edu__slf_001__v02)),
                                               as.numeric(as.character(ab_p_demo__edu__slf_001__v01)),
                                               as.numeric(as.character(ab_p_demo__edu__slf_001))),
           ab_p_demo__edu__prtnr_temp = coalesce(as.numeric(as.character(ab_p_demo__edu__prtnr_001__v01)),
                                                 as.numeric(as.character(ab_p_demo__edu__prtnr_001)))) %>%
    mutate(ab_p_demo__edu__slf_temp   = if_else(ab_p_demo__edu__slf_temp %in% c(22, 23), 15, ab_p_demo__edu__slf_temp),
           ab_p_demo__edu__prtnr_temp = if_else(ab_p_demo__edu__prtnr_temp %in% c(22, 23), 15, ab_p_demo__edu__prtnr_temp))

  # remove 777 & 999
  demographics_df <- demographics_df %>%
    mutate(ab_p_demo__edu__slf_temp = case_when(ab_p_demo__edu__slf_temp >= 777 ~ NA_real_, TRUE ~ ab_p_demo__edu__slf_temp),
           ab_p_demo__edu__prtnr_temp = case_when( ab_p_demo__edu__prtnr_temp >= 777 ~ NA_real_,TRUE ~ ab_p_demo__edu__prtnr_temp)
    )

  # max of the 2 parents
  demographics_df <- demographics_df %>%
    mutate(parents_high_edu_br = case_when(
      is.na(ab_p_demo__edu__slf_temp) & is.na(ab_p_demo__edu__prtnr_temp) ~ NA_real_,
      TRUE ~ pmax(ab_p_demo__edu__slf_temp, ab_p_demo__edu__prtnr_temp, na.rm = TRUE)
    ))

  # 13 & 14 are the same
  # 16 and 17 are the same
  demographics_df <- demographics_df %>%
    mutate(parents_high_edu_br = case_when(parents_high_edu_br %in% c(13, 14) ~ 13,
                                           parents_high_edu_br %in% c(16, 17) ~ 16,
                                           TRUE ~ parents_high_edu_br),
           # remove the gap in 14 and 17 values so parents_high_edu_br is a numeric variable with no gaps
           parents_high_edu_br = case_when(parents_high_edu_br >= 17 ~ parents_high_edu_br - 2,
                                           parents_high_edu_br >= 14 ~ parents_high_edu_br - 1,
                                           TRUE ~ parents_high_edu_br)

    )

  demographics_df <- demographics_df %>%
    mutate(parents_high_edu_text_br = case_when(
      parents_high_edu_br <= 12 ~ "highschool_below",
      parents_high_edu_br <= 13 ~ "highschool_graduate",
      parents_high_edu_br < 16 ~ "post_highschool_education",
      parents_high_edu_br == 16 ~ "bachelor",
      parents_high_edu_br > 16 ~ "master_above"
    )) %>% 
    mutate(
  # Create a dummy for master_above
    parents_high_edu_br_master_above = case_when(
      parents_high_edu_br > 16 ~ 1, parents_high_edu_br <= 16 ~ 0, TRUE ~ NA_real_
    ))


  ########### economic hardship
  # Identify economic hardship variables
  var_hardship <- demographics_df %>% select(matches("^ab_p_demo__exp__fam_00[1-7]$")) %>% names()

  # Clean values and compute poverty score
  demographics_df <- demographics_df %>%
    mutate(
      # Convert factor to numeric and replace 777 with NA
      across(
        all_of(var_hardship),
        ~ {
          x <- as.numeric(as.character(.))
          ifelse(x == 777, NA, x)
        }
      ),

      # Compute poverty score: NA if all values are NA, otherwise row sum
      demo_fam_poverty_br = if_else( rowSums(!is.na(across(all_of(var_hardship)))) == 0,
                                     NA_real_,
                                     rowSums(across(all_of(var_hardship)), na.rm = TRUE)
      )
    )


  ########### family income
  demographics_df <- demographics_df %>%
    mutate(household_income = coalesce(ab_p_demo__income__hhold_001, ab_p_demo__income__hhold_001__v01),
           household_income = as.numeric(as.character(household_income)),
           household_income = if_else(household_income >= 777, NA_real_, household_income))


  ########### parents married status
  demographics_df <- demographics_df %>%
    mutate(ab_p_demo__marital__slf_001 = as.numeric(as.character(ab_p_demo__marital__slf_001)),
           ab_p_demo__marital__slf_001 = case_when(ab_p_demo__marital__slf_001 == 777 ~ NA,
                                                   TRUE ~ ab_p_demo__marital__slf_001),
           parents_married = case_when(ab_p_demo__marital__slf_001 > 1 ~ 0, .default = ab_p_demo__marital__slf_001),
           separated_or_divorced = case_when(ab_p_demo__marital__slf_001 %in% c(3, 4) ~ 1,
                                             ab_p_demo__marital__slf_001 < 3 | ab_p_demo__marital__slf_001 > 4 ~ 0,
                                             .default = NA),
           living_with_partner_or_married = case_when(ab_p_demo__marital__slf_001 %in% c(1, 6) ~ 1,
                                                      ab_p_demo__marital__slf_001 > 1 & ab_p_demo__marital__slf_001 < 6 ~ 0,
                                                      .default = NA))

  ########### child race
  race_df = compute_stc__cohort_race(demographics_df)
  
  
  ########### number of household members
  demographics_df <- demographics_df %>%
    # remove outliers
    mutate(ab_p_demo__roster_br = case_when(ab_p_demo__roster_001 <= 0 | ab_p_demo__roster_001 >= 40 ~ NA,
                                            TRUE ~ ab_p_demo__roster_001))




  ##################################################
  ##################### static #####################
  ##################################################
  # static variables between timepoints
  abcd_general_stc <- read_parquet(file.path(abcd_main_data_path, "ab_g_stc.parquet"))

  abcd_general_stc <- abcd_general_stc %>% mutate(
    family_id_br = ab_g_stc__design_id__fam,
    abcd_pps_weight = ab_g_stc__cohort_acsps,
    # ab_g_stc__cohort_dob #DOB

    ethnicity_hisp_br = as.numeric(as.character(ab_g_stc__cohort_ethn)) %% 2,

    # ab_g_stc__cohort_ethnrace__leg # Hispanic ethnicity report outweighs any racial endorements)
    # ab_g_stc__cohort_ethnrace__mblack # providing information on Black identity for multiracial endorements
    # ab_g_stc__cohort_ethnrace__meim # ce_p_meim_001
    # ab_g_stc__cohort_ethnrace__mhisp # providing information on ethnicity for multiracial endorements
    # ab_g_stc__cohort_race__nih # Cohort description: Race (NIH classification reporting 7 levels

    sex_br = as.numeric(as.character(ab_g_stc__cohort_sex)) - 1,
    sex = ifelse(ab_g_stc__cohort_sex == 2, "F", "M")

  )


  ###################################################
  ##################### dynamic #####################
  ###################################################
  # dynamic variables between timepoints
  abcd_general_dyn <- read_parquet(file.path(abcd_main_data_path, "ab_g_dyn.parquet"))

  abcd_general_dyn <- abcd_general_dyn %>% mutate(

    visit_date_br = as.Date(ab_g_dyn__visit_dtt),

    # age_years_br = floor(ab_g_dyn__visit_age),
    age_br = ab_g_dyn__visit_age,

    site_br = ab_g_dyn__design_site,

  )


  ############################################################
  ############################################################

  abcd_basic_static = merge(race_df,
                            abcd_general_stc[ ,c("participant_id", grep("race|_br$|^sex$|weight", colnames(abcd_general_stc), value = T))],
                            all = T)


  # NHB - NHW - HISP # similar to ab_g_stc__cohort_ethnrace__leg and ab_g_stc__cohort_ethnrace__mhisp
  # Unlike us, abcd priorities mix/other over white/black
  abcd_basic_static = abcd_basic_static %>%
    mutate(
      race_ethnicity_3_text_br = case_when(
        race_black == 1 & ethnicity_hisp_br == 0 ~ "NH_Black",
        race_white == 1 & ethnicity_hisp_br == 0 ~ "NH_White",
        ethnicity_hisp_br == 1 ~ "Hispanic",
        .default = NA
      ))


  abcd_basic_dynamic = merge(demographics_df[, grep("parti|session|separated|married$|income$|_br$|above$", colnames(demographics_df), value = T)],
                             abcd_general_dyn[ ,c("participant_id", "session_id", "visit_date_br", "site_br", "age_br")],
                             all = T)

  abcd_basic_dynamic = get_income_to_needs_ratio(abcd_basic_dynamic)


  abcd_basic = merge(abcd_basic_static, abcd_basic_dynamic)


  return(abcd_basic)

}

# library(lubridate)
# library(data.table)
# library(cdcanthro) # install.packages( 'https://raw.github.com/CDC-DNPAO/CDCAnthro/master/cdcanthro_0.1.3.tar.gz', type='source', repos=NULL )
# library(purrr)
# library(ABCDscores) #remotes::install_github("nbdc-datahub/ABCDscores")

# source("configurations/path_config.R")

# this function is based on code from ABCDscores::compute_ab_g_stc__cohort_race__nih
# combine base + long data to create race options.
# doesn't create one sum variable
# ABCD function masks other selection in case one of them was other [priorities longitudinal on other baseline selection]
compute_stc__cohort_race = function(data){

  tmp_data <- purrr::map2_dfc(
    ABCDscores::vars_ab_g_stc__cohort_race__nih$baseline_choice,
    ABCDscores::vars_ab_g_stc__cohort_race__nih$longitudinal_choice,
    # combine baseline and longitudinal data
    function(x, y) {
      ABCDscores::combine_cols(
        data = data,
        col_1 = x,
        col_2 = y,
        keep_other = F
      )
    }
  ) |>
    mutate(
      participant_id = data[["participant_id"]],
      session_id = data[["session_id"]],
      .before = 1
    ) |>
    mutate(
      across(
        ABCDscores::vars_ab_g_stc__cohort_race__nih$baseline_choice,
        ~ as.numeric(as.character(.x))
      )
    ) |>
    mutate(
      # intermediate columns
      tmp_unknown = if_else(
        rowSums(
          across(matches("^ab_p_demo__race_001___[7|9]{3}$"))
        ) >= 1 |
          ab_p_demo__race_001___0 == 1,
        1,
        0
      ),
      race_multi = if_else(
        rowSums(
          across(matches("^ab_p_demo__race_001___(1[0-9]|2[0-4])$"))
        ) >= 2,
        1,
        0
      ),
      race_white = if_else(
        ab_p_demo__race_001___10 == 1,
        1,
        0
      ),
      race_black = if_else(
        ab_p_demo__race_001___11 == 1,
        1,
        0
      ),
      race_asian = if_else(
        ab_p_demo__race_001___18 == 1 |
          ab_p_demo__race_001___19 == 1 |
          ab_p_demo__race_001___20 == 1 |
          ab_p_demo__race_001___21 == 1 |
          ab_p_demo__race_001___22 == 1 |
          ab_p_demo__race_001___23 == 1 |
          ab_p_demo__race_001___24 == 1,
        1,
        0
      ),
      race_haw_pac = if_else(
        ab_p_demo__race_001___14 == 1 |
          ab_p_demo__race_001___15 == 1 |
          ab_p_demo__race_001___16 == 1 |
          ab_p_demo__race_001___17 == 1,
        1,
        0
      ),
      race_aian = if_else(
        ab_p_demo__race_001___12 == 1 |
          ab_p_demo__race_001___13 == 1,
        1,
        0
      ),
      race_other = if_else(
        ab_p_demo__race_001___25 == 1 | tmp_unknown == 1,
        1,
        0
      )
    )


  #ABCDscores::make_static doesnt work. takes only first value
  data_ss = tmp_data %>%
    group_by(participant_id) %>%
    summarise(
      across(starts_with("race"), ~ if_else(any(. == 1, na.rm = TRUE), 1, 0)),
      .groups = "drop"
    )

  return(data_ss)

}

recode_edu <- function(x) {
  x <- as.numeric(x)

  # Set 777, 999, etc. → NA
  x[x >= 777 & !is.na(x)] <- NA

  # Collapse 22,23 → 15
  x[x %in% c(22, 23)] <- 15

  # GED (14) → HS graduate (13)
  x[x == 14] <- 13

  # Associate academic (17) → associate occupational (16)
  x[x == 17] <- 16

  # Shift 15 → 14 and 16 → 15
  x[x == 15] <- 14
  x[x == 16] <- 15

  # Shift higher degrees down by 2 (BA→16, MA→17, etc.)
  idx <- which(!is.na(x) & x >= 18)
  x[idx] <- x[idx] - 2

  return(x)
}



get_income_to_needs_ratio <- function(df) {

  ########### income to needs ratio
  # SES was estimated using the income-to-needs ratio (INR).
  # The INR was calculated by dividing reported household income by the federal poverty threshold for a given household size.
  # A lower INR ratio indicated higher SES.
  # Income was reported in bins and was adjusted to the median for each income bin.
  # We used the 2017 federal poverty level for the corresponding household sizes
  # The U.S. Census Bureau defines “deep poverty” as living in a household with a total cash income below 50 percent of its poverty threshold.

  df_ = df[,c("participant_id", "session_id", "visit_date_br", "household_income", "ab_p_demo__roster_br") ]
  df_$year = year(df_$visit_date_br)

  df_ <- df_ %>%
    mutate(median_income = case_when(
      household_income == 1 ~ median(seq(1, 4999, 1)),
      household_income == 2 ~ median(seq(5000, 11999, 1)),
      household_income == 3 ~ median(seq(12000, 15999, 1)),
      household_income == 4 ~ median(seq(16000, 24999, 1)),
      household_income == 5 ~ median(seq(25000, 34999, 1)),
      household_income == 6 ~ median(seq(35000, 49999, 1)),
      household_income == 7 ~ median(seq(50000, 74999, 1)),
      household_income == 8 ~ median(seq(75000, 99999, 1)),
      household_income == 9 ~ median(seq(100000, 199999, 1)),
      household_income == 10 ~ median(seq(200000, 299999, 1)),
      .default = NA
      # 200000 is higher than the threshold of a household of 20 people (maximum number of household members in ABCD study is 19) (41320 + 12*4180 = 91480)
      # assume the max of group 10 is 299999 so that median is 249999.5 (higher than 200k
    ))

  df_ <- df_ %>%
    # add poverty threshold according to the number of hh members (FPL)
    # https://aspe.hhs.gov/topics/poverty-economic-mobility/poverty-guidelines/prior-hhs-poverty-guidelines-federal-register-references/2017-poverty-guidelines till 21
    # https://aspe.hhs.gov/topics/poverty-economic-mobility/poverty-guidelines
    mutate(pov_threshold = case_when(
      year <= 2017 ~ 12060 + (ab_p_demo__roster_br -1)*4180,
      year == 2018 ~ 12140 + (ab_p_demo__roster_br -1)*4320,
      year == 2019 ~ 12490 + (ab_p_demo__roster_br -1)*4420,
      year == 2020 ~ 12760 + (ab_p_demo__roster_br -1)*4480,
      year == 2021 ~ 12880 + (ab_p_demo__roster_br -1)*4540,
      year == 2022 ~ 13590 + (ab_p_demo__roster_br -1)*4720,
      year == 2023 ~ 14580 + (ab_p_demo__roster_br -1)*5140,
      year == 2024 ~ 15060 + (ab_p_demo__roster_br -1)*5380,
      year >= 2025 ~ 15650 + (ab_p_demo__roster_br -1)*5500,
      .default = NA
    ))

  df_ <- df_ %>%
    mutate(
      income_to_needs_ratio_br = median_income/pov_threshold,
      fam_under_poverty_line_br = case_when(
        median_income < pov_threshold ~ 1,
        median_income >= pov_threshold ~ 0,
        .default = NA
      ))

  df = merge(df, df_[,c("participant_id", "session_id", "income_to_needs_ratio_br", "fam_under_poverty_line_br")])

  return(df)
}


get_BMI_percentiles <- function(df){

  setDT(df)
  # https://www.cdc.gov/growth-chart-training/hcp/computer-programs/sas.html?CDC_AAref_Val=https://www.cdc.gov/nccdphp/dnpao/growthcharts/resources/sas.htm
  df[, bmi_age_months := ph_y_anthr_age*12]

  # BMI
  df[, weight_kg := ph_y_anthr__weight_mean / 2.20462]
  df[, height_cm := ph_y_anthr__height_mean * 2.54]
  df[, bmi := weight_kg / (height_cm /100)^2]


  df_bmi = cdcanthro(df, age = bmi_age_months, wt = weight_kg, ht = height_cm, bmi = bmi, all = F)
  df = merge(df, df_bmi[, c("participant_id", "session_id", "bmip", "bmiz", "bmip95", "mod_bmiz")], all.x = T)

  # Remove outliers based on bmi/mod_bmiz
  df[mod_bmiz < -4 | mod_bmiz > 8, bmi := NA]
  df[mod_bmiz < -4 | mod_bmiz > 8, bmip := NA]
  df[mod_bmiz < -4 | mod_bmiz > 8, bmip95 := NA]

  df[, bmi_obesity := (bmip >= 95) * 1  ]
  df[, bmi_sever_obesity := (bmip95 >= 120) * 1  ]

  return(setDF(df))
}


create_ever_var <- function(data, search_term, new_col_name, NA0is0 = F) { # NA0is0 = F for like suicide
  matching_cols <- grepl(search_term, colnames(data))

  data_df <- data %>%
    mutate(!!new_col_name := as.integer(apply(data[, matching_cols], 1, \(x) any(x == 1))))

  if(NA0is0) {
    data_df <- data_df %>%
      mutate(!!new_col_name := ifelse(is.na(get(new_col_name)) & apply(data[, matching_cols], 1, \(x) any(x == 0)), 0, get(new_col_name)))
  }

  return(data_df)
}



# "For symptom items, we coded them separately if they were deliberately not asked (888)
# because the individual did not answer screening items affirmatively
# (i.e., due to branching logic)"
replace_888_with_0 <- function(df) {
  # Identify columns ending with "_sx", symptoms columns
  symptoms_cols <- grep("_sx$", names(df), value = TRUE)

  # Replace 888 with 0
  df[symptoms_cols] <- lapply(df[symptoms_cols], function(x) replace(x, x == 888, 0))

  # Replace all remaining 888 values with NA (for diagnosis columns)
  df[df == 888| df == 555] <- NA

  # Drop unused factor levels
  df <- droplevels(df)

  return(df)
}

create_diagnosis_summary_var = function(data, NA0is0 ){
  as.factor(apply(data[, grepl("_dx$", colnames(data))], 1,
                  \(x){
                    res = any(x == 1)*1
                    if(NA0is0 & is.na(res) & any(!is.na(x)) ){
                      res = 0
                    }
                    return(res)
                  }
  ))
}

# Helper function to make covariate list for 2 aims
make_cov_list <- function(c1, c2, add_vars, names_covars) {
  setNames(
    map(names_covars, ~ list(
      covars_0 = c1,
      covars_1 = c(c1, add_vars[[.x]]),
      covars_2 = c(c2, add_vars[[.x]])
    )),
    names_covars
  )
}


row_sum_na_safe <- function(df) {

    out <- rowSums(df, na.rm = TRUE)
    out[rowSums(!is.na(df)) == 0] <- NA_real_

    return(out)
}


# Data reading, manipulation and functional programming
library(readr)        # For fast CSV file import (read_csv)
library(dplyr)        # For filtering, selecting, mutating, joining data frames
library(tidyr)        # For handling missing data (drop_na) and reshaping
library(purrr)        # For mapping functions over lists (map, map_dfr)

# Modeling
library(lme4)         # For fitting linear and generalized linear mixed-effects models (lmer, glmer)
library(lavaan)       # For structural equation modeling (SEM), used in mediation analysis

# Model output and reporting
library(sjPlot)       # For creating regression tables from lmer/glmer models (tab_model)
library(broom.mixed)  # For extracting tidy summaries from mixed-effect models (tidy)
library(naniar)  # For extracting tidy summaries from mixed-effect models (tidy)

# Correlation analysis
library(psych)        # For computing mixed-type correlation matrices (mixedCor)
library(Hmisc)        # For calculating p-values for correlations (cor.mtest)
library(corrplot)     # For visualizing correlation matrices


create_ever_var <- function(data, search_term, new_col_name, NA0is0 = FALSE) {

  # Combine multiple search terms if provided
  if (length(search_term) > 0) {
    search_term <- paste0(search_term, collapse = "|")
  }

  # Identify columns matching the pattern
  matching_cols <- grepl(search_term, colnames(data))

  # Compute the 'ever' indicator as a vector
  ever_vector <- as.integer(apply(data[, matching_cols], 1, function(x) any(x == 1)))

  # Add new column to the data frame using dynamic column name
  data[[new_col_name]] <- ever_vector

  # Optional: set NA to 0 if there are any 0s in the row
  if (NA0is0) {
    replace_na_vector <- ifelse(
      is.na(data[[new_col_name]]) & apply(data[, matching_cols], 1, function(x) any(x == 0)),
      0,
      data[[new_col_name]]
    )
    data[[new_col_name]] <- replace_na_vector
  }

  return(data)
}


get_model <- function(data, outcome, predictor, covariates = NULL, random_eff = "(1 | site_id_l_br/rel_family_id/src_subject_id)", control_model = FALSE, binary_DV = FALSE) {

  mod_formula <- stats::reformulate(c(covariates, predictor, random_eff), response = outcome)
  message("Final model formula: ", deparse(mod_formula))

  model <- tryCatch({
    if (binary_DV) {
      return(glmer(mod_formula, data = data, nAGQ = 0, family = "binomial"))
    } else{
      if (control_model) {
        # Use lmerControl with specified parameters
        control <- lmerControl(check.nobs.vs.nlev = "ignore",
                               check.nobs.vs.rankZ = "ignore",
                               check.nobs.vs.nRE = "ignore",
                               optimizer = "bobyqa",
                               optCtrl = list(maxfun = 2e5))
        return(lmer(mod_formula, data = data, control = control))
      } else {
        return(lmer(mod_formula, data = data))
      }
    }

  }, warning = function(w) {
    if (grepl("failed to converge", w$message, ignore.case = TRUE)) {
      if(control_model){
        message("Warning: ", w$message, " - also with control_model = TRUE")
        return()
      }
      message("Warning: ", w$message, " - retrying with control_model = TRUE")
      return(get_model(data, outcome, predictor, covariates, random_eff, control_model = TRUE, binary_DV = binary_DV))
    }
  }, error = function(e){ return()})

  return(model)
}

run_write_models <- function(data,
                             list_DVs,
                             list_covars,
                             random_eff,
                             list_IVs,
                             binary_DV = FALSE,
                             ext = NULL,
                             CI_level = 0.95,
                             write_to_env = FALSE,
                             model_prefix = "mod_") {

  if (is.null(list_IVs)) {
    message("Warning: list_IVs is NULL, models will be run with DVs and covariates only (no IVs).")
  }

  model_counter <- 1

  # Create a global lookup list if it doesn't exist
  if (!exists("model_formula_lookup", envir = .GlobalEnv)) {
    assign("model_formula_lookup", list(), envir = .GlobalEnv)
  }

  all_models <- purrr::map(list_DVs, function(outcome) {

    # 1. Covariates-only models
    covar_only_models <- purrr::map(list_covars, function(covariate) {
      if (!is.character(covariate)) return(NULL)

      model <- get_model(data = data,
                         outcome = outcome,
                         predictor = NULL,
                         covariates = covariate,
                         random_eff = random_eff,
                         control_model = FALSE,
                         binary_DV = binary_DV)

      if (!is.null(model)) {
        model_name <- paste(outcome, paste(covariate, collapse = "_"), random_eff, "no_IV", sep = "__")
        mod_label <- paste0(model_prefix, model_counter)

        if (write_to_env) {
          assign(mod_label, model, envir = .GlobalEnv)
          cat(mod_label, "->", deparse(formula(model)), "\n")
        }

        # Store formula in the global lookup
        model_formula_lookup <- get("model_formula_lookup", envir = .GlobalEnv)
        model_formula_lookup[[mod_label]] <- formula(model)
        assign("model_formula_lookup", model_formula_lookup, envir = .GlobalEnv)

        model_counter <<- model_counter + 1
        return(setNames(list(model), model_name))
      }

      return(NULL)
    })

    # 2. IV-only models
    iv_only_models <- purrr::map(list_IVs, function(IV) {
      model <- get_model(data = data,
                         outcome = outcome,
                         predictor = IV,
                         covariates = NULL,
                         random_eff = random_eff,
                         control_model = FALSE,
                         binary_DV = binary_DV)

      if (!is.null(model)) {
        model_name <- paste(IV, "no_covariates", random_eff, sep = "__")
        mod_label <- paste0(model_prefix, model_counter)

        if (write_to_env) {
          assign(mod_label, model, envir = .GlobalEnv)
          cat(mod_label, "->", deparse(formula(model)), "\n")
        }

        model_formula_lookup <- get("model_formula_lookup", envir = .GlobalEnv)
        model_formula_lookup[[mod_label]] <- formula(model)
        assign("model_formula_lookup", model_formula_lookup, envir = .GlobalEnv)

        model_counter <<- model_counter + 1
        return(setNames(list(model), model_name))
      }

      return(NULL)
    })

    # 3. IV + covariates models
    iv_and_covar_models <- purrr::map(list_IVs, function(IV) {
      purrr::map(list_covars, function(covariate) {
        if (!is.character(covariate)) return(NULL)

        model <- get_model(data = data,
                           outcome = outcome,
                           predictor = IV,
                           covariates = covariate,
                           random_eff = random_eff,
                           control_model = FALSE,
                           binary_DV = binary_DV)

        if (!is.null(model)) {
          model_name <- paste(IV, paste(covariate, collapse = "_"), random_eff, sep = "__")
          mod_label <- paste0(model_prefix, model_counter)

          if (write_to_env) {
            assign(mod_label, model, envir = .GlobalEnv)
            cat(mod_label, "->", deparse(formula(model)), "\n")
          }

          model_formula_lookup <- get("model_formula_lookup", envir = .GlobalEnv)
          model_formula_lookup[[mod_label]] <- formula(model)
          assign("model_formula_lookup", model_formula_lookup, envir = .GlobalEnv)

          model_counter <<- model_counter + 1
          return(setNames(list(model), model_name))
        }

        return(NULL)
      })
    })

    all_outcome_models <- c(covar_only_models, iv_only_models, iv_and_covar_models)
    models <- unlist(all_outcome_models, recursive = TRUE)

    if (length(models) == 0) {
      message("No valid models created for DV: ", outcome)
      return(NULL)
    }

    file_path <- paste0("results/DV__", outcome)
    if (!is.null(ext)) {
      file_path <- paste0(file_path, "__", ext)
    }
    file_path <- paste0(file_path, ".xls")

    tryCatch({
      print(tab_model(models,
                      digits.rsq = 4,
                      digits.p = 5,
                      auto.label = FALSE,
                      collapse.ci = FALSE,
                      file = file_path,
                      show.ci = CI_level))
    }, error = function(e) {
      message("Error in writing models to file: ", e$message)
    })

    return(setNames(list(models), outcome))
  })

  return(unlist(all_models, recursive = FALSE))
}



generate_table1<- function(data, vars, strata, factor_vars = NULL, event_name = NULL, nonnormal_vars = NULL, output_prefix = NULL, digits = 2) {
  # if digits = 1 then we can round later
  # include_na = TRUE to get the % and include_na = FALSE to get p-values

  if (!is.null(event_name)) {
    data <- data %>% filter(eventname == event_name)
  }

  table <- CreateTableOne(data = data,
                          strata = strata,
                          vars = vars,
                          factorVars = factor_vars,
                          includeNA = TRUE,
                          addOverall = TRUE) # for %. mean, median

  table_p_value <- CreateTableOne(data = data,
                                  strata = strata,
                                  vars = vars,
                                  factorVars = factor_vars,
                                  includeNA = FALSE,
                                  addOverall = TRUE) # for p-values



  table_df <- as.data.frame(print(table, missing = TRUE, pDigits = digits, contDigits = digits, catDigits = digits, nonnormal = nonnormal_vars, printToggle= F))
  table_df$variable <- rownames(table_df)
  table_df$variable <- stringr::str_replace_all(table_df$variable, "^X...", "")
  table_df$variable <- trimws(stringr::str_replace_all(table_df$variable, "\\..*", " "))

  table_p_value_df <- as.data.frame(print(table_p_value, missing = TRUE, pDigits = digits, contDigits = digits, catDigits = digits, nonnormal = nonnormal_vars, printToggle= F))
  table_p_value_df$description <- rownames(table_p_value_df)
  table_p_value_df$variable <- trimws(stringr::str_replace_all(table_p_value_df$description, "( =|\\().*", ""))
  colnames(table_p_value_df)[colnames(table_p_value_df) == "p"] = "p_value_no_na"


  table1 = left_join(table_df,
                     table_p_value_df[!is.na(table_p_value_df$description),c("variable", "description", "p_value_no_na")])

  # TODO check what are the options instead of 0 & 1:
  table1 = table1[, c("variable", "description", "Overall", "0", "1", "p", "p_value_no_na", "test", "Missing")]


  if(digits < 2) {
    write.csv(table1,
              paste0("results/table1_", output_prefix, ".csv"), row.names = F)
    return()
  }

  # Get missing % with digits digits
  table_missing <- miss_var_summary(data %>% select(all_of(vars)), digits = digits)
  table_missing$pct_miss = as.numeric(table_missing$pct_miss)
  # TODO print percentages nicely
  table1$variable = stringr::str_replace_all(table1$variable, "\\.[0-9]+", "")
  table1 = left_join(table1,table_missing )
  write.csv(table1, paste0("results/table1_", output_prefix, ".csv"), na = "", row.names = F)
}


make_forest_plot <- function(
    df,
    x = OR, y = Outcome,
    xmin = CI_low, xmax = CI_high,
    color = "darkblue",
    x_breaks = seq(0.5, 2.5, 0.5),
    min_x = NULL,
    max_x = NULL,
    facet_var = NULL,
    title = "",
    y_title = NULL) {

    min_x =  ifelse(is.null(min_x), min(x_breaks), min_x)
    max_x =  ifelse(is.null(max_x), max(x_breaks), max_x)

  p <- ggplot(df, aes(x = {{ x }}, y = {{ y }})) +
    geom_vline(xintercept = 1, linetype = "dashed",
               color = "#B22222", linewidth = 1.3) +
    geom_errorbarh(aes(xmin = {{ xmin }}, xmax = {{ xmax }}),
                   height = 0.15, size = 1.3, color = color) +
    geom_point(size = 5, color = color) +
    scale_x_continuous(
      name = "Odds Ratio (95% Confidence Interval)",
      breaks = x_breaks,
      limits = c(min_x, max_x)
    ) +
    labs(title = title, y = y_title) +
    theme_abcd()

  if (!is.null(facet_var)) {
    p <- p + facet_wrap(vars({{ facet_var }}), ncol = 1, scales = "free_y") +
      theme(strip.text = element_text(size = 20, face = "bold"))
  }
  p
}

prep_forest_activities <- function(df, outcome_levels, top_predictor = "Social media") {

  df %>%
    mutate(
      Outcome = factor(Outcome, levels = outcome_levels),
      sort_key = ifelse(Predictor == top_predictor, Inf, OR)
    ) %>%
    group_by(Outcome) %>%
    arrange(desc(sort_key), .by_group = TRUE) %>%
    mutate(Predictor = factor(Predictor, levels = rev(unique(Predictor)))) %>%
    ungroup() %>%
    select(-sort_key)
}


make_forest_plot_activities <- function(
    df,
    x = OR, y = Predictor,
    xmin = CI_low, xmax = CI_high,
    color = "darkblue",
    x_breaks = seq(0.6, 1.8, 0.2),
    facet_var = Outcome,
    title = "") {

  ggplot(df, aes(x = {{ x }}, y = {{ y }})) +
    geom_vline(xintercept = 1, linetype = "dashed",
               color = "#B22222", linewidth = 1.3) +
    geom_errorbarh(aes(xmin = {{ xmin }}, xmax = {{ xmax }}),
                   height = 0.15, size = 1.3, color = color) +
    geom_point(size = 5, color = color) +
    scale_x_continuous(
      name = "Odds Ratio (95% Confidence Interval)",
      breaks = x_breaks,
      limits = c(min(x_breaks), max(x_breaks))
    ) +
    labs(title = title, y = "") +
    theme_abcd() +
    facet_wrap(vars({{ facet_var }}), ncol = 1, scales = "free_y") +
    theme(strip.text = element_text(size = 20, face = "bold"))
}


make_ci_barplot <- function(
    df,
    x = Category,
    y = mean,
    lower = CI_lower,
    upper = CI_upper,
    fill_palette = "Set2",
    ylab = "Hours/week (Mean & 95% CI)") {

  ggplot(df, aes(x = {{ x }}, y = {{ y }}, fill = {{ x }})) +
    geom_col(width = 0.65, color = "white") +
    geom_errorbar(aes(ymin = {{ lower }}, ymax = {{ upper }}),
                  width = 0.2, size = 1.0) +
    scale_fill_brewer(palette = fill_palette) +
    labs(x = "", y = ylab) +
    theme_abcd() +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = 45, hjust = 1, size = 14, face = "bold"),
      axis.title.y = element_text(size = 22, face = "bold")
    )
}

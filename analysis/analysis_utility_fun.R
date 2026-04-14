
# Data reading, manipulation and functional programming
library(readr)        # For fast CSV file import (read_csv)
library(dplyr)        # For filtering, selecting, mutating, joining data frames
library(tidyr)        # For handling missing data (drop_na) and reshaping
library(purrr)        # For mapping functions over lists (map, map_dfr)

# Modeling
library(lme4)         # For fitting linear and generalized linear mixed-effects models (lmer, glmer)

# Model output and reporting
library(sjPlot)       # For creating regression tables from lmer/glmer models (tab_model)
library(broom.mixed)  # For extracting tidy summaries from mixed-effect models (tidy)
library(naniar)  # For extracting tidy summaries from mixed-effect models (tidy)

# Correlation analysis
library(psych)        # For computing mixed-type correlation matrices (mixedCor)
library(Hmisc)        # For calculating p-values for correlations (cor.mtest)

# pooling results from multiple imputation
library(tidyverse)
library(mice)
library(rvest)
library(openxlsx)



get_model <- function(data,
                      outcome,
                      predictor,
                      covariates = NULL,
                      random_eff = "(1 | site_id_l_br/rel_family_id/src_subject_id)",
                      control_model = FALSE,
                      binary_DV = FALSE) {
  mod_formula <- stats::reformulate(c(covariates, predictor, random_eff), response = outcome)
  message("Final model formula: ", deparse(mod_formula))
  
  model <- tryCatch({
    if (binary_DV) {
      return(glmer(
        mod_formula,
        data = data,
        nAGQ = 0,
        family = "binomial"
      ))
    } else{
      if (control_model) {
        # Use lmerControl with specified parameters
        control <- lmerControl(
          check.nobs.vs.nlev = "ignore",
          check.nobs.vs.rankZ = "ignore",
          check.nobs.vs.nRE = "ignore",
          optimizer = "bobyqa",
          optCtrl = list(maxfun = 2e5)
        )
        return(lmer(mod_formula, data = data, control = control))
      } else {
        return(lmer(mod_formula, data = data))
      }
    }
    
  }, warning = function(w) {
    if (grepl("failed to converge", w$message, ignore.case = TRUE)) {
      if (control_model) {
        message("Warning: ", w$message, " - also with control_model = TRUE")
        return()
      }
      message("Warning: ",
              w$message,
              " - retrying with control_model = TRUE")
      return(
        get_model(
          data,
          outcome,
          predictor,
          covariates,
          random_eff,
          control_model = TRUE,
          binary_DV = binary_DV
        )
      )
    }
  }, error = function(e) {
    return()
  })
  
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
      if (!is.character(covariate))
        return(NULL)
      
      model <- get_model(
        data = data,
        outcome = outcome,
        predictor = NULL,
        covariates = covariate,
        random_eff = random_eff,
        control_model = FALSE,
        binary_DV = binary_DV
      )
      
      if (!is.null(model)) {
        model_name <- paste(outcome,
                            paste(covariate, collapse = "_"),
                            random_eff,
                            "no_IV",
                            sep = "__")
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
      model <- get_model(
        data = data,
        outcome = outcome,
        predictor = IV,
        covariates = NULL,
        random_eff = random_eff,
        control_model = FALSE,
        binary_DV = binary_DV
      )
      
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
        if (!is.character(covariate))
          return(NULL)
        
        model <- get_model(
          data = data,
          outcome = outcome,
          predictor = IV,
          covariates = covariate,
          random_eff = random_eff,
          control_model = FALSE,
          binary_DV = binary_DV
        )
        
        if (!is.null(model)) {
          model_name <- paste(IV, paste(covariate, collapse = "_"), random_eff, sep = "__")
          mod_label <- paste0(model_prefix, model_counter)
          
          if (write_to_env) {
            assign(mod_label, model, envir = .GlobalEnv)
            cat(mod_label, "->", deparse(formula(model)), "\n")
          }
          
          model_formula_lookup <- get("model_formula_lookup", envir = .GlobalEnv)
          model_formula_lookup[[mod_label]] <- formula(model)
          assign("model_formula_lookup",
                 model_formula_lookup,
                 envir = .GlobalEnv)
          
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
      print(
        tab_model(
          models,
          digits.rsq = 4,
          digits.p = 5,
          auto.label = FALSE,
          collapse.ci = FALSE,
          file = file_path,
          show.ci = CI_level
        )
      )
    }, error = function(e) {
      message("Error in writing models to file: ", e$message)
    })
    
    return(setNames(list(models), outcome))
  })
  
  return(unlist(all_models, recursive = FALSE))
}




make_forest_plot <- function(df,
                             x,
                             y,
                             xmin,
                             xmax,
                             n_var = NULL,
                             min_x = 0.4,
                             max_x = 2.3,
                             x_breaks = NULL,
                             x_label = "Odds Ratio (95% Confidence Interval)",
                             y_label = NULL,
                             title = "") {
  p <- ggplot(df, aes({{ x }}, factor({{ y }}, levels = rev(unique({{ y }}))))) +
    geom_vline(
      xintercept = 1,
      linetype = "dashed",
      color = "#B22222",
      linewidth = 1.2
    ) +
    geom_errorbarh(
      aes(xmin = {{ xmin }}, xmax = {{ xmax }}),
      height = 0.15,
      size = 1.2,
      color = "darkblue"
    ) +
    geom_point(size = 5, color = "darkblue") +
    scale_x_continuous(
      name = x_label,
      breaks = x_breaks,
      limits = c(min_x, max_x * 1.2)
    ) +
    labs(title = title, y = y_label) +
    theme_abcd()
  
  if (!is.null(substitute(n_var)))
    p <- p +
      geom_text(
        aes(label = paste0("n = ", {{ n_var }})),
        x = max_x * 1.005,
        hjust = 0,
        size = 8,
        fontface = "bold"
      )
  
  p
}




# https://rdrr.io/cran/mice/src/R/pool.vector.R
# Rubin's rules for scalar estimates
pool_MI <- function(input_table, digits = 4, is_OR = T) {
  long_df <- input_table %>%
    pivot_longer(
      cols = -outcome,
      names_to = c(".value", "imp"),
      names_pattern = "(OR|L|U)_(\\d+)"
    ) %>%
    mutate(imp = as.numeric(imp))
  
  pool_rubin <- function(df) {
    df <- df %>%
      mutate(
        logOR = ifelse(is_OR,log(OR),OR),
        SE = ifelse(is_OR, (log(U) - log(L)) / (2 * 1.96), (U - L) / (2 * 1.96)),
        VAR = SE^2
      )
    
    m <- nrow(df)

    # Generate the pooled results
    res <- mice:::pool.scalar(df$logOR, df$VAR)
    
    t_stat = res$qbar / sqrt(res$t)
    p <- 2 * pt(abs(t_stat), res$df, lower.tail = FALSE)
    
    CI_lower = res$qbar - qt(0.975, res$df) * sqrt(res$t)
    CI_upper = res$qbar + qt(0.975, res$df) * sqrt(res$t)
    
    data.frame(
      OR = ifelse(is_OR, exp(res$qbar), res$qbar),
      CI_lower = ifelse(is_OR, exp(CI_lower), CI_lower),
      CI_upper = ifelse(is_OR, exp(CI_upper), CI_upper),
      p = p
    )
  }
  
  final_results <- long_df %>%
    group_by(outcome) %>%
    group_modify( ~ pool_rubin(.x)) %>%
    ungroup()
  
  final_results <- final_results %>%
    mutate(
      OR = round(OR, digits),
      CI_lower = round(CI_lower, digits),
      CI_upper = round(CI_upper, digits),
      CI = ifelse(!is.na(CI_lower), paste0(CI_lower, "–", CI_upper), NA),
      p = ifelse(p < 0.001, "<0.001", round(p, 4))
    ) %>%
    select(outcome, OR, CI, p)
  
  return(final_results)
}



calculate_pool_MI_from_table <- function(file_prefix, M=5, path = "results", is_OR=T) {
  files <- paste0(file_prefix, 1:M, ".xls")
  
  #1. READ FILES
  read_html_excel <- function(file) {
    doc <- read_html(file)
    tabs <- html_table(doc, fill = TRUE)
    as.data.frame(tabs[[1]])
  }
  tables <- map(file.path(path, files), read_html_excel)
  
  #2. CLEAN EACH TABLE
  clean_table <- function(df) {
    colnames(df) <- as.character(df[1, ])
    df <- df[-c(1, 2), ]
    
    randomEffects_location = which(df$Predictors == "Random Effects")
    observations_location = which(df$Predictors == "Observations")
    
    df <- df[-c(
      as.numeric(randomEffects_location):(observations_location - 1),
      observations_location + 1
    ), ]
    
    rownames(df) <- df$Predictors
    df$Predictors <- NULL
    
    return(df)
  }
  tables <- map(tables, clean_table)
  
  #3. TEMPLATE (OUTPUT STRUCTURE)
  final_table <- tables[[1]]
  final_table[] = NA
  
  or_cols <- which(str_detect(colnames(final_table), "Odds|Estimates"))
  ci_cols <- which(str_detect(colnames(final_table), "CI"))
  p_cols <- which(str_detect(colnames(final_table), "^p"))
  
  n_models <- length(or_cols)
  
  #4. LOOP OVER MODELS
  for (k in seq_len(n_models)) {
    # ---- build pooled input table for model k ----
    pooled_input <- map2_dfc(tables, seq_along(tables), function(df, imp) {
      
      ci_string <- df[[ci_cols[k]]]
      nums <- str_extract_all(ci_string, "-?[0-9\\.]+")
      
      L_vals <- map_dbl(nums, ~ as.numeric(.x[1]))
      U_vals <- map_dbl(nums, ~ as.numeric(.x[2]))
      
      res = tibble(
        !!paste0("OR_", imp) := as.numeric(df[[or_cols[k]]]),
        !!paste0("L_", imp) := as.numeric(L_vals),
        !!paste0("U_", imp) := as.numeric(U_vals)
      )
    })
    pooled_input <- bind_cols(outcome = rownames(final_table), pooled_input)
    
    pooled_res <- pool_MI(pooled_input, is_OR=is_OR)
    
    #write results back into template
    final_table[pooled_res$outcome, or_cols[k]] <- pooled_res$OR
    final_table[pooled_res$outcome, ci_cols[k]] <- pooled_res$CI
    final_table[pooled_res$outcome, p_cols[k]]  <- pooled_res$p
  }
  
  # Export
  out_path <- file.path(path, paste0(file_prefix, "_POOLED.xlsx"))
  write.xlsx(cbind(rownames(final_table), final_table), out_path, overwrite = TRUE)
  
}

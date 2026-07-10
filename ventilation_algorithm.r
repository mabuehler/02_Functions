
#######################################################
#######################################################
#####                                             #####
#####    Different functions for the algorithm    #####
#####                                             #####
#######################################################
#######################################################

library(data.table)
library(lmtest)
library(ranger)
library(car)



test_linearity <- function(data, pred = 'AER', var = c('mean_wind_speed', 'Temp_K', 'diff_WD'), place = c("Odder", "Gjerlev", "Lindå", "Auning", "Hobro"), 
                           type = c('Control', 'Acid'), summary = FALSE) {
 # browser() 
  # 1. Filter Data
  dt <- data[Place %in% place & Type %in% type]
  
  # 2. Build Formula
  fmla_str <- paste(pred, "~", paste(var, collapse = " + "))
  fmla <- as.formula(fmla_str)
  
  # 3. Define Combinations
  combos <- CJ(place = place, type = type, unique = TRUE)
  results <- list()
  
  # 4. Loop
  for (i in 1:nrow(combos)) {
    p <- combos$place[i]
    t <- combos$type[i]
    
    dt_sub <- dt[Place == p & Type == t]

    if (nrow(dt_sub) < 10) {
      message(sprintf("Skipping %s/%s: Insufficient data (%d rows)", p, t, nrow(dt_sub)))
      next
    }
    
    # Fit Model
    model <- tryCatch({
      lm(fmla, data = dt_sub)
    }, error = function(e) {
      warning(sprintf("Failed to fit model for %s/%s: %s", p, t, e$message))
      return(NULL)
    })
    
    if (is.null(model)) next
    
    # RESET Test
    reset_res <- tryCatch({
      resettest(model, power = 2:3, type = "regressor")
    }, error = function(e) {
      warning(sprintf("RESET test failed for %s/%s", p, t))
      return(NULL)
    })
    
    if (is.null(reset_res)) next
    
    p_val <- reset_res$p.value
    
    # LOGIC FLIP: 
    # If p >= 0.05, we fail to reject linearity -> Is_Linear = TRUE
    # If p < 0.05, we reject linearity -> Is_Linear = FALSE
    is_linear <- p_val >= 0.05
    
    status_label <- ifelse(is_linear, "Linear OK", "Non-Linear")
    
    results[[paste(p, t, sep = "_")]] <- list(
      place = p,
      type = t,
      n_obs = nrow(dt_sub),
      model = model,
      reset_p_value = p_val,
      is_linear = is_linear,
      status = status_label,
      formula = fmla_str
    )
  }
  
  # 5. Create Summary Table
  if (length(results) > 0) {
    summary_df <- rbindlist(lapply(results, function(x) {
      data.table(
        Place = x$place,
        Type = x$type,
        N_Obs = x$n_obs,
        P_Value = round(x$reset_p_value, 4),
        Is_Linear = x$is_linear,
        Status = x$status
      )
    }))
    

    
    cat("\n--- Linearity Test Summary ---\n")
    cat("Legend: 'Linear OK' = Linear model assumption holds (p >= 0.05)\n")
    print(summary_df)
    
    if(summary) {
      # Return both summary and models
      return(list(
        summary = summary_df,
        models = results
      ))
    }
  } else {
    warning("No valid models were fitted.")
    return(NULL)
  }
}


test_individual_linearity <- function(data, pred = 'AER', var = c('mean_wind_speed', 'Temp_K', 'diff_WD'), place = c("Odder", "Gjerlev", "Lindå", "Auning", "Hobro"), 
                           type = c('Control', 'Acid'), summary = FALSE) {
    
  # 1. Filter Data
  dt <- data[Place %in% place & Type %in% type]
  
  # 2. Define Combinations
  combos <- CJ(place = place, type = type, unique = TRUE)
  results <- list()
  
  # 3. Loop through combinations
  for (i in 1:nrow(combos)) {
    p <- combos$place[i]
    t <- combos$type[i]
    dt_sub <- dt[Place == p & Type == t]
    
    if (nrow(dt_sub) < 10) {
      message(sprintf("Skipping %s/%s: Insufficient data (%d rows)", p, t, nrow(dt_sub)))
      next
    }
    
    # 4. Test EACH variable separately
    var_results <- list()
    for (v in var) {
      # Skip if variable doesn't exist
      if (!v %in% names(dt_sub)) next
      # Fit simple bivariate model
      fmla_str <- paste(pred, "~", v)
      fmla <- as.formula(fmla_str)
      
      model <- tryCatch({
        lm(fmla, data = dt_sub)
      }, error = function(e) {
        warning(sprintf("Failed to fit model for %s/%s/%s: %s", p, t, v, e$message))
        return(NULL)
      })
      
      if (is.null(model)) next
      
      # RESET test
      reset_res <- tryCatch({
        resettest(model, power = 2:3, type = "regressor")
      }, error = function(e) {
        warning(sprintf("RESET test failed for %s/%s/%s", p, t, v))
        return(NULL)
      })
      
      if (is.null(reset_res)) next
      
      p_val <- reset_res$p.value
      is_linear <- p_val >= 0.05
      
      var_results[[v]] <- list(
        variable = v,
        model = model,
        reset_p_value = p_val,
        is_linear = is_linear,
        r_squared = summary(model)$r.squared
      )
    }
    
    # Store all variable results for this combination
    results[[paste(p, t, sep = "_")]] <- list(
      place = p,
      type = t,
      n_obs = nrow(dt_sub),
      variable_results = var_results
    )
  }
  
  # 5. Create Summary Table
  if (length(results) > 0) {
    summary_list <- lapply(results, function(x) {
      if (length(x$variable_results) == 0) return(NULL)
      
      lapply(names(x$variable_results), function(v) {
        vr <- x$variable_results[[v]]
        data.table(
          Place = x$place,
          Type = x$type,
          Variable = v,
          N_Obs = x$n_obs,
          P_Value = round(vr$reset_p_value, 4),
          Is_Linear = vr$is_linear,
          R_Squared = round(vr$r_squared, 4)
        )
      })
    })
    
    summary_df <- rbindlist(unlist(summary_list, recursive = FALSE))
    
    cat("\n--- Individual Variable Linearity Test Summary ---\n")
    cat("Legend: 'Is_Linear = TRUE' means RESET test did NOT reject linearity (p >= 0.05)\n")
    cat("Higher R-squared indicates stronger linear relationship\n\n")
    print(summary_df)
    
    if(summary) {
      return(list(
        summary = summary_df,
        models = results
      ))
    }
  } else {
    warning("No valid models were fitted.")
    return(NULL)
  }
}




non_lin_reg <- function(
    data,
    pred = "AER",
    var = c("mean_wind_speed", "Temp_K", "diff_WD"),
    place = c("Odder", "Gjerlev", "Lindå", "Auning", "Hobro"),
    type = c("Control", "Acid"),
    rounds = 1:3,
    min_samples = 20,
    degree = 2,
    interactions = TRUE
) {

  library(data.table)
  library(car)

  # ------------------------- #
  # Filter base data
  # ------------------------- #

  dt <- copy(
    data[
      Place %in% place &
      Type %in% type &
      round %in% rounds
    ]
  )

  # ------------------------- #
  # Formula builder
  # ------------------------- #

  build_formula <- function(
      pred,
      vars,
      degree = 2,
      interactions = TRUE
  ) {

    terms <- vars

    # Polynomial terms
    if (degree >= 2) {

      poly_terms <- unlist(
        lapply(2:degree, function(d) {
          paste0("I(", vars, "^", d, ")")
        })
      )

      terms <- c(terms, poly_terms)
    }

    # Interactions
    if (interactions && length(vars) > 1) {

      inter_terms <- combn(
        vars,
        2,
        FUN = function(x) {
          paste0(x[1], ":", x[2])
        }
      )

      terms <- c(terms, inter_terms)
    }

    as.formula(
      paste(pred, "~", paste(terms, collapse = " + "))
    )
  }

  # ------------------------- #
  # Combinations
  # ------------------------- #

  combos <- CJ(
    place = place,
    type = type,
    round = rounds,
    unique = TRUE
  )

  # Count usable samples
  combos[, n := mapply(function(p, t, r) {

    dt_sub <- dt[
      Place == p &
      Type == t &
      round == r
    ]

    dt_sub <- na.omit(
      dt_sub[, c(pred, var), with = FALSE]
    )

    nrow(dt_sub)

  }, place, type, round)]

  combos_valid <- combos[n >= min_samples]

  cat("\nCombinations tested:\n")
  print(combos_valid, nrows = Inf)

  # ------------------------- #
  # Fit function
  # ------------------------- #

  fit_one <- function(p, t, r) {

    dt_sub <- dt[
      Place == p &
      Type == t &
      round == r
    ]

    dt_sub <- na.omit(
      dt_sub[, c(pred, var), with = FALSE]
    )

    if (nrow(dt_sub) < min_samples)
      return(NULL)

    # ------------------------- #
    # Center predictors
    # ------------------------- #

    means <- dt_sub[
      ,
      lapply(.SD, mean),
      .SDcols = var
    ]

    for (v in var) {

      dt_sub[
        ,
        (v) := get(v) - means[[v]]
      ]
    }

    # ------------------------- #
    # Build formula
    # ------------------------- #

    formula_obj <- build_formula(
      pred = pred,
      vars = var,
      degree = degree,
      interactions = interactions
    )

    # ------------------------- #
    # Fit model
    # ------------------------- #

    model <- lm(
      formula_obj,
      data = dt_sub
    )

    s <- summary(model)

    rmse <- sqrt(
      mean(residuals(model)^2)
    )

    vif_terms <- tryCatch(
      suppressWarnings(
        round(vif(model), 2)
      ),
      error = function(e) NA
    )

    # ------------------------- #
    # Simple flags
    # ------------------------- #

    flags <- c()

    if (s$adj.r.squared < 0.3)
      flags <- c(flags, "LOW_R2")

    if (any(vif_terms > 10, na.rm = TRUE))
      flags <- c(flags, "HIGH_VIF")

    # ------------------------- #
    # Output
    # ------------------------- #

    list(
      place = p,
      type = t,
      round = r,
      n = nrow(dt_sub),
      means = means,
      formula = formula_obj,
      coefficients = coef(model),
      model = model,
      summary = s,
      adj_r2 = s$adj.r.squared,
      rmse = rmse,
      vif_terms = vif_terms,
      flags = flags
    )
  }

  # ------------------------- #
  # Run models
  # ------------------------- #

  results <- lapply(
    1:nrow(combos_valid),
    function(i) {

      fit_one(
        combos_valid$place[i],
        combos_valid$type[i],
        combos_valid$round[i]
      )
    }
  )

  results <- Filter(
    Negate(is.null),
    results
  )

  return(results)
}


score_model <- function(m) {

  max_vif <- max(m$vif_terms, na.rm = TRUE)

  score <- 0

  # R2
  if (m$adj_r2 > 0.9) score <- score + 3
  else if (m$adj_r2 > 0.8) score <- score + 2
  else if (m$adj_r2 > 0.7) score <- score + 1

  # RMSE
  if (m$rmse < 0.003) score <- score + 2
  else if (m$rmse < 0.006) score <- score + 1

  # VIF penalty
  if (max_vif > 100) score <- score - 4
  else if (max_vif > 30) score <- score - 3
  else if (max_vif > 10) score <- score - 1

  # sample size
  if (m$n < 25) score <- score - 2
  else if (m$n < 40) score <- score - 1

  classification <- dplyr::case_when(
    score >= 4 ~ "GOOD",
    score >= 2 ~ "ACCEPTABLE",
    score >= 0 ~ "QUESTIONABLE",
    TRUE ~ "REJECT"
  )

  data.frame(
    place = m$place,
    type = m$type,
    round = m$round,
    n = m$n,
    adj_r2 = m$adj_r2,
    rmse = m$rmse,
    max_vif = max_vif,
    score = score,
    classification = classification
  )
}
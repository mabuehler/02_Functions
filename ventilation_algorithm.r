
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




RMSE <- function(x_real, x_model){
  s2 = (x_real - x_model)^2
  s2 <- s2[!is.na(s2)]
  s <- sum(sqrt(s2) / length(s2))
  return(s)
}


test_linearity <- function(data, pred = 'AER', var = c('mean_wind_speed', 'Temp_K', 'diff_WD'), place = c("Odder", "Gjerlev", "Lindå", "Auning", "Hobro"), 
                           type = c('Control', 'Acid'), position = c('Position 1', 'Position 2'), summary = FALSE) {
  
  # 1. Filter Data
  dt <- data[Place %in% place & Type %in% type & Measurement_point %in% position]
  
  # 2. Build Formula
  fmla_str <- paste(pred, "~", paste(var, collapse = " + "))
  fmla <- as.formula(fmla_str)
  
  # 3. Define Combinations
  combos <- CJ(place = place, type = type, position = position, unique = TRUE)
  results <- list()
  
  # 4. Loop
  for (i in 1:nrow(combos)) {
    p <- combos$place[i]
    t <- combos$type[i]
    pos <- combos$position[i]
    
    dt_sub <- dt[Place == p & Type == t & Measurement_point == pos]
    
    if (nrow(dt_sub) < 10) {
      message(sprintf("Skipping %s/%s/%s: Insufficient data (%d rows)", p, t, pos, nrow(dt_sub)))
      next
    }
    
    # Fit Model
    model <- tryCatch({
      lm(fmla, data = dt_sub)
    }, error = function(e) {
      warning(sprintf("Failed to fit model for %s/%s/%s: %s", p, t, pos, e$message))
      return(NULL)
    })
    
    if (is.null(model)) next
    
    # RESET Test
    reset_res <- tryCatch({
      resettest(model, power = 2:3, type = "regressor")
    }, error = function(e) {
      warning(sprintf("RESET test failed for %s/%s/%s", p, t, pos))
      return(NULL)
    })
    
    if (is.null(reset_res)) next
    
    p_val <- reset_res$p.value
    
    # LOGIC FLIP: 
    # If p >= 0.05, we fail to reject linearity -> Is_Linear = TRUE
    # If p < 0.05, we reject linearity -> Is_Linear = FALSE
    is_linear <- p_val >= 0.05
    
    status_label <- ifelse(is_linear, "Linear OK", "Non-Linear")
    
    results[[paste(p, t, pos, sep = "_")]] <- list(
      place = p,
      type = t,
      position = pos,
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
        Position = x$position,
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
                           type = c('Control', 'Acid'), position = c('Position 1', 'Position 2'), summary = FALSE) {
    
  # 1. Filter Data
  dt <- data[Place %in% place & Type %in% type & Measurement_point %in% position]
  
  # 2. Define Combinations
  combos <- CJ(place = place, type = type, position = position, unique = TRUE)
  results <- list()
  
  # 3. Loop through combinations
  for (i in 1:nrow(combos)) {
    p <- combos$place[i]
    t <- combos$type[i]
    pos <- combos$position[i]
    dt_sub <- dt[Place == p & Type == t & Measurement_point == pos]
    
    if (nrow(dt_sub) < 10) {
      message(sprintf("Skipping %s/%s/%s: Insufficient data (%d rows)", p, t, pos, nrow(dt_sub)))
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
        warning(sprintf("Failed to fit model for %s/%s/%s/%s: %s", p, t, pos, v, e$message))
        return(NULL)
      })
      
      if (is.null(model)) next
      
      # RESET test
      reset_res <- tryCatch({
        resettest(model, power = 2:3, type = "regressor")
      }, error = function(e) {
        warning(sprintf("RESET test failed for %s/%s/%s/%s", p, t, pos, v))
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
    results[[paste(p, t, pos, sep = "_")]] <- list(
      place = p,
      type = t,
      position = pos,
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
          Position = x$position,
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





# build_ensemble_models <- function(data, pred = 'AER', var = c('mean_wind_speed', 'Temp_K', 'diff_WD'), place = c("Odder", "Gjerlev", "Lindå", "Auning", "Hobro"), 
#                            type = c('Control', 'Acid'), position = c('Position 1', 'Position 2'), min_samples = 10) {
#   # 1. Filter Data
#   dt <- copy(data[Place %in% place & Type %in% type & Measurement_point %in% position])
  
#   # 2. Define Combinations
#   combos <- CJ(place = place, type = type, position = position, unique = TRUE)
#   results <- list()
    
#   # 3. Loop
#   for (i in 1:nrow(combos)) {
#     p <- combos$place[i]
#     t <- combos$type[i]
#     pos <- combos$position[i]
    
#     dt_sub <- dt[Place == p & Type == t & Measurement_point == pos]
    
#     # Check data sufficiency (RF needs more data than LM)
#     if (nrow(dt_sub) < min_samples) {
#       message(sprintf("Skipping %s/%s/%s: Insufficient data (%d rows)", p, t, pos, nrow(dt_sub)))
#       next
#     }
    
#     # Check if predictor columns exist
#     vars_to_use <- c('mean_wind_speed', 'Temp_K', 'diff_WD')
#     if (!all(vars_to_use %in% names(dt_sub))) {
#       warning(sprintf("Missing predictors for %s/%s/%s", p, t, pos))
#       next
#     }
    
#     # FIX: Ensure the target column is in the data passed to ranger
#     # We select predictors AND the target column
#     cols_to_pass <- c(vars_to_use, pred)
    
#     # Train Random Forest
#     model <- tryCatch({
#       ranger(
#         formula = as.formula(paste(pred, "~", paste(vars_to_use, collapse = " + "))),
#         data = dt_sub[, ..cols_to_pass], # Include BOTH predictors and target
#         num.trees = 100,
#         min.node.size = 5,
#         importance = "impurity"
#       )
#     }, error = function(e) {
#       warning(sprintf("Failed to train RF for %s/%s/%s: %s", p, t, pos, e$message))
#       return(NULL)
#     })
    
#     if (is.null(model)) next
    
#     # Calculate Performance
#     # Now we can predict because the model was trained correctly
#     preds <- predict(model, data = dt_sub[, ..vars_to_use])$predictions
#     actuals <- dt_sub[[pred]]
    
#     ss_res <- sum((actuals - preds)^2)
#     ss_tot <- sum((actuals - mean(actuals))^2)
#     r_sq <- 1 - (ss_res / ss_tot)
#     rmse <- sqrt(mean((actuals - preds)^2))
    
#     # Store Results
#     results[[paste(p, t, pos, sep = "_")]] <- list(
#       place = p,
#       type = t,
#       position = pos,
#       n_obs = nrow(dt_sub),
#       model = model,
#       r_squared = r_sq,
#       rmse = rmse
#     )
#   }
  
#   # 4. Create Summary
#   if (length(results) > 0) {
#     summary_df <- rbindlist(lapply(results, function(x) {
#       data.table(
#         Place = x$place,
#         Type = x$type,
#         Position = x$position,
#         N_Obs = x$n_obs,
#         R_Squared = round(x$r_squared, 3),
#         RMSE = round(x$rmse, 3)
#       )
#     }))
    
#     # Sort by Quality (Good first)
#     setorder(summary_df, rmse, -R_Squared)
    
#     cat("\n--- Model Performance Summary ---\n")
#     print(summary_df)
    
#     return(list(
#       summary = summary_df,
#       models = results
#     ))
#   } else {
#     warning("No models trained.")
#     return(NULL)
#   }
# }


build_ensemble_models <- function(data, pred = 'AER', var = c('mean_wind_speed', 'Temp_K', 'diff_WD'),
                           place = c("Odder", "Gjerlev", "Lindå", "Auning", "Hobro"),
                           type = c('Control', 'Acid'), position = c('Position 1', 'Position 2'),
                           min_samples = 10, test_fraction = 0.2) {

  # 1. Filter Data
  dt <- copy(data[Place %in% place & Type %in% type & Measurement_point %in% position])

  # 2. Define Combinations
  combos <- CJ(place = place, type = type, position = position, unique = TRUE)
  results <- list()

  # 3. Loop
  for (i in 1:nrow(combos)) {
    p <- combos$place[i]
    t <- combos$type[i]
    pos <- combos$position[i]

    dt_sub <- dt[Place == p & Type == t & Measurement_point == pos]

    # Check data sufficiency
    if (nrow(dt_sub) < min_samples) {
      message(sprintf("Skipping %s/%s/%s: Insufficient data (%d rows)", p, t, pos, nrow(dt_sub)))
      next
    }

    # Check if predictor columns exist
    if (!all(var %in% names(dt_sub))) {
      warning(sprintf("Missing predictors for %s/%s/%s", p, t, pos))
      next
    }

    # Split data
    set.seed(123)
    train_idx <- sample(1:nrow(dt_sub), size = round(nrow(dt_sub) * (1 - test_fraction)))
    dt_train <- dt_sub[train_idx]
    dt_test  <- dt_sub[-train_idx]

    # Train Random Forest
    model <- tryCatch({
      ranger(
        formula = as.formula(paste(pred, "~", paste(var, collapse = " + "))),
        data = dt_train[, c(var, pred), with = FALSE],
        num.trees = 100,
        min.node.size = 5,
        importance = "impurity"
      )
    }, error = function(e) {
      warning(sprintf("Failed to train RF for %s/%s/%s: %s", p, t, pos, e$message))
      return(NULL)
    })

    if (is.null(model)) next

    # Calculate Performance on test set
    preds <- predict(model, data = dt_test[, ..var])$predictions
    actuals <- dt_test[[pred]]

    ss_res <- sum((actuals - preds)^2)
    ss_tot <- sum((actuals - mean(actuals))^2)
    r_sq <- 1 - (ss_res / ss_tot)
    rmse <- sqrt(mean((actuals - preds)^2))

    # Store Results
    results[[paste(p, t, pos, sep = "_")]] <- list(
      place = p,
      type = t,
      position = pos,
      n_obs = nrow(dt_sub),
      n_train = nrow(dt_train),
      n_test = nrow(dt_test),
      model = model,
      r_squared = r_sq,
      rmse = rmse
    )
  }

  # 4. Create Summary
  if (length(results) > 0) {
    summary_df <- rbindlist(lapply(results, function(x) {
      data.table(
        Place = x$place,
        Type = x$type,
        Position = x$position,
        N_Obs = x$n_obs,
        N_Train = x$n_train,
        N_Test = x$n_test,
        R_Squared = round(x$r_squared, 3),
        RMSE = round(x$rmse, 3)
      )
    }))

    # Sort by R_Squared (Good first)
    setorder(summary_df, -R_Squared)

    cat("\n--- Model Performance Summary ---\n")
    print(summary_df)

    return(list(
      summary = summary_df,
      models = results
    ))
  } else {
    warning("No models trained.")
    return(NULL)
  }
}

build_single_model <- function(data, pred = 'AER', var = c('mean_wind_speed', 'Temp_K', 'diff_WD')) {

  # Check if predictor columns exist
  if (!all(var %in% names(data))) {
    stop("Missing one or more predictor columns in the data.")
  }

  # Check if target column exists
  if (!pred %in% names(data)) {
    stop("Target column not found in the data.")
  }

  # Train Random Forest
  model <- tryCatch({
    ranger(
      formula = as.formula(paste(pred, "~", paste(var, collapse = " + "))),
      data = data[, c(var, pred), with = FALSE],
      num.trees = 100,
      min.node.size = 5,
      importance = "impurity"
    )
  }, error = function(e) {
    stop(sprintf("Failed to train model: %s", e$message))
  })

  # Calculate Performance
  preds <- predict(model, data = data[, ..var])$predictions
  actuals <- data[[pred]]

  ss_res <- sum((actuals - preds)^2)
  ss_tot <- sum((actuals - mean(actuals))^2)
  r_sq <- 1 - (ss_res / ss_tot)
  rmse <- sqrt(mean((actuals - preds)^2))

  # Return model and metrics
  list(
    model = model,
    r_squared = r_sq,
    rmse = rmse
  )
}




library(car)

non_lin_reg <- function(
    data,
    pred = "AER",
    var = c("mean_wind_speed", "Temp_K", "diff_WD"),
    place = c("Odder", "Gjerlev", "Lindå", "Auning", "Hobro"),
    type = c("Control", "Acid"),
    position = c("Position 1", "Position 2"),
    min_samples = 10,
    degree = 2,
    interactions = TRUE
) {

  # Filter base data
  dt <- copy(
    data[
      Place %in% place &
      Type %in% type &
      Measurement_point %in% position
    ]
  )

  # Combinations
  combos <- CJ(
    place = place,
    type = type,
    position = position,
    unique = TRUE
  )

  # Count valid samples AFTER NA removal
  combos[, n := mapply(function(p, t, pos) {

    dt_sub <- dt[
      Place == p &
      Type == t &
      Measurement_point == pos
    ]

    dt_sub <- na.omit(dt_sub[, c(pred, var), with = FALSE])

    nrow(dt_sub)

  }, place, type, position)]

  combos_valid <- combos[n >= min_samples]

  cat("\nCombinations tested:\n")
  print(combos_valid, nrows = Inf)

  # Formula builder
  build_formula <- function(pred, vars, degree = 2, interactions = TRUE) {

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
        FUN = function(x) paste0(x[1], ":", x[2])
      )
      terms <- c(terms, inter_terms)
    }

    as.formula(paste(pred, "~", paste(terms, collapse = " + ")))
  }

  # Fit function
  fit_one <- function(p, t, pos) {

    dt_sub <- dt[
      Place == p &
      Type == t &
      Measurement_point == pos
    ]

    dt_sub <- na.omit(dt_sub[, c(pred, var), with = FALSE])

    if (nrow(dt_sub) < min_samples) return(NULL)

    # --------------------------- #
    # CENTERING STEP (IMPORTANT)
    # --------------------------- #
    means <- dt_sub[, lapply(.SD, mean), .SDcols = var]

    for (v in var) {
      dt_sub[, (v) := get(v) - means[[v]]]
    }

    # Build formula
    formula_obj <- build_formula(
      pred = pred,
      vars = var,
      degree = degree,
      interactions = interactions
    )

    # Fit model
    model <- lm(formula_obj, data = dt_sub)

    # VIFs
    vif_terms <- tryCatch(
      suppressWarnings(round(vif(model))),
      error = function(e) NA
    )

    # Return
    list(
      place = p,
      type = t,
      position = pos,
      n = nrow(dt_sub),
      means = means,              # IMPORTANT for deployment
      formula = formula_obj,
      model = model,
      summary = summary(model),
      vif_terms = vif_terms
    )
  }

  # Run all models
  results <- lapply(
    1:nrow(combos_valid),
    function(i) {
      fit_one(
        combos_valid$place[i],
        combos_valid$type[i],
        combos_valid$position[i]
      )
    }
  )

  # Remove NULLs
  results <- Filter(Negate(is.null), results)

  return(results)
}
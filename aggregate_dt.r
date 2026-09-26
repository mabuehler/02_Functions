
###########################################################################################
###########################################################################################
#####                                                                                 #####
#####    Function to aggregate large data.table with numeric and character columns    #####
#####                                                                                 #####
###########################################################################################
###########################################################################################

library(data.table)

aggregate_dt <- function(dt, by, cov_col = 'hours') {

  # identify columns as some are numeric, and WD and coverage need special treatment
  num_cols  <- names(dt)[sapply(dt, is.numeric)]
  num_cols  <- setdiff(num_cols, c("WD", cov_col, by))
  char_cols <- names(dt)[sapply(dt, is.character)]
  char_cols <- setdiff(char_cols, by)
  factor_cols <- names(dt)[sapply(dt, is.factor)]
  factor_cols <- setdiff(factor_cols, by)
  # make it also work for dates and times
  date_cols <- names(dt)[sapply(dt, function(x) inherits(x, "Date") || inherits(x, "POSIXt"))]
  date_cols <- setdiff(date_cols, by)
  
  # keep original column order
  original_order <- names(dt)
  
  # define SD columns (only include cov_col if present)
  sd_cols <- c(num_cols, char_cols, factor_cols, date_cols)
  if (cov_col %in% names(dt)) sd_cols <- c(sd_cols, cov_col)
  
  result <- dt[, {
    
    # numeric means
    num_means <- lapply(.SD[, num_cols, with = FALSE], mean, na.rm = TRUE)
    
    # date/POSIX means
    date_means <- lapply(.SD[, date_cols, with = FALSE], function(x) {
      m <- mean(as.numeric(x), na.rm = TRUE)
      if (inherits(x, "Date")) {
        as.Date(m, origin = "1970-01-01")
      } else {
        as.POSIXct(m, origin = "1970-01-01", tz = attr(x, "tzone"))
      }
    })

    # circular mean for WD (if present)
    wd_mean <- NULL
    if ("WD" %in% names(dt)) {
      rad <- WD * pi / 180
      wd_mean <- atan2(mean(sin(rad), na.rm = TRUE), mean(cos(rad), na.rm = TRUE)) * 180 / pi
      wd_mean <- (wd_mean + 360) %% 360
    }
    
    # character columns (take first value, as they should be the same)
    chars <- lapply(.SD[, char_cols, with = FALSE], function(z) z[1L])
    # factor columns (take the first value, as the rest should be the same)
    factors <- lapply(.SD[, factor_cols, with =FALSE], function(z) z[1L])

    # sum coverage (if present)
    coverage_sum <- NULL
    if (cov_col %in% names(dt)) {
      coverage_sum <- sum(.SD[[cov_col]], na.rm = TRUE)
    }
    
    # combine results
    c(num_means, date_means, if (!is.null(wd_mean)) list(WD = wd_mean), chars, factors, if (!is.null(coverage_sum)) setNames(list(coverage_sum), cov_col))

  }, by = by, .SDcols = sd_cols]
  
  # restore column order (by first, then original order)
  final_order <- c(by, setdiff(original_order, by))
  final_order <- intersect(final_order, names(result))
  setcolorder(result, final_order)
  
  return(result)
}



average_by <- function(dt, by) {
  
  # Capture the expression supplied to 'by'
  by_expr <- substitute(by)
  
  # Name of the grouping variable
  by_name <- deparse(by_expr)
  
  # Evaluate the grouping expression
  grouping <- eval(by_expr, envir = dt, enclos = parent.frame())
  
  # Check that grouping has one value per row
  if (length(grouping) != nrow(dt)) {
    stop("'by' must produce a vector with length nrow(dt)")
  }
  
  # Work on a copy so the original dt is untouched
  x <- copy(dt)
  
  # Unique temporary column name
  tmp_group <- "..average_by_group"
  
  # Add grouping variable
  x[, (tmp_group) := grouping]
  
  # Columns to aggregate
  agg_cols <- names(dt)
  
  # Aggregate
  result <- x[
    ,
    lapply(.SD, function(v) {
      
      if (inherits(v, "POSIXct")) {
        
        # Average POSIXct values and return POSIXct
        tz <- attr(v, "tzone")
        if (is.null(tz)) tz <- ""
        
        as.POSIXct(
          mean(as.numeric(v), na.rm = TRUE),
          origin = "1970-01-01",
          tz = tz
        )
        
      } else if (is.numeric(v)) {
        
        # Numeric and integer
        mean(v, na.rm = TRUE)
        
      } else if (is.character(v) || is.logical(v)) {
        
        # Assumes values are identical within each group
        idx <- which(!is.na(v))
        
        if (length(idx) > 0) {
          v[idx[1L]]
        } else {
          v[NA_integer_]
        }
        
      } else {
        
        # Fallback for other classes
        idx <- which(!is.na(v))
        
        if (length(idx) > 0) {
          v[idx[1L]]
        } else {
          v[NA_integer_]
        }
      }
      
    }),
    by = tmp_group,
    .SDcols = agg_cols
  ]
  
  # Give the grouping column its appropriate name
  setnames(result, tmp_group, by_name)
  
  result
}


###########################################################################################
###########################################################################################
#####                                                                                 #####
#####    Function to aggregate large data.table with numeric and character columns    #####
#####                                                                                 #####
###########################################################################################
###########################################################################################

library(data.table)

aggregate_dt <- function(dt, by) {

  # identify columns as some are numeric, and WD and coverage need special treatment
  num_cols  <- names(dt)[sapply(dt, is.numeric)]
  num_cols  <- setdiff(num_cols, c("WD", "coverage", by))
  char_cols <- names(dt)[sapply(dt, is.character)]
  char_cols <- setdiff(char_cols, by)
  
  # keep original column order
  original_order <- names(dt)
  
  result <- dt[, {
    # numeric means
    num_means <- lapply(.SD[, num_cols, with = FALSE], mean, na.rm = TRUE)
    
    # circular mean for WD (if present)
    wd_mean <- NULL
    if ("WD" %in% names(dt)) {
      rad <- WD * pi / 180
      wd_mean <- atan2(mean(sin(rad), na.rm = TRUE), mean(cos(rad), na.rm = TRUE)) * 180 / pi
      wd_mean <- (wd_mean + 360) %% 360
    }
    
    # character columns (take first value, as they should be the same)
    chars <- lapply(.SD[, char_cols, with = FALSE], function(z) z[1L])
    
    # sum coverage (if present)
    coverage_sum <- NULL
    if ("coverage" %in% names(dt)) {
      coverage_sum <- sum(coverage, na.rm = TRUE)
    }
    
    # combine results
    c(num_means, if (!is.null(wd_mean)) list(WD = wd_mean), chars, if (!is.null(coverage_sum)) list(coverage = coverage_sum))

  }, by = by, .SDcols = c(num_cols, char_cols)]
  
  # restore column order (by first, then original order)
  final_order <- c(by, setdiff(original_order, by))
  final_order <- intersect(final_order, names(result))
  setcolorder(result, final_order)
  
  return(result)
}

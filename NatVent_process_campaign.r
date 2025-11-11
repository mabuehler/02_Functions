
############################################################
############################################################
#####                                                  #####
#####    Functions used in concentration evaluation    #####
#####                                                  #####
############################################################
############################################################

library(data.table)
library(ggplot2)
library(lubridate)
library(circular)

##### 1. Load initially processd data:
get_campaign <- function(PathRSaves, farm) {
  # Get the list of file names in the directory
  file_paths <- list.files(file.path(PathRSaves, farm), pattern = paste0(farm, "_dt_P\\d+_full.rds"), full.names = TRUE)
  # Check if no files are found
  if (length(file_paths) == 0) {
    stop("No matching .rds files found in the directory.")
  }
  # Read the .rds files into a list
  ls_dt <- lapply(file_paths, readRDS)
  # Assign names to the list elements based on the pattern in the file names
  names(ls_dt) <- sub(paste0(farm, "_dt_(P\\d+)_full.rds"), "\\1", basename(file_paths))
  return(ls_dt)
}

##### 2. Get campaign period:
get_period <- function(dt) {
  c(dt[1, st], dt[.N, st])
}

##### 3. Dispaly lm for times where there was no NH3_dry:
display_linear_models <- function(dt_all_full, periods = paste0('P', 1:6)) {
  
  # Use lapply to iterate through each period
  models_summary <- lapply(periods, function(x) {
    # Extract the data.table for the current period
    dt <- dt_all_full[[x]]
    
    # Get the unique device for this period (assuming one device per period)
    device <- dt[!is.na(Device), unique(Device)]
    
    # Subset data to exclude rows with NH3_dry == 0 or NA
    train_data <- dt[!is.na(NH3_dry) & NH3_dry != 0, .(NH3, H2O, NH3_dry)]  # Only valid NH3_dry rows
    
    # Fit the linear model if there is sufficient data
    if (nrow(train_data) > 0) {
      lm_model <- lm(NH3_dry ~ NH3 + H2O, data = train_data)
      
      # Extract coefficients (Intercept, NH3, H2O)
      coef_vals <- coef(lm_model)
      
      # Return a data.table with Period, Device, Intercept, NH3, H2O coefficients
      return(data.table(Period = x,
                        Device = device,
                        Intercept = coef_vals[1], 
                        NH3 = coef_vals['NH3'], 
                        H2O = coef_vals['H2O']))
    } else {
      return(data.table(Period = x, Device = device, Intercept = NA, NH3 = NA, H2O = NA))  # Not enough data
    }
  })
  
  # Combine all the individual data.tables into one compact table
  return(rbindlist(models_summary))
}


##### 4. Predict NH3_dry for times it's missing:
predict_NH3_dry <- function(dt_all_full, missing_period = NULL, training_period = NULL) {
  # browser()
  # If the missing period is part of the training period, include it in the training data
  ls_training <- lapply(training_period, function(x) {
    dt_all_full[[x]]  # Extract the data.table for the current subset
  })
  # Combine the list of data.tables into one
  dt_training <- rbindlist(ls_training)
  
  # Train the linear model using rows where NH3_dry is not 0 and not NA
  train_data <- dt_training[NH3_dry != 0 & !is.na(NH3_dry), .(NH3, H2O, NH3_dry)]  # Subset of data with known NH3_dry values
  lm_model <- lm(NH3_dry ~ NH3 + H2O, data = train_data)  # Fit linear model
  
  # Extract the data for the missing period (whether it's part of the training data or not)
  dt_missing <- dt_all_full[[missing_period]]
  
  # Ensure we have NH3 and H2O columns in the prediction dataset
  missing_data <- dt_missing[NH3_dry == 0, .(NH3, H2O)]  # Use only rows where NH3_dry is 0
  
  # Predict missing NH3_dry values using the model and update the data.table
  dt_missing[NH3_dry == 0, NH3_dry := predict(lm_model, newdata = missing_data)]
  
  # Return the updated data.table for the missing period
  return(dt_missing)
}


##### 5. Apply calibration:
apply_calibration <- function(dt, device = NULL) {
  # Default to 'Device' column from dt if 'device' is not provided
  if (is.null(device)) {
    device <- dt[!is.na(Device), unique(Device)]  # Assuming 'Device' column exists and there is one unique device
  }

  # Ensure that the device is a valid entry
  if (length(device) != 1) {
    stop("There must be exactly one unique device in the 'Device' column if no device is specified.")
  }

  # Define the calibration constants in a list
  calibration_values <- list(
    '#03' = list(
      CH4_dry = list(offset = 0.0085, factor = 0.9893617),
      CO2_dry = list(offset = 0.1, factor = 0.9344964),
      NH3_dry = list(offset = 1.405, factor = 0.9281952),
      N2O_dry = list(offset = 0, factor = 1)
    ),
    '#02' = list(
      CH4_dry = list(offset = 0, factor = 1),  # Placeholder, no calibration for now
      CO2_dry = list(offset = 0, factor = 1),  # Placeholder, no calibration for now
      NH3_dry = list(offset = 0, factor = 1),  # Placeholder, no calibration for now
      N2O_dry = list(offset = 0, factor = 1)   # Placeholder, no calibration for now
    )
  )
  
  # Check if the device calibration exists
  if (device %in% names(calibration_values)) {
    # Apply calibration for CH4, CO2, NH3, and N2O based on the device
    dt[, CH4_dry_cal := (CH4_dry - calibration_values[[device]]$CH4_dry$offset) / calibration_values[[device]]$CH4_dry$factor]
    dt[, CO2_dry_cal := (CO2_dry - calibration_values[[device]]$CO2_dry$offset) / calibration_values[[device]]$CO2_dry$factor]
    dt[, NH3_dry_cal := (NH3_dry - calibration_values[[device]]$NH3_dry$offset) / calibration_values[[device]]$NH3_dry$factor]
    dt[, N2O_dry_cal := (N2O_dry - calibration_values[[device]]$N2O_dry$offset) / calibration_values[[device]]$N2O_dry$factor]
    
    return(dt)
  } else {
    message(paste('No calibration available for device:', device))
    return(dt)  # return uncalibrated data if no calibration exists
  }
}


##### 6. Define valve cycle and time per cycle:
assign_valve_cycles <- function(dt) {
  # Create MPVPosition_round
  dt[, MPVPosition_round := round(MPVPosition, 0)]
  # Generate the cycle identifiers
  dt[, N_cycle_all := rleid(MPVPosition_round)]
  # Calculate elapsed time in minutes based on actual timestamps
  dt[, Minutes := as.numeric(difftime(st, min(st), units = "mins")), by = N_cycle_all]
  return(dt)
}


##### 7.Plot concentration over valve cycles:
plot_barn_bgd_change <- function(dt, gas = 'NH3', bgd_pos, max_time = 15, sampling_pos = 3) {
    p1 <- dt[Minutes < max_time & MPVPosition_round == sampling_pos, {
      ggplot(.SD, aes(x = Minutes, y = get(paste0(gas, '_dry_cal')), group = N_cycle_all)) +
      geom_line() + ylab(gas) + theme_bw() + ggtitle(paste(gas, 'bgd to barn'))
    }]
    p2 <- dt[Minutes < max_time & MPVPosition_round %in% bgd_pos, {
      ggplot(.SD, aes(x = Minutes, y = get(paste0(gas, '_dry_cal')), group = N_cycle_all)) +
      geom_line() + ylab(gas) + theme_bw() + ggtitle(paste(gas, 'barn to bgd, pos = ', paste(bgd_pos, collapse = '/')))
    }]
    p <- ggpubr::ggarrange(p1, p2)
    print(p)
}


##### 8. Appy hard flags:
apply_flags <- function(dt, rm_start_min, st_mixing = NULL, mixing_duration = 30){
  # Flag logging rates longer than 5 seconds
  dt[logging_rate > 5, Flag_Logging := 1]

  ### MPVPositio change

  # Find indices where MPVPosition_round changes
  i_switch <- dt[dt[, which(diff(MPVPosition_round) != 0)], st]

  # Create a data.table of intervals
  intervals <- data.table(st = i_switch - seconds(10), et = i_switch + minutes(rm_start_min))

  # Set keys for fast overlap join
  setkey(dt, st)
  setkey(intervals, st, et)

  # Use foverlaps to flag rows where dt$st falls within any interval
  dt[, Flag_start := NA_integer_]
  overlaps <- foverlaps(dt[, .(st, et, Flag_start)], intervals, type = 'within', by.x = c("st", "et"), by.y = c("st", "et"), nomatch = 0L)
  dt[st %in% overlaps[, i.st], Flag_start := 1L]

  ### FLAG Mixing 
# browser()
  if (!is.null(st_mixing)){
  st_mixing <- with_tz(st_mixing, 'ETC/GMT-1')
  # Create a data.table of intervals
  mixing_int <- data.table(st = st_mixing, et = st_mixing + mixing_duration * 60)

  # Set keys for fast overlap join
  setkey(mixing_int, st, et)

  # Use foverlaps to flag rows where dt$st falls within any interval
  dt[, Flag_mixing := NA_integer_]
  overlaps_mixing <- foverlaps(dt[, .(st, et, Flag_mixing)], mixing_int, type = 'within', by.x = c("st", "et"), by.y = c("st", "et"), nomatch = 0L)
  dt[st %in% overlaps_mixing[, i.st], Flag_mixing := 1L]

  } else {
    dt[, Flag_mixing := NA_integer_]
  }

  if (!'Flag_operation' %in% names(dt)) dt[, Flag_operation := NA_integer_]

  dt[Flag_start == 1 | Flag_Logging == 1 | ALARM_STATUS != 0 |
        INST_STATUS == 7 | Flag_operation == 1, Flag_rm := 1]
  # if there is a bad INST_STATUS it is always also an alarm status excpet of INST_STATUS 7
  return(dt)
}


##### 9. Flag outlier drops in CH4 via IQR:
flag_drops <- function(dt, gas = 'CH4', mpv_positions) {
  dt[is.na(Flag_rm) & MPVPosition_round %in% mpv_positions, Flag_drop := {
    Q_subset <- quantile(get(paste0(gas, "_dry_cal")), 0.98, na.rm = TRUE)
    subset_vals <- get(paste0(gas, "_dry_cal"))[get(paste0(gas, "_dry_cal")) <= Q_subset]
    Q1 <- quantile(subset_vals, 0.25, na.rm = TRUE)
    Q3 <- quantile(subset_vals, 0.75, na.rm = TRUE)
    IQR_val <- Q3 - Q1
    lower_bound <- Q1 - 2 * IQR_val
    ifelse(get(paste0(gas, "_dry_cal")) < lower_bound, 1L, NA_integer_)
  }, by = N_cycle_all]
  return(dt)
}

##### 10. Unit conversion ppm & ppb to mg/m3:
convert_to_mgm3 <- function(dt) {
  R <- 8.31446261815324
  molar_mass <- c(CO2 = 44.009, CH4 = 16.043, NH3 = 17.031, N2O = 44.013, H2O = 18.015)
  for (gas in names(molar_mass)) {
    if (gas == 'H2O') {
      cal_col <- gas
      out_col <- paste0(gas, "_mgm3")
    } else {
      cal_col <- paste0(gas, "_dry_cal")
      out_col <- paste0(gas, "_dry_mgm3")
    }
    dt[MPVPosition == 3, (out_col) := (get(cal_col) * molar_mass[gas] * mean_pressure *
          ifelse(gas == "NH3", 0.0001, ifelse(gas == "H2O", 1000, 0.1))) / (R * (Temp + 273.15))]
    dt[MPVPosition != 3, (out_col) := (get(cal_col) * molar_mass[gas] * mean_pressure *
          ifelse(gas == "NH3", 0.0001, ifelse(gas == "H2O", 1000, 0.1))) / (R * (mean_temp + 273.15))]
  }
  return(dt)
}


##### 11. Do new cycles and calculate mean concentraiton per cycle:
cal_mean_cycle <- function(dt, gases = c("CH4", "CO2", "NH3", "N2O")) {
  # Assign a new cycle index
  dt[is.na(Flag_rm), N_cycle := rleid(MPVPosition_round)]
  # Compute mean time per cycle
  dt[is.na(Flag_rm), mean_st := mean(st), by = N_cycle]
  for (g in gases) {
    mgm3_col <- paste0(g, "_dry_mgm3")
    mean_col <- paste0("mean_", g, "_dry")
    dt[is.na(Flag_rm), (mean_col) := mean(get(mgm3_col), na.rm = TRUE), by = N_cycle]
  }
  # Calculate cycle duration in minutes
  dt[is.na(Flag_rm), min_perCycle := as.numeric(
    difftime(max(st), min(st), units = "secs")
  ) / 60, by = N_cycle]
  return(dt)
}


##### 12. plot the mean concentraion per cycle:
plot_mean <- function(dt, gas = 'CH4') {
  # Top plot: gas concentration vs time with custom legend title
  p1 <- ggplot(dt[is.na(Flag_rm)], aes(mean_st, get(paste0('mean_', gas, '_dry')), col = factor(MPVPosition_round))) + 
    geom_point() + 
    ylab(paste0(gas, ' mg/m3')) + 
    theme_bw() + 
    labs(color = "MPVPosition")
  
  # Calculate scale and offset for temp secondary axis
  wind_dir <- dt$mean_wind_dir
  temp <- dt$Temp
  scale_factor <- (max(wind_dir, na.rm=TRUE) - min(wind_dir, na.rm=TRUE)) / 
                  (max(temp, na.rm=TRUE) - min(temp, na.rm=TRUE))
  offset <- min(wind_dir, na.rm=TRUE) - min(temp, na.rm=TRUE) * scale_factor
  
  # Bottom plot: wind direction with secondary temp axis, no x labels, colored y labels
  p_WD <- ggplot(dt, aes(mean_st)) + 
    geom_point(aes(y = mean_wind_dir), color = "blue") +
    geom_point(aes(y = Temp * scale_factor + offset), color = "red") +
    scale_y_continuous(
      name = "Wind Dir (deg)",
      sec.axis = sec_axis(
        ~ (. - offset) / scale_factor, 
        name = "Temperature (°C)"
      )
    ) +
    theme_bw() +
    theme(
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.title.y.left = element_text(color = "blue"),
      axis.title.y.right = element_text(color = "red")
    )
  
  ggpubr::ggarrange(p1, p_WD, ncol = 1, align = "hv")
}



##### 13. Background interpolation:
interpolate_background <- function(dt, bgd_pos, gases = c('CH4', 'CO2', 'NH3', 'N2O'), max_gap_hours = 2) {
  max_gap_seconds <- max_gap_hours * 3600
  for (pos in bgd_pos) {
    for (g in gases) {
      mean_col <- paste0("mean_", g, "_dry")
      i_col <- paste0("i", pos, "_", mean_col)
      dt[is.na(Flag_rm) & MPVPosition_round == pos, (i_col) := get(mean_col)]
      dt <- dt[order(st)]
      # interpolate
      dt[, (i_col) := zoo::na.approx(get(i_col), x = as.numeric(st), na.rm = FALSE, maxgap = max_gap_seconds)]
      dt[, (i_col) := zoo::na.locf(get(i_col), na.rm = FALSE)]
      dt[, (i_col) := zoo::na.locf(get(i_col), na.rm = FALSE, fromLast = TRUE)]
    }
  }
  return(dt)
}


##### 14. Creat WD sector input:
create_WD_sector <- function(start, end, bgd_pos) {
  data.table(WD_start = start, WD_end = end, bgd_pos = list(bgd_pos))
}


##### 15.Subtract background concentration:
sub_background <- function(dt, WD_sectors, gases = c('CH4', 'CO2', 'NH3', 'N2O')) {
  # Initialize background columns to NA first
  for (g in gases) {
    bgd_col <- paste0(g, "_bgd")
    dt[, (bgd_col) := NA_real_]
  }

  # Assign background values according to wind sectors
  for (i in seq_len(nrow(WD_sectors))) {
    sector <- WD_sectors[i]
    for (g in gases) {
      mean_col <- paste0("mean_", g, "_dry")
      bgd_col <- paste0(g, "_bgd")
      # Compose background columns for this sector positions and gas
      bg_cols <- paste0("i", unlist(sector$bgd_pos), "_", mean_col)
      if (sector$WD_start < sector$WD_end) {
        cond <- dt$mean_wind_dir >= sector$WD_start & dt$mean_wind_dir < sector$WD_end
      } else {
        cond <- dt$mean_wind_dir >= sector$WD_start | dt$mean_wind_dir < sector$WD_end
      }
      # Assign mean background value to the new bgd_col for matching rows
      dt[cond & MPVPosition_round == 3, (bgd_col) := rowMeans(.SD, na.rm = TRUE), .SDcols = bg_cols]
    }
  }

  # Now subtract background from sampling gas columns to get corrected
  for (g in gases) {
    mean_col <- paste0("mean_", g, "_dry")
    bgd_col <- paste0(g, "_bgd")
    corr_col <- paste0(mean_col, "_corr")
    dt[MPVPosition_round == 3, (corr_col) := get(mean_col) - get(bgd_col)]
  }

  return(dt)
}


##### 16. Aggregate to hourly values and keep only relevant columns:
aggregate_hourly <- function(dt) {
  # browser()
  dt[, mean_st_floor := floor_date(mean_st, unit = "hour")]

  dt_hour <- dt[is.na(Flag_rm) & MPVPosition_round == 3,
    .(delta_CH4_mgm3 = mean(mean_CH4_dry_corr, na.rm = TRUE),
      delta_CO2_mgm3 = mean(mean_CO2_dry_corr, na.rm = TRUE),
      delta_NH3_mgm3 = mean(mean_NH3_dry_corr, na.rm = TRUE),
      delta_N2O_mgm3 = mean(mean_N2O_dry_corr, na.rm = TRUE),
      CH4_barn = mean(mean_CH4_dry, na.rm = TRUE),
      CO2_barn = mean(mean_CO2_dry, na.rm = TRUE),
      NH3_barn = mean(mean_NH3_dry, na.rm = TRUE),
      N2O_barn = mean(mean_N2O_dry, na.rm = TRUE),
      CH4_bgd = mean(CH4_bgd, na.rm = TRUE),
      CO2_bgd = mean(CO2_bgd, na.rm = TRUE),
      NH3_bgd = mean(NH3_bgd, na.rm = TRUE),
      N2O_bgd = mean(N2O_bgd, na.rm = TRUE),
      Temp = mean(Temp, na.rm = TRUE),
      RH = mean(RH, na.rm = TRUE),
      Press = mean(mean_pressure, na.rm = TRUE),
      Temp_out = mean(mean_temp, na.rm = TRUE),
      WS = mean(mean_wind_speed, na.rm = TRUE),
      WD = as.numeric(mean.circular(circular(mean_wind_dir, units = "degrees", template = "geographic", modulo = '2pi'), na.rm = TRUE))
    ),
    by = .(DateTime = mean_st_floor)]

  dt_DateTime <- data.table(DateTime = seq(from = dt[!is.na(st), ceiling_date(st[1], unit = 'day')], 
    to = floor_date(dt[!is.na(st), st[.N]], unit = 'day') + hours(7), by = '1 hour'))

  dt_hour_full <- merge(dt_hour, dt_DateTime, all = TRUE)
  dt_hour_full[, Day := as.IDate(DateTime)]

  dt[, mean_st_floor_new := floor_date(st, unit = 'hour')]
  dt_hour_meteo <- dt[MPVPosition_round == 3,
      .(Temp = mean(Temp, na.rm = TRUE),
        RH = mean(RH, na.rm = TRUE),
        Press = mean(mean_pressure, na.rm = TRUE),
        Temp_out = mean(mean_temp, na.rm = TRUE),
        WS = mean(mean_wind_speed, na.rm = TRUE),
        WD = as.numeric(mean.circular(circular(mean_wind_dir, units = "degrees", template = "geographic", modulo = '2pi'), na.rm = TRUE))
      ),
      by = .(DateTime = mean_st_floor_new)]

  dt_hour_final <- merge(dt_hour_full, dt_hour_meteo, by = 'DateTime', suffixes = c("", "_dt2"), all = TRUE)

  for (col in names(dt_hour_meteo)[names(dt_hour_meteo) != 'DateTime']) {
    dt_hour_final[is.na(get(col)), (col) := get(paste0(col, "_dt2"))]
  }

  # remove the added meteo data
  dt_hour_final[, grep("_dt2$", names(dt_hour_final), value = TRUE) := NULL]

  dt_hour_final[, c("Farm", "Period", "Device") := list(dt[!is.na(Farm), unique(Farm)], dt[!is.na(Period), unique(Period)], dt[!is.na(Device), unique(Device)])]
  return(dt_hour_final)
}


##### 17. Convert date/time function often used:
convert_date <- function(x, tz = NULL) {
  formats <- c("Y", "%d.%m.%Y", "%d.%m.%Y %H:%M", "%d.%m.%Y %H:%M:%S", "Ymd", "YmdHM", "YmdHMS")
  date_time <- parse_date_time(x, orders = formats, tz = tz)
  return(date_time)
}



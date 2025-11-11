
####################################################
####################################################
#####                                          #####
#####    Calculate CO2 production of cattle    #####
#####                                          #####
####################################################
####################################################


########################
### Define Functions ###
########################

ECM_to_milk <- function(ECM, fat, protein, percent = 0.95) { # converte ECM back to milk yield
  if(is.null(fat) && is.null(protein)) {
    milk <- ECM * percent
  } else {
    milk <- ECM / ((0.383 * fat + 0.242 * protein + 0.7832) / 3.14)
  }
  return(milk)
}

hpu_cow <- function(weight, milk, DinP) { # hpu of lactating and dry cows
	(5.6 * (weight^0.75) + (22 * milk) + ((1.6E-5) * (DinP^3))) * 44.009 / 22.4 * 24 / 1000
}

hpu_heifer <- function(weight, gain, energy_content, DinP) {
	((7.64 * (weight^0.67)) + (gain * (23 / energy_content - 1) * ((57.27 + 0.302 * weight) / (1 - 0.171 * gain))) + 1.6E-5 * DinP^3) * 44.009 / 22.4 * 24 / 1000
}

hpu_calf <- function(weight, gain) {
	(6.44 * weight^0.7 + ((13.3 * gain * (6.28 + 0.0188 * weight)) / (1 - 0.3 * gain))) * 44.009 / 22.4 * 24 / 1000
}

CO2_calc <- function(hpu, factor, N) { # CO2 production for all cattle according to CIGR and Pedersen
	if(is.nan(hpu)) {
		0
	} else {
		round(hpu * factor * N)
	}
}

CO2_kjeldsen <- function(hpu, factor, N) { # CO2 production and correct it with linear regression from Kjeldsen et al. 2024
	if(is.nan(hpu)) {
		0
	} else {
		round((((hpu * 0.180) * 0.76 + 4.721) + hpu * (factor - 0.180)) * N)
	}
}


calcCO2prod <- function(Farm, par_list, CHR, calves = FALSE) { # function to create the data.table with all the information
  rbindlist(lapply(names(par_list), function(period) {
    prm <- par_list[[period]]
    # browser()
	##### Cows:
    if (is.null(prm$cows_lact$milk)) {
      prm$cows_lact$milk <- ECM_to_milk(ECM = prm$cows_lact$ECM, fat = prm$cows_lact$fat, protein = prm$cows_lact$protein)
    }
    hpu_lact <- hpu_cow(weight = prm$cows_lact$weight, milk = prm$cows_lact$milk, DinP = prm$cows_lact$DinP)
    hpu_dry <- hpu_cow(weight = prm$cows_dry$weight, milk = 0, DinP = prm$cows_dry$DinP)
    # CIGR
    CO2_cow_lact <- CO2_calc(hpu = hpu_lact, factor = prm$cows_lact$factor, N = prm$cows_lact$N)
    CO2_cow_dry <- CO2_calc(hpu = hpu_dry, factor = prm$cows_dry$factor, N = prm$cows_dry$N)
    CO2_cows <- CO2_cow_lact + CO2_cow_dry
    # CIGR + Kjeldsen
  	CO2_cow_lact_kjeldsen <- CO2_kjeldsen(hpu = hpu_lact, factor = prm$cows_lact$factor, N = prm$cows_lact$N)
	  CO2_cow_dry_kjeldsen <- CO2_kjeldsen(hpu = hpu_dry, factor = prm$cows_dry$factor, N = prm$cows_dry$N)
    CO2_cow_kjeldsen <- CO2_cow_lact_kjeldsen + CO2_cow_dry_kjeldsen
    
    ##### Heifers:
    hpu_h0 <- hpu_heifer(weight = prm$heifers_0$weight, gain = prm$heifers_0$gain, energy = prm$heifers_0$energy, DinP = prm$heifers_0$DinP)
    hpu_h1 <- hpu_heifer(weight = prm$heifers_1$weight, gain = prm$heifers_1$gain, energy = prm$heifers_1$energy, DinP = prm$heifers_1$DinP)
    CO2_h0 <- CO2_calc(hpu = hpu_h0, factor = prm$heifers_0$factor, N = prm$heifers_0$N)
    CO2_h1 <- CO2_calc(hpu = hpu_h1, factor = prm$heifers_1$factor, N = prm$heifers_1$N)
    CO2_heif <- CO2_h0 + CO2_h1

    ##### Calves:
    if(calves == TRUE) {
      N_calves <- CHR[P == period & Sex == 'Kvie' & Age_days > prm$calves$min_age, .N] - prm$heifers_0$N - prm$heifers_1$N
      weight_calves <- round(mean(CHR[P == period & Sex == 'Kvie' &
                                          Age_days > prm$calves$min_age & Age_days <= prm$calves$max_age,
                                          Age_days * prm$calves$gain + prm$calves$weight_birth], na.rm = TRUE))
    } else {
      N_calves <- 0
      weight_calves <- 0
    }
    hpu_calves <- hpu_calf(weight = weight_calves, gain = prm$calves$gain)
    CO2_calves <- CO2_calc(hpu = hpu_calves, factor = prm$calves$factor, N = N_calves)

    # write values into original list
    par_list[[period]]$calves$N <- N_calves
    par_list[[period]]$calves$weight <- weight_calves

    ##### total CO2 production:
    CO2_total <- CO2_cows + CO2_heif + CO2_calves
    CO2_total_kjeldsen <- CO2_cow_kjeldsen + CO2_heif + CO2_calves

    # write table
    data.table(
      Farm = Farm,
      Period = period,
      CO2_production = CO2_total_kjeldsen,
      CO2_production_old = CO2_total,
      CO2_cows_lact = CO2_cow_lact_kjeldsen,
      CO2_cows_dry = CO2_cow_dry_kjeldsen,
      CO2_cows = CO2_cow_kjeldsen,
      CO2_kaelvekvier = CO2_h0,
      CO2_h1 = CO2_h1,
      CO2_heifers = CO2_heif,
      CO2_calves = CO2_calves,
      N_cows = prm$cows_lact$N + prm$cows_dry$N,
      N_heifers = prm$heifers_0$N + prm$heifers_1$N,
      N_calves = N_calves,
      N_animals = prm$cows_lact$N + prm$cows_dry$N + prm$heifers_0$N + prm$heifers_1$N + N_calves,
      Total_weight_cows = prm$cows_lact$weight * prm$cows_lact$N + prm$cows_dry$weight * prm$cows_dry$N,
      Total_weight_heifers = prm$heifers_0$weight * prm$heifers_0$N + prm$heifers_1$weight * prm$heifers_1$N,
      Total_weight_calves = weight_calves * N_calves,
      Total_weight_animals = prm$cows_lact$weight * prm$cows_lact$N + prm$cows_dry$weight * prm$cows_dry$N +
        prm$heifers_0$weight * prm$heifers_0$N + prm$heifers_1$weight * prm$heifers_1$N + weight_calves * N_calves,
      ECM = prm$cows_lact$ECM,
      Milk = prm$cows_lact$milk,
      Fat = prm$cows_lact$fat,
      Protein = prm$cows_lact$protein
    )
  }))
}


#################################
### Define General Parameters ###
#################################

## these are parameters I assume as given

DinP_lact <- 56 # average value
DinP_dry <- 250 # average value
DinP_heif1 <- 140 # average value
DinP_heif0 <- 0 # they don't give any milk
factor_RC <- 0.200 # Ring channel
factor_calves_RC <- 0.170 # Ring channel
factor_DSF <- 0.180 # Drained solid floor
factor_calves_DSF <- 0.155 # drained solid floor
gain_calves <- 0.65 # guessed
weight_birth <- 50 # guessed





#######################################
#######################################
#####                             #####
#####    Functions for Brøndum    #####
#####                             #####
#######################################
#######################################


extWeight <- function(dt, name = NULL) {
  out <- lapply(1:length(dt), function(x) {
    data.table(
      ECM = dt[[x]]$cows_lact$ECM, 
      BW_lac = dt[[x]]$cows_lact$weight,
      BW_dry = dt[[x]]$cows_dry$weight,
      BW_heif = dt[[x]]$heifers_0$weight,
      Name = name
    )
  })
  return(rbindlist(out))
}


calcWeight <- function(dt, ECM, animal) {
  if(animal == 'lac') {
    modBW_lac <- lm(BW_lac ~ ECM, dt)
    return(round(as.numeric(coef(modBW_lac)[1] + coef(modBW_lac)[2] * ECM)))
  }
  if(animal == 'dry') {
    modBW_lac <- lm(BW_lac ~ ECM, dt)
    modBW_dry <- lm(BW_dry ~ BW_lac, dt[BW_dry > 0])
    return(round(as.numeric(coef(modBW_dry)[1] + coef(modBW_dry)[2] * (coef(modBW_lac)[1] + coef(modBW_lac)[2] * ECM))))
  }
  if(animal == 'heif') {
    modBW_lac <- lm(BW_lac ~ ECM, dt)
    modBW_heif <- lm(BW_heif ~ BW_lac, dt[BW_heif > 0])
    return(round(as.numeric(coef(modBW_heif)[1] + coef(modBW_heif)[2] * (coef(modBW_lac)[1] + coef(modBW_lac)[2] * ECM))))
  }
}

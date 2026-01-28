
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

ECM_to_milk <- function(ECM, fat = NULL, protein = NULL, percent = 0.95) { # converte ECM back to milk yield
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
	if(is.na(hpu) | is.na(N)) {
		0
	} else {
		round(hpu * factor * N)
	}
}

CO2_kjeldsen_corr <- function(hpu, factor, N) { # CO2 production and correct it with linear regression from Kjeldsen et al. 2024
	if(is.na(hpu) | is.na(N)) {
		0
	} else {
		round((((hpu * 0.180) * 0.76 + 4.721) + hpu * (factor - 0.180)) * N)
	}
}


CO2_kjeldsen_mod1 <- function(dt, EFK = FALSE) {
  ### Holstein
  if (dt$breed == 'Holstein') {
    if (EFK) {
      # 1. parity
      CO2_gd_p1 <- dt$N_par1 * (956 + (122 * dt$DMI_la_EFK) + (60.4 * (dt$w_la)^0.75) + (3.44 * dt$DCP_la) - 777 + (206 * dt$DMI_la_EFK) + (0 * dt$DMI_la_EFK) + (-18.5 * (dt$w_la)^0.75))
      # 2. parity
      CO2_gd_p2 <- dt$N_par2 * (956 + (122 * dt$DMI_la_EFK) + (60.4 * (dt$w_la)^0.75) + (3.44 * dt$DCP_la) - 777 + (206 * dt$DMI_la_EFK) + (7.53 * dt$DMI_la_EFK) + (-18.5 * (dt$w_la)^0.75))
      # 3.+ parity
      CO2_gd_p3 <- dt$N_par3 * (956 + (122 * dt$DMI_la_EFK) + (60.4 * (dt$w_la)^0.75) + (3.44 * dt$DCP_la) - 777 + (206 * dt$DMI_la_EFK) + (15.7 * dt$DMI_la_EFK) + (-18.5 * (dt$w_la)^0.75))
    } else {
      # 1. parity
      CO2_gd_p1 <- dt$N_par1 * (956 + (122 * dt$DMI_la) + (60.4 * (dt$w_la)^0.75) + (3.44 * dt$DCP_la) - 777 + (206 * dt$DMI_la) + (0 * dt$DMI_la) + (-18.5 * (dt$w_la)^0.75))
      # 2. parity
      CO2_gd_p2 <- dt$N_par2 * (956 + (122 * dt$DMI_la) + (60.4 * (dt$w_la)^0.75) + (3.44 * dt$DCP_la) - 777 + (206 * dt$DMI_la) + (7.53 * dt$DMI_la) + (-18.5 * (dt$w_la)^0.75))
      # 3.+ parity
      CO2_gd_p3 <- dt$N_par3 * (956 + (122 * dt$DMI_la) + (60.4 * (dt$w_la)^0.75) + (3.44 * dt$DCP_la) - 777 + (206 * dt$DMI_la) + (15.7 * dt$DMI_la) + (-18.5 * (dt$w_la)^0.75))
    }
    CO2_kd <- (CO2_gd_p1 + CO2_gd_p2 + CO2_gd_p3) / 1000
  }

  ### Jersey
 if (dt$breed == 'Jersey') {
  if (EFK) {
  # 1. parity
  CO2_gd_p1 <- dt$N_par1 * (956 + (122 * dt$DMI_la_EFK) + (60.4 * (dt$w_la)^0.75) + (3.44 * dt$DCP_la) + 1103 + (204 * dt$DMI_la_EFK) + (0 * dt$DMI_la_EFK) + (-37.3 * (dt$w_la)^0.75))
  # 2. parity
  CO2_gd_p2 <- dt$N_par2 * (956 + (122 * dt$DMI_la_EFK) + (60.4 * (dt$w_la)^0.75) + (3.44 * dt$DCP_la) + 1103 + (204 * dt$DMI_la_EFK) + (7.53 * dt$DMI_la_EFK) + (-37.3 * (dt$w_la)^0.75))
  # 3.+ parity
  CO2_gd_p3 <- dt$N_par3 * (956 + (122 * dt$DMI_la_EFK) + (60.4 * (dt$w_la)^0.75) + (3.44 * dt$DCP_la) + 1103 + (204 * dt$DMI_la_EFK) + (15.7 * dt$DMI_la_EFK) + (-37.3 * (dt$w_la)^0.75))
  } else {
    # 1. parity
    CO2_gd_p1 <- dt$N_par1 * (956 + (122 * dt$DMI_la) + (60.4 * (dt$w_la)^0.75) + (3.44 * dt$DCP_la) + 1103 + (204 * dt$DMI_la) + (0 * dt$DMI_la) + (-37.3 * (dt$w_la)^0.75))
    # 2. parity
    CO2_gd_p2 <- dt$N_par2 * (956 + (122 * dt$DMI_la) + (60.4 * (dt$w_la)^0.75) + (3.44 * dt$DCP_la) + 1103 + (204 * dt$DMI_la) + (7.53 * dt$DMI_la) + (-37.3 * (dt$w_la)^0.75))
    # 3.+ parity
    CO2_gd_p3 <- dt$N_par3 * (956 + (122 * dt$DMI_la) + (60.4 * (dt$w_la)^0.75) + (3.44 * dt$DCP_la) + 1103 + (204 * dt$DMI_la) + (15.7 * dt$DMI_la) + (-37.3 * (dt$w_la)^0.75))
    }
  CO2_kd <- (CO2_gd_p1 + CO2_gd_p2 + CO2_gd_p3) / 1000
  }
  CO2_kd
} 

CO2_kjeldsen_mod2 <- function(dt, EFK = FALSE) {
  # browser()
  # CO2 <- b0 + (bECM * ECM) + (bBW075 * BW75) + (bMCF * MCF) + (bDIM * DIM) + breed + (bDIMDCF * DIM * DCF) + (bECMDIM * ECM * DIM) + (bECMBW * ECM * BW) + (bMCFBW * MCF * BW) + (bBWbreed * BW) + (bDIMbreed * DIM) + (bBWparity * BW)
  # CO2 <- -6134 + (213 * dt$ECM) + (126 * (dt$w_la)^0.75) + (52.5 * dt$MCF) + (-5.13 * dt$DIM_la) + breed + (-0.122 * dt$DIM_la * dt$DCF_la) + (0.386 * dt$ECM * dt$DIM_la) + (-1.18 * dt$ECM * (dt$w_la)^0.75) + (-0.614 * dt$MCF * (dt$w_la)^0.75) + (bBWbreed * (dt$w_la)^0.75) + (bDIMbreed * dt$DIM_la) + (bBWparity * (dt$w_la)^0.75)
  ### Holstein
  if (dt$breed == 'Holstein') {
    if (EFK) {
    # 1. parity
    CO2_gd_p1 <- dt$N_par1 * (-6134 + (213 * dt$ECM) + (126 * (dt$w_la)^0.75) + (52.5 * dt$MCF) + (-5.13 * dt$DIM_la) + 2117 + (-0.122 * dt$DIM_la * dt$DCF_la_EFK) + (0.386 * dt$ECM * dt$DIM_la) + (-1.18 * dt$ECM * (dt$w_la)^0.75) + (-0.614 * dt$MCF * (dt$w_la)^0.75) + (-5.96 * (dt$w_la)^0.75) + (2.06 * dt$DIM_la) + (0 * (dt$w_la)^0.75))
    # 2. parity
    CO2_gd_p2 <- dt$N_par2 * (-6134 + (213 * dt$ECM) + (126 * (dt$w_la)^0.75) + (52.5 * dt$MCF) + (-5.13 * dt$DIM_la) + 2117 + (-0.122 * dt$DIM_la * dt$DCF_la_EFK) + (0.386 * dt$ECM * dt$DIM_la) + (-1.18 * dt$ECM * (dt$w_la)^0.75) + (-0.614 * dt$MCF * (dt$w_la)^0.75) + (-5.96 * (dt$w_la)^0.75) + (2.06 * dt$DIM_la) + (3.66 * (dt$w_la)^0.75))
    # 3.+ parity
    CO2_gd_p3 <- dt$N_par3 * (-6134 + (213 * dt$ECM) + (126 * (dt$w_la)^0.75) + (52.5 * dt$MCF) + (-5.13 * dt$DIM_la) + 2117 + (-0.122 * dt$DIM_la * dt$DCF_la_EFK) + (0.386 * dt$ECM * dt$DIM_la) + (-1.18 * dt$ECM * (dt$w_la)^0.75) + (-0.614 * dt$MCF * (dt$w_la)^0.75) + (-5.96 * (dt$w_la)^0.75) + (2.06 * dt$DIM_la) + (4.01 * (dt$w_la)^0.75))
    } else {
      # 1. parity
      CO2_gd_p1 <- dt$N_par1 * (-6134 + (213 * dt$ECM) + (126 * (dt$w_la)^0.75) + (52.5 * dt$MCF) + (-5.13 * dt$DIM_la) + 2117 + (-0.122 * dt$DIM_la * dt$DCF_la) + (0.386 * dt$ECM * dt$DIM_la) + (-1.18 * dt$ECM * (dt$w_la)^0.75) + (-0.614 * dt$MCF * (dt$w_la)^0.75) + (-5.96 * (dt$w_la)^0.75) + (2.06 * dt$DIM_la) + (0 * (dt$w_la)^0.75))
      # 2. parity
      CO2_gd_p2 <- dt$N_par2 * (-6134 + (213 * dt$ECM) + (126 * (dt$w_la)^0.75) + (52.5 * dt$MCF) + (-5.13 * dt$DIM_la) + 2117 + (-0.122 * dt$DIM_la * dt$DCF_la) + (0.386 * dt$ECM * dt$DIM_la) + (-1.18 * dt$ECM * (dt$w_la)^0.75) + (-0.614 * dt$MCF * (dt$w_la)^0.75) + (-5.96 * (dt$w_la)^0.75) + (2.06 * dt$DIM_la) + (3.66 * (dt$w_la)^0.75))
      # 3.+ parity
      CO2_gd_p3 <- dt$N_par3 * (-6134 + (213 * dt$ECM) + (126 * (dt$w_la)^0.75) + (52.5 * dt$MCF) + (-5.13 * dt$DIM_la) + 2117 + (-0.122 * dt$DIM_la * dt$DCF_la) + (0.386 * dt$ECM * dt$DIM_la) + (-1.18 * dt$ECM * (dt$w_la)^0.75) + (-0.614 * dt$MCF * (dt$w_la)^0.75) + (-5.96 * (dt$w_la)^0.75) + (2.06 * dt$DIM_la) + (4.01 * (dt$w_la)^0.75))
      }
    CO2_kd <- (CO2_gd_p1 + CO2_gd_p2 + CO2_gd_p3) / 1000
  }

  ### Jersey
 if (dt$breed == 'Jersey') {
  if (EFK) {
  # 1. parity
  CO2_gd_p1 <- dt$N_par1 * (-6134 + (213 * dt$ECM) + (126 * (dt$w_la)^0.75) + (52.5 * dt$MCF) + (-5.13 * dt$DIM_la) + 1364 + (-0.122 * dt$DIM_la * dt$DCF_la_EFK) + (0.386 * dt$ECM * dt$DIM_la) + (-1.18 * dt$ECM * (dt$w_la)^0.75) + (-0.614 * dt$MCF * (dt$w_la)^0.75) + (-1.03 * (dt$w_la)^0.75) + (2.49 * dt$DIM_la) + (0 * (dt$w_la)^0.75))
  # 2. parity
  CO2_gd_p2 <- dt$N_par2 * (-6134 + (213 * dt$ECM) + (126 * (dt$w_la)^0.75) + (52.5 * dt$MCF) + (-5.13 * dt$DIM_la) + 1364 + (-0.122 * dt$DIM_la * dt$DCF_la_EFK) + (0.386 * dt$ECM * dt$DIM_la) + (-1.18 * dt$ECM * (dt$w_la)^0.75) + (-0.614 * dt$MCF * (dt$w_la)^0.75) + (-1.03 * (dt$w_la)^0.75) + (2.49 * dt$DIM_la) + (3.66 * (dt$w_la)^0.75))
  # 3.+ parity
  CO2_gd_p3 <- dt$N_par3 * (-6134 + (213 * dt$ECM) + (126 * (dt$w_la)^0.75) + (52.5 * dt$MCF) + (-5.13 * dt$DIM_la) + 1364 + (-0.122 * dt$DIM_la * dt$DCF_la_EFK) + (0.386 * dt$ECM * dt$DIM_la) + (-1.18 * dt$ECM * (dt$w_la)^0.75) + (-0.614 * dt$MCF * (dt$w_la)^0.75) + (-1.03 * (dt$w_la)^0.75) + (2.49 * dt$DIM_la) + (4.01 * (dt$w_la)^0.75))
  } else {
    # 1. parity
    CO2_gd_p1 <- dt$N_par1 * (-6134 + (213 * dt$ECM) + (126 * (dt$w_la)^0.75) + (52.5 * dt$MCF) + (-5.13 * dt$DIM_la) + 1364 + (-0.122 * dt$DIM_la * dt$DCF_la) + (0.386 * dt$ECM * dt$DIM_la) + (-1.18 * dt$ECM * (dt$w_la)^0.75) + (-0.614 * dt$MCF * (dt$w_la)^0.75) + (-1.03 * (dt$w_la)^0.75) + (2.49 * dt$DIM_la) + (0 * (dt$w_la)^0.75))
    # 2. parity
    CO2_gd_p2 <- dt$N_par2 * (-6134 + (213 * dt$ECM) + (126 * (dt$w_la)^0.75) + (52.5 * dt$MCF) + (-5.13 * dt$DIM_la) + 1364 + (-0.122 * dt$DIM_la * dt$DCF_la) + (0.386 * dt$ECM * dt$DIM_la) + (-1.18 * dt$ECM * (dt$w_la)^0.75) + (-0.614 * dt$MCF * (dt$w_la)^0.75) + (-1.03 * (dt$w_la)^0.75) + (2.49 * dt$DIM_la) + (3.66 * (dt$w_la)^0.75))
    # 3.+ parity
    CO2_gd_p3 <- dt$N_par3 * (-6134 + (213 * dt$ECM) + (126 * (dt$w_la)^0.75) + (52.5 * dt$MCF) + (-5.13 * dt$DIM_la) + 1364 + (-0.122 * dt$DIM_la * dt$DCF_la) + (0.386 * dt$ECM * dt$DIM_la) + (-1.18 * dt$ECM * (dt$w_la)^0.75) + (-0.614 * dt$MCF * (dt$w_la)^0.75) + (-1.03 * (dt$w_la)^0.75) + (2.49 * dt$DIM_la) + (4.01 * (dt$w_la)^0.75))
    } 
  CO2_kd <- (CO2_gd_p1 + CO2_gd_p2 + CO2_gd_p3) / 1000
  }
  CO2_kd
} 


CO2_kjeldsen_mod3 <- function(dt, EFK = FALSE) {
  # CO2 <- b0 + (bECM * ECM) + (bDIM * DIM_la) + breed + parity + (bbreedparity) + (bDIMDCF * DIM * DCF) + (bECMDIM * ECM * DIM) + (bDIMbreed * DIM) + (bMCFparity * MCF)
  # CO2 <- 8781 + (80.3 * dt$ECM) + (-4.66 * dt$DIM_la) + breed + parity + (bbreedparity) + (-0.149 * dt$DIM_la * dt$DCF_la) + (0.338 * dt$ECM * dt$DIM_la) + (bDIMbreed * dt$DIM_la) + (bMCFparity * dt$MCF)
  ### Holstein
  if (dt$breed == 'Holstein') {
    if (EFK) {
    # 1. parity
    CO2_gd_p1 <- dt$N_par1 * (8781 + (80.3 * dt$ECM) + (-4.66 * dt$DIM_la) - 49.0 + 0 + (0) + (-0.149 * dt$DIM_la * dt$DCF_la_EFK) + (0.338 * dt$ECM * dt$DIM_la) + (6.05 * dt$DIM_la) + (-4.18 * dt$MCF))
    # 2. parity
    CO2_gd_p2 <- dt$N_par2 * (8781 + (80.3 * dt$ECM) + (-4.66 * dt$DIM_la) - 49.0 + 511 + (775) + (-0.149 * dt$DIM_la * dt$DCF_la_EFK) + (0.338 * dt$ECM * dt$DIM_la) + (6.05 * dt$DIM_la) + (-10.5 * dt$MCF))
    # 3.+ parity
    CO2_gd_p3 <- dt$N_par3 * (8781 + (80.3 * dt$ECM) + (-4.66 * dt$DIM_la) - 49.0 + 1587 + (803) + (-0.149 * dt$DIM_la * dt$DCF_la_EFK) + (0.338 * dt$ECM * dt$DIM_la) + (6.05 * dt$DIM_la) + (-28.8 * dt$MCF))
    } else {
      # 1. parity
      CO2_gd_p1 <- dt$N_par1 * (8781 + (80.3 * dt$ECM) + (-4.66 * dt$DIM_la) - 49.0 + 0 + (0) + (-0.149 * dt$DIM_la * dt$DCF_la) + (0.338 * dt$ECM * dt$DIM_la) + (6.05 * dt$DIM_la) + (-4.18 * dt$MCF))
      # 2. parity
      CO2_gd_p2 <- dt$N_par2 * (8781 + (80.3 * dt$ECM) + (-4.66 * dt$DIM_la) - 49.0 + 511 + (775) + (-0.149 * dt$DIM_la * dt$DCF_la) + (0.338 * dt$ECM * dt$DIM_la) + (6.05 * dt$DIM_la) + (-10.5 * dt$MCF))
      # 3.+ parity
      CO2_gd_p3 <- dt$N_par3 * (8781 + (80.3 * dt$ECM) + (-4.66 * dt$DIM_la) - 49.0 + 1587 + (803) + (-0.149 * dt$DIM_la * dt$DCF_la) + (0.338 * dt$ECM * dt$DIM_la) + (6.05 * dt$DIM_la) + (-28.8 * dt$MCF))
      }       
    CO2_kd <- (CO2_gd_p1 + CO2_gd_p2 + CO2_gd_p3) / 1000
  }

  ### Jersey
 if (dt$breed == 'Jersey') {
  if (EFK) {
  # 1. parity
  CO2_gd_p1 <- dt$N_par1 * (8781 + (80.3 * dt$ECM) + (-4.66 * dt$DIM_la) - 2321 + 0 + (0) + (-0.149 * dt$DIM_la * dt$DCF_la_EFK) + (0.338 * dt$ECM * dt$DIM_la) + (6.02 * dt$DIM_la) + (-4.18 * dt$MCF))
  # 2. parity
  CO2_gd_p2 <- dt$N_par2 * (8781 + (80.3 * dt$ECM) + (-4.66 * dt$DIM_la) - 2321 + 511 + (608) + (-0.149 * dt$DIM_la * dt$DCF_la_EFK) + (0.338 * dt$ECM * dt$DIM_la) + (6.02 * dt$DIM_la) + (-10.5 * dt$MCF))
  # 3.+ parity
  CO2_gd_p3 <- dt$N_par3 * (8781 + (80.3 * dt$ECM) + (-4.66 * dt$DIM_la) - 2321 + 1587 + (1307) + (-0.149 * dt$DIM_la * dt$DCF_la_EFK) + (0.338 * dt$ECM * dt$DIM_la) + (6.02 * dt$DIM_la) + (-28.8 * dt$MCF))
  } else {
    # 1. parity
    CO2_gd_p1 <- dt$N_par1 * (8781 + (80.3 * dt$ECM) + (-4.66 * dt$DIM_la) - 2321 + 0 + (0) + (-0.149 * dt$DIM_la * dt$DCF_la) + (0.338 * dt$ECM * dt$DIM_la) + (6.02 * dt$DIM_la) + (-4.18 * dt$MCF))
    # 2. parity
    CO2_gd_p2 <- dt$N_par2 * (8781 + (80.3 * dt$ECM) + (-4.66 * dt$DIM_la) - 2321 + 511 + (608) + (-0.149 * dt$DIM_la * dt$DCF_la) + (0.338 * dt$ECM * dt$DIM_la) + (6.02 * dt$DIM_la) + (-10.5 * dt$MCF))
    # 3.+ parity
    CO2_gd_p3 <- dt$N_par3 * (8781 + (80.3 * dt$ECM) + (-4.66 * dt$DIM_la) - 2321 + 1587 + (1307) + (-0.149 * dt$DIM_la * dt$DCF_la) + (0.338 * dt$ECM * dt$DIM_la) + (6.02 * dt$DIM_la) + (-28.8 * dt$MCF))
    } 
  CO2_kd <- (CO2_gd_p1 + CO2_gd_p2 + CO2_gd_p3) / 1000
  }
  CO2_kd
} 



calcCO2prod_new <- function(dt, Farm = NULL, ECM = TRUE) { # function to create the data.table with all the information
  if (is.null(Farm)) {
    Farm <- paste('Farm', 1:8, sep = '_')
  }

  rbindlist(lapply(Farm, function(farm) {
    Period <- dt[Farm == farm, unique(Period)] 
    rbindlist(lapply(Period, function(period) {
      dt_sub <- dt[Farm == farm & Period == period, ]


## Days in milk
dt_sub[, DIM_la := 153] # a lactating cows gives 305 days of milk. Thus, on average it is in day 153 as a lactating cow is defined as the time it gives milk.
dt_sub[, DIM_dr := 0] # doesn't give milk thus 0
dt_sub[, DIM_kk := 0] # doesn't give milk
dt_sub[, DIM_he := 0] # doesn't give milk

## Days into pregnancy
dt_sub[, DIP_la := 83] # In Demnark a cow is 284 days pregnant and gets impregnated after 79 days. For simplification and that the numbers add up
# assume that the cow is pregnant for 285 days and impregnated after 80 days. So, on average they are 83 days pregnant (there are many that are 0 days pregnant so you can't just calcualte 153-80).
dt_sub[, DIP_dr := 255] # At the end of the dry period a cow is 285 days pregnant. The dry period is 60 days. Thus on average a dry cow is 255 days pregnant.
dt_sub[, DIP_kk := 255] # same as for dry cows

      ###############
      ### CIGR model:
# browser()
      ##### heat production:
      # lactating and dry cows
      if (ECM) { #### with ECM as an input
        hpu_la <- hpu_cow(weight = dt_sub$w_la, milk = dt_sub$ECM, DinP = dt_sub$DIP_la)
        hpu_dr <- hpu_cow(weight = dt_sub$w_dr, milk = dt_sub$ECM, DinP = dt_sub$DIP_dr)
        # hpu_kk <- hpu_cow(weight = dt_sub$w_kk, milk = dt_sub$ECM, DinP = dt_sub$DIP_kk) # kaelvekvier
      } else { #### with milk as an input
        hpu_la <- hpu_cow(weight = dt_sub$w_la, milk = dt_sub$milk, DinP = dt_sub$DIP_la)
        hpu_dr <- hpu_cow(weight = dt_sub$w_dr, milk = dt_sub$milk, DinP = dt_sub$DIP_dr)
        # hpu_kk <- hpu_cow(weight = dt_sub$w_kk, milk = dt_sub$milk, DinP = dt_sub$DIP_kk) # kaelvekvier
      }
      ## kaelvekvier and heifers
      hpu_kk <- hpu_heifer(weight = dt_sub$w_kk, gain = dt_sub$g_kk, energy = dt_sub$energy_kk, DinP = dt_sub$DIP_kk)
      hpu_he <- hpu_heifer(weight = dt_sub$w_he, gain = dt_sub$g_he, energy = dt_sub$energy_he, DinP = dt_sub$DIP_he)
      # calves
      hpu_ca <- hpu_calf(weight = dt_sub$w_ca, gain = dt_sub$g_ca)

    ##### CO2 production:
      # lactating and dry cows
      CO2_CIGR_la <- CO2_calc(hpu = hpu_la, factor = dt_sub$fac_ch, N = dt_sub$N_la)
      CO2_CIGR_dr <- CO2_calc(hpu = hpu_dr, factor = dt_sub$fac_ch, N = dt_sub$N_dr)
      # kaelvekvier and heifers
      CO2_CIGR_kk <- CO2_calc(hpu = hpu_kk, factor = dt_sub$fac_ch, N = dt_sub$N_kk)
      CO2_CIGR_he <- CO2_calc(hpu = hpu_he, factor = dt_sub$fac_ch, N = dt_sub$N_he)
      # calves
      CO2_CIGR_ca <- CO2_calc(hpu = hpu_ca, factor = dt_sub$fac_ca, N = dt_sub$N_ca)

    ##### CO2 production with a correction of Kjeldsen according to Figure :
      CO2_CIGR_Kjel_la <- CO2_kjeldsen_corr(hpu = hpu_la, factor = dt_sub$fac_ch, N = dt_sub$N_la)
      CO2_CIGR_Kjel_dr <- CO2_kjeldsen_corr(hpu = hpu_dr, factor = dt_sub$fac_ch, N = dt_sub$N_dr)


    ############################
    ### Kjeldsen et al. (2024):

    ##### Model 1-3:
      CO2_Kjel_mod1_la <- CO2_kjeldsen_mod1(dt_sub)
      CO2_Kjel_mod2_la <- CO2_kjeldsen_mod2(dt_sub)
      CO2_Kjel_mod3_la <- CO2_kjeldsen_mod3(dt_sub)

# browser()

    ##### Model 1-3 with EFK values
      CO2_Kjel_mod1_la_EFK <- CO2_kjeldsen_mod1(dt_sub, EFK = TRUE)
      CO2_Kjel_mod2_la_EFK <- CO2_kjeldsen_mod2(dt_sub, EFK = TRUE)
      CO2_Kjel_mod3_la_EFK <- CO2_kjeldsen_mod3(dt_sub, EFK = TRUE)


      # write table
      data.table(
        Farm = Farm,
        Period = period,
        CO2_CIGR_la = CO2_CIGR_la,
        CO2_CIGR_dr = CO2_CIGR_dr,
        CO2_CIGR_kk = CO2_CIGR_kk,
        CO2_CIGR_he = CO2_CIGR_he,
        CO2_CIGR_ca = CO2_CIGR_ca,
        CO2_CIGR_Kjel_la = CO2_CIGR_Kjel_la,
        CO2_CIGR_Kjel_dr = CO2_CIGR_Kjel_dr,
        CO2_Kjel_mod1_la = CO2_Kjel_mod1_la,
        CO2_Kjel_mod2_la = CO2_Kjel_mod2_la,
        CO2_Kjel_mod3_la = CO2_Kjel_mod3_la,
        CO2_Kjel_mod1_la_EFK = CO2_Kjel_mod1_la_EFK,
        CO2_Kjel_mod2_la_EFK = CO2_Kjel_mod2_la_EFK,
        CO2_Kjel_mod3_la_EFK = CO2_Kjel_mod3_la_EFK,
        N_la = dt_sub$N_la,
        N_dr = dt_sub$N_dr,
        N_kk = dt_sub$N_kk,
        N_he = dt_sub$N_he,
        N_ca = dt_sub$N_ca,
        N_all = dt_sub$N_la + dt_sub$N_dr + dt_sub$N_kk + dt_sub$N_he + dt_sub$N_ca,
        w_la = dt_sub$w_la,
        w_dr = dt_sub$w_dr,
        w_kk = dt_sub$w_kk,
        w_he = dt_sub$w_he,
        w_ca = dt_sub$w_ca,
        w_all = dt_sub$w_la + dt_sub$w_dr + dt_sub$w_kk + dt_sub$w_he + dt_sub$w_ca
      )


}))
}))


}



    

#################################
### Define General Parameters ###
#################################

## these are parameters I assume as given
# Days into pregnancy
DinP_lact <- 56 # average value 50-120 or  60-90
DinP_dry <- 250 # average value 20-35 or 45-60
DinP_heif1 <- 140 # average value probably like dry cows
DinP_heif0 <- 0 # they are not pregnant yet
# Days in milk
DinM_lact <- 56 # average value 150-170 or 120-150
DinM_dry <- 250 # average value 365
DinM_heif1 <- 140 # average value
DinM_heif0 <- 0 # they don't give any milk

factor_RC <- 0.200 # Ring channel
factor_calves_RC <- 0.170 # Ring channel
factor_DSF <- 0.180 # Drained solid floor
factor_calves_DSF <- 0.155 # drained solid floor
gain_calves <- 0.65 # guessed
weight_birth <- 50 # guessed


##### Kjeldsen et al., 2024 model input table:

dt_Kjeldsen <- data.table(
  Orig = c(
    "Intercept", "DMI (kg/d)", "ECM (kg/d)", "MetaBW (kg)",
    "Diet CP (g/kg DM)", "Milk CF (g/kg)", "DIM (d)", "Ayrshire",
    "Holstein", "Jersey", "Others/crossbreeds", "First", "Second",
    "Third and higher", "DIM × Diet CF", "ECM × DIM",
    "ECM × metaBW", "Milk CF × metaBW", "DMI × Ayrshire",
    "DMI × Holstein", "DMI × Jersey", "DMI × others/crossbreds",
    "DMI × first parity", "DMI × second parity", "DMI × third parity",
    "MetaBW × Ayrshire", "MetaBW × Holstein", "MetaBW × Jersey",
    "MetaBW × others/crossbreeds", "DIM × Ayrshire", "DIM × Holstein",
    "DIM × Jersey", "DIM × others/crossbreeds", "MetaBW × first parity",
    "MetaBW × second parity", "MetaBW × third parity",
    "First parity × milk CF", "Second parity × milk CF",
    "Third parity × milk CF", "Ayrshire × first parity",
    "Ayrshire × second parity", "Ayrshire × third parity",
    "Holstein × first parity", "Holstein × second parity",
    "Holstein × third parity", "Jersey × first parity",
    "Jersey × second parity", "Jersey × third parity",
    "Others/crossbreeds × first parity",
    "Others/crossbreeds × second parity",
    "Others/crossbreeds × third parity"
  ),
  Item = c(
    "Intercept", "DMI", "ECM", "metaBW", "DCP", "MCF", "DIM",
    "Ayrshire", "Holstein", "Jersey", "Others", "first", "second",
    "third", "DIM_DCF", "ECM_DIM", "ECM_metaBW", "MCF_metaBW",
    "DMI_Ayrshire", "DMI_Holstein", "DMI_Jersey", "DMI_others",
    "DMI_first", "DMI_second", "DMI_third", "metaBW_Ayrshire",
    "metaBW_Holstein", "metaBW_Jersey", "metaBW_others",
    "DIM_Ayrshire", "DIM_Holstein", "DIM_Jersey", "DIM_others",
    "metaBW_first", "metaBW_second", "metaBW_third",
    "first_MCF", "second_MCF", "third_MCF", "Ayrshire_first",
    "Ayrshire_second", "Ayrshire_third", "Holstein_first",
    "Holstein_second", "Holstein_third", "Jersey_first",
    "Jersey_second", "Jersey_third", "others_first",
    "others_second", "others_third"
  ),
  Model_1 = c(
    956, 122, NA, 60.4, 3.44, NA, NA, 0, -777, 1103, 1501, rep(NA, 7), 
    0, 206, 204, 225, 0, 7.53, 15.7, 0, -18.5, -37.3, -43.2, rep(NA, 22)
  ),
  Model_2 = c(
    -6134, NA, 213, 126, NA, 52.50, -5.13, 0, 2117, 1364, 4083, rep(NA, 3), -0.12, 0.39, 
    -1.18, -0.61, rep(NA, 7), 0, -5.96, -1.03, -33.4, 0, 2.06, 2.49, 8.94, 0, 3.66, 4.01, rep(NA, 15)
  ),
  Model_3 = c(
    8781, NA, 80.3, rep(NA, 3), -4.66, 0, -49, -2321, -1237, 0, 511, 1587, -0.15, 0.34, rep(NA, 13), 0, 
    6.05, 6.02, 11.3, rep(NA, 3), -4.18, -10.5, -28.8, rep(0, 4), 775, 803, 0, 608, 1307, 0, 791, 659
  )
)



calcCO2prod <- function(Farm, par_list, CHR, ECM = TRUE, calves = FALSE) { # function to create the data.table with all the information
  rbindlist(lapply(names(par_list), function(period) {
    prm <- par_list[[period]]
    # browser()
  ##### Cows:
    if (ECM) {
      prm$cows$milk <- prm$cows$ECM
    }
    if (is.null(prm$cows_lact$milk)) {
      prm$cows_lact$milk <- ECM_to_milk(ECM = prm$cows_lact$ECM, fat = prm$cows_lact$fat, protein = prm$cows_lact$protein)
    }
    hpu_lact <- hpu_cow(weight = prm$cows_lact$weight, milk = prm$cows_lact$milk, DinP = prm$cows_lact$DinP)
    hpu_dry <- hpu_cow(weight = prm$cows_dry$weight, milk = 0, DinP = prm$cows_dry$DinP)
    # CIGR
    CO2_cow_lact <- CO2_calc(hpu = hpu_lact, factor = prm$cows_lact$factor, N = prm$cows_lact$N)
    CO2_cow_dry <- CO2_calc(hpu = hpu_dry, factor = prm$cows_dry$factor, N = prm$cows_dry$N)
    CO2_cows <- CO2_cow_lact + CO2_cow_dry
    # CIGR + Kjeldsen correction
    CO2_cow_lact_kjeldsen_corr <- CO2_kjeldsen_corr(hpu = hpu_lact, factor = prm$cows_lact$factor, N = prm$cows_lact$N)
    CO2_cow_dry_kjeldsen_corr <- CO2_kjeldsen_corr(hpu = hpu_dry, factor = prm$cows_dry$factor, N = prm$cows_dry$N)
    CO2_cow_kjeldsen_corr <- CO2_cow_lact_kjeldsen_corr + CO2_cow_dry_kjeldsen_corr
    ## Kjeldsen et al. 2024
    # Model 1
    CO2_cow_lact_kjeldsen_mod1 <- CO2_kjeldsen_mod1(prm)
    # Model 2
    CO2_cow_lact_kjeldsen_mod2 <- CO2_kjeldsen_mod2(prm)
    # Model 3
    CO2_cow_lact_kjeldsen_mod3 <- CO2_kjeldsen_mod3(prm)

    ##### Heifers:
    hpu_h0 <- hpu_heifer(weight = prm$heifers_0$weight, gain = prm$heifers_0$gain, energy = prm$heifers_0$energy, DinP = prm$heifers_0$DinP)
    hpu_h1 <- hpu_heifer(weight = prm$heifers_1$weight, gain = prm$heifers_1$gain, energy = prm$heifers_1$energy, DinP = prm$heifers_1$DinP)
    CO2_h0 <- CO2_calc(hpu = hpu_h0, factor = prm$heifers_0$factor, N = prm$heifers_0$N)
    CO2_h1 <- CO2_calc(hpu = hpu_h1, factor = prm$heifers_1$factor, N = prm$heifers_1$N)
    CO2_heif <- CO2_h0 + CO2_h1

    ##### Calves:
    if(prm$calves$max_age > 0) {
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
    CO2_total_kjeldsen <- CO2_cow_kjeldsen_corr + CO2_heif + CO2_calves

    # write table
    data.table(
      Farm = Farm,
      Period = period,
      CO2_production = CO2_total_kjeldsen,
      CO2_production_old = CO2_total,
      CO2_cows_lact = CO2_cow_lact_kjeldsen_corr,
      CO2_cows_dry = CO2_cow_dry_kjeldsen_corr,
      CO2_cows = CO2_cow_kjeldsen_corr,
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

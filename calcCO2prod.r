
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
  milk <- round(milk, 1)
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
      CO2_gd_p1 <- dt$N_par1 * (956 + (122 * dt$DM_la_EFK) + (60.4 * (dt$w_la)^0.75) + (3.44 * dt$DCP_tot_la) - 777 + (206 * dt$DM_la_EFK) + (0 * dt$DM_la_EFK) + (-18.5 * (dt$w_la)^0.75))
      # 2. parity
      CO2_gd_p2 <- dt$N_par2 * (956 + (122 * dt$DM_la_EFK) + (60.4 * (dt$w_la)^0.75) + (3.44 * dt$DCP_tot_la) - 777 + (206 * dt$DM_la_EFK) + (7.53 * dt$DM_la_EFK) + (-18.5 * (dt$w_la)^0.75))
      # 3.+ parity
      CO2_gd_p3 <- dt$N_par3 * (956 + (122 * dt$DM_la_EFK) + (60.4 * (dt$w_la)^0.75) + (3.44 * dt$DCP_tot_la) - 777 + (206 * dt$DM_la_EFK) + (15.7 * dt$DM_la_EFK) + (-18.5 * (dt$w_la)^0.75))
    } else {
      # 1. parity
      CO2_gd_p1 <- dt$N_par1 * (956 + (122 * dt$DM_tot_la) + (60.4 * (dt$w_la)^0.75) + (3.44 * dt$DCP_tot_la) - 777 + (206 * dt$DM_tot_la) + (0 * dt$DM_tot_la) + (-18.5 * (dt$w_la)^0.75))
      # 2. parity
      CO2_gd_p2 <- dt$N_par2 * (956 + (122 * dt$DM_tot_la) + (60.4 * (dt$w_la)^0.75) + (3.44 * dt$DCP_tot_la) - 777 + (206 * dt$DM_tot_la) + (7.53 * dt$DM_tot_la) + (-18.5 * (dt$w_la)^0.75))
      # 3.+ parity
      CO2_gd_p3 <- dt$N_par3 * (956 + (122 * dt$DM_tot_la) + (60.4 * (dt$w_la)^0.75) + (3.44 * dt$DCP_tot_la) - 777 + (206 * dt$DM_tot_la) + (15.7 * dt$DM_tot_la) + (-18.5 * (dt$w_la)^0.75))
    }
    CO2_kd <- (CO2_gd_p1 + CO2_gd_p2 + CO2_gd_p3) / 1000
  }

  ### Jersey
 if (dt$breed == 'Jersey') {
  if (EFK) {
  # 1. parity
  CO2_gd_p1 <- dt$N_par1 * (956 + (122 * dt$DM_la_EFK) + (60.4 * (dt$w_la)^0.75) + (3.44 * dt$DCP_tot_la) + 1103 + (204 * dt$DM_la_EFK) + (0 * dt$DM_la_EFK) + (-37.3 * (dt$w_la)^0.75))
  # 2. parity
  CO2_gd_p2 <- dt$N_par2 * (956 + (122 * dt$DM_la_EFK) + (60.4 * (dt$w_la)^0.75) + (3.44 * dt$DCP_tot_la) + 1103 + (204 * dt$DM_la_EFK) + (7.53 * dt$DM_la_EFK) + (-37.3 * (dt$w_la)^0.75))
  # 3.+ parity
  CO2_gd_p3 <- dt$N_par3 * (956 + (122 * dt$DM_la_EFK) + (60.4 * (dt$w_la)^0.75) + (3.44 * dt$DCP_tot_la) + 1103 + (204 * dt$DM_la_EFK) + (15.7 * dt$DM_la_EFK) + (-37.3 * (dt$w_la)^0.75))
  } else {
    # 1. parity
    CO2_gd_p1 <- dt$N_par1 * (956 + (122 * dt$DM_tot_la) + (60.4 * (dt$w_la)^0.75) + (3.44 * dt$DCP_tot_la) + 1103 + (204 * dt$DM_tot_la) + (0 * dt$DM_tot_la) + (-37.3 * (dt$w_la)^0.75))
    # 2. parity
    CO2_gd_p2 <- dt$N_par2 * (956 + (122 * dt$DM_tot_la) + (60.4 * (dt$w_la)^0.75) + (3.44 * dt$DCP_tot_la) + 1103 + (204 * dt$DM_tot_la) + (7.53 * dt$DM_tot_la) + (-37.3 * (dt$w_la)^0.75))
    # 3.+ parity
    CO2_gd_p3 <- dt$N_par3 * (956 + (122 * dt$DM_tot_la) + (60.4 * (dt$w_la)^0.75) + (3.44 * dt$DCP_tot_la) + 1103 + (204 * dt$DM_tot_la) + (15.7 * dt$DM_tot_la) + (-37.3 * (dt$w_la)^0.75))
    }
  CO2_kd <- (CO2_gd_p1 + CO2_gd_p2 + CO2_gd_p3) / 1000
  }
  CO2_kd
} 

CO2_kjeldsen_mod2 <- function(dt, EFK = FALSE) {
  # browser()
  # CO2 <- b0 + (bECM * ECM) + (bBW075 * BW75) + (bMCF * MCF) + (bDIM * DIM) + breed + (bDIMDCF * DIM * DCF) + (bECMDIM * ECM * DIM) + (bECMBW * ECM * BW) + (bMCFBW * MCF * BW) + (bBWbreed * BW) + (bDIMbreed * DIM) + (bBWparity * BW)
  # CO2 <- -6134 + (213 * dt$ECM) + (126 * (dt$w_la)^0.75) + (52.5 * dt$MCF) + (-5.13 * dt$DIM_la) + breed + (-0.122 * dt$DIM_la * dt$DCF_tot_la) + (0.386 * dt$ECM * dt$DIM_la) + (-1.18 * dt$ECM * (dt$w_la)^0.75) + (-0.614 * dt$MCF * (dt$w_la)^0.75) + (bBWbreed * (dt$w_la)^0.75) + (bDIMbreed * dt$DIM_la) + (bBWparity * (dt$w_la)^0.75)
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
      CO2_gd_p1 <- dt$N_par1 * (-6134 + (213 * dt$ECM) + (126 * (dt$w_la)^0.75) + (52.5 * dt$MCF) + (-5.13 * dt$DIM_la) + 2117 + (-0.122 * dt$DIM_la * dt$DCF_tot_la) + (0.386 * dt$ECM * dt$DIM_la) + (-1.18 * dt$ECM * (dt$w_la)^0.75) + (-0.614 * dt$MCF * (dt$w_la)^0.75) + (-5.96 * (dt$w_la)^0.75) + (2.06 * dt$DIM_la) + (0 * (dt$w_la)^0.75))
      # 2. parity
      CO2_gd_p2 <- dt$N_par2 * (-6134 + (213 * dt$ECM) + (126 * (dt$w_la)^0.75) + (52.5 * dt$MCF) + (-5.13 * dt$DIM_la) + 2117 + (-0.122 * dt$DIM_la * dt$DCF_tot_la) + (0.386 * dt$ECM * dt$DIM_la) + (-1.18 * dt$ECM * (dt$w_la)^0.75) + (-0.614 * dt$MCF * (dt$w_la)^0.75) + (-5.96 * (dt$w_la)^0.75) + (2.06 * dt$DIM_la) + (3.66 * (dt$w_la)^0.75))
      # 3.+ parity
      CO2_gd_p3 <- dt$N_par3 * (-6134 + (213 * dt$ECM) + (126 * (dt$w_la)^0.75) + (52.5 * dt$MCF) + (-5.13 * dt$DIM_la) + 2117 + (-0.122 * dt$DIM_la * dt$DCF_tot_la) + (0.386 * dt$ECM * dt$DIM_la) + (-1.18 * dt$ECM * (dt$w_la)^0.75) + (-0.614 * dt$MCF * (dt$w_la)^0.75) + (-5.96 * (dt$w_la)^0.75) + (2.06 * dt$DIM_la) + (4.01 * (dt$w_la)^0.75))
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
    CO2_gd_p1 <- dt$N_par1 * (-6134 + (213 * dt$ECM) + (126 * (dt$w_la)^0.75) + (52.5 * dt$MCF) + (-5.13 * dt$DIM_la) + 1364 + (-0.122 * dt$DIM_la * dt$DCF_tot_la) + (0.386 * dt$ECM * dt$DIM_la) + (-1.18 * dt$ECM * (dt$w_la)^0.75) + (-0.614 * dt$MCF * (dt$w_la)^0.75) + (-1.03 * (dt$w_la)^0.75) + (2.49 * dt$DIM_la) + (0 * (dt$w_la)^0.75))
    # 2. parity
    CO2_gd_p2 <- dt$N_par2 * (-6134 + (213 * dt$ECM) + (126 * (dt$w_la)^0.75) + (52.5 * dt$MCF) + (-5.13 * dt$DIM_la) + 1364 + (-0.122 * dt$DIM_la * dt$DCF_tot_la) + (0.386 * dt$ECM * dt$DIM_la) + (-1.18 * dt$ECM * (dt$w_la)^0.75) + (-0.614 * dt$MCF * (dt$w_la)^0.75) + (-1.03 * (dt$w_la)^0.75) + (2.49 * dt$DIM_la) + (3.66 * (dt$w_la)^0.75))
    # 3.+ parity
    CO2_gd_p3 <- dt$N_par3 * (-6134 + (213 * dt$ECM) + (126 * (dt$w_la)^0.75) + (52.5 * dt$MCF) + (-5.13 * dt$DIM_la) + 1364 + (-0.122 * dt$DIM_la * dt$DCF_tot_la) + (0.386 * dt$ECM * dt$DIM_la) + (-1.18 * dt$ECM * (dt$w_la)^0.75) + (-0.614 * dt$MCF * (dt$w_la)^0.75) + (-1.03 * (dt$w_la)^0.75) + (2.49 * dt$DIM_la) + (4.01 * (dt$w_la)^0.75))
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
      CO2_gd_p1 <- dt$N_par1 * (8781 + (80.3 * dt$ECM) + (-4.66 * dt$DIM_la) - 49.0 + 0 + (0) + (-0.149 * dt$DIM_la * dt$DCF_tot_la) + (0.338 * dt$ECM * dt$DIM_la) + (6.05 * dt$DIM_la) + (-4.18 * dt$MCF))
      # 2. parity
      CO2_gd_p2 <- dt$N_par2 * (8781 + (80.3 * dt$ECM) + (-4.66 * dt$DIM_la) - 49.0 + 511 + (775) + (-0.149 * dt$DIM_la * dt$DCF_tot_la) + (0.338 * dt$ECM * dt$DIM_la) + (6.05 * dt$DIM_la) + (-10.5 * dt$MCF))
      # 3.+ parity
      CO2_gd_p3 <- dt$N_par3 * (8781 + (80.3 * dt$ECM) + (-4.66 * dt$DIM_la) - 49.0 + 1587 + (803) + (-0.149 * dt$DIM_la * dt$DCF_tot_la) + (0.338 * dt$ECM * dt$DIM_la) + (6.05 * dt$DIM_la) + (-28.8 * dt$MCF))
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
    CO2_gd_p1 <- dt$N_par1 * (8781 + (80.3 * dt$ECM) + (-4.66 * dt$DIM_la) - 2321 + 0 + (0) + (-0.149 * dt$DIM_la * dt$DCF_tot_la) + (0.338 * dt$ECM * dt$DIM_la) + (6.02 * dt$DIM_la) + (-4.18 * dt$MCF))
    # 2. parity
    CO2_gd_p2 <- dt$N_par2 * (8781 + (80.3 * dt$ECM) + (-4.66 * dt$DIM_la) - 2321 + 511 + (608) + (-0.149 * dt$DIM_la * dt$DCF_tot_la) + (0.338 * dt$ECM * dt$DIM_la) + (6.02 * dt$DIM_la) + (-10.5 * dt$MCF))
    # 3.+ parity
    CO2_gd_p3 <- dt$N_par3 * (8781 + (80.3 * dt$ECM) + (-4.66 * dt$DIM_la) - 2321 + 1587 + (1307) + (-0.149 * dt$DIM_la * dt$DCF_tot_la) + (0.338 * dt$ECM * dt$DIM_la) + (6.02 * dt$DIM_la) + (-28.8 * dt$MCF))
    } 
  CO2_kd <- (CO2_gd_p1 + CO2_gd_p2 + CO2_gd_p3) / 1000
  }
  CO2_kd
} 



calcCO2prod <- function(dt, farm = NULL, ECM = TRUE) { # function to create the data.table with all the information
  period <- dt[Farm == farm, unique(Period)] 
  rbindlist(lapply(period, function(P) {
    dt_sub <- dt[Farm == farm & Period == P, ]

    ###############
    ### CIGR model:

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

    ##### Model 1-3 with EFK values
    CO2_Kjel_mod1_la_EFK <- CO2_kjeldsen_mod1(dt_sub, EFK = TRUE)
    CO2_Kjel_mod2_la_EFK <- CO2_kjeldsen_mod2(dt_sub, EFK = TRUE)
    CO2_Kjel_mod3_la_EFK <- CO2_kjeldsen_mod3(dt_sub, EFK = TRUE)

    ##### Write table:
    data.table(
      Farm = farm,
      Period = P,
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
      CO2_CIGR_kkheca = CO2_CIGR_kk + CO2_CIGR_he + CO2_CIGR_ca,
      CO2_CIGR_total = CO2_CIGR_la + CO2_CIGR_dr + CO2_CIGR_kk + CO2_CIGR_he + CO2_CIGR_ca,
      CO2_CIGR_Kjel_total = CO2_CIGR_Kjel_la + CO2_CIGR_Kjel_dr + CO2_CIGR_kk + CO2_CIGR_he + CO2_CIGR_ca,
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

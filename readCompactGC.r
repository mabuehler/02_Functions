
#########################################
#########################################
#####                               #####
#####    read in Compact GC data    #####
#####                               #####
#########################################
#########################################



library(data.table)
library(readxl)


readCompactGC <- function(path, cut = TRUE) {
	# browser()
	files <- list.files(path, recursive = TRUE, full = TRUE, pattern = 'xls$')
	## make a dt and consider only xls files
	dt_files <- data.table(file = files)[grepl("xls$", file)]
	# extract info from the path name	
	dt_files[, relative := sub(paste0("^", path, "[/\\\\]?"), "", file)]
	dt_files[, parts := strsplit(relative, "[/\\\\]")]
	dt_files[, period := vapply(parts, function(x) x[grepl("^P[1-6]$", x)][1], character(1))]
	dt_files[, method := vapply(parts, function(x) x[1], character(1))]

	dt_files[, gas := vapply(parts, function(x) {
	    p <- which(grepl("^P[1-6]$", x))[1]
	    if (p == 2) {
	      # all orig / METHOD / P1
	      # Gas is encoded in METHOD
	      if (grepl("^CH4", x[1])) {"CH4"
	      } else if (grepl("^CO2", x[1])) {"CO2"
	      } else if (grepl("^N2", x[1])) {"N2"
	      } else {NA_character_
	      }
	    } else {
	      # all orig / METHOD / GAS / P1
	      x[p - 1]
	    }
	  },
	  character(1)
	)]

	# remove dublicates within the "P1" to "P6' folders
	dt_files <- dt_files[!duplicated(dirname(file))]
	# as we have the 'summary', it is only needed to read in one file per P folder.
	out <- lapply(1:dt_files[, .N], function(i) {
		i_path <- dt_files[i, file]
		suppressWarnings(dt_raw <- read_excel(i_path, sheet = 'Summary', skip = 13))
		setDT(dt_raw)
		setnames(dt_raw, c('V1', 'name', 'ret_time', 'area', 'height', 'amount', 'rel_Area', 'peak_Type'))
		## remove everything that is not a STEMUD sample
		dt <- dt_raw[grepl("^202.*[0-9]$", name) & !grepl("standby", name, ignore.case = TRUE)]
		## make a sample column
		dt[, sample := sub('.* - ', '', name)]
		## make some columns numeric
		suppressWarnings(dt[, c('ret_time', 'area', 'height', 'amount', 'rel_Area', 'sample') := lapply(.SD, as.numeric), .SDcols = c('ret_time', 'area', 'height', 'amount', 'rel_Area', 'sample')])
		## convert concentrations to real precentage
		dt[, conc := round(amount / 100, 6)]
		## set NA values to zero
		dt[is.na(conc), conc := 0]
		## add more info
		dt$method <- dt_files[i, method]
		dt$gas <- dt_files[i, gas]
		dt$period <- dt_files[i, period]
		# remove V1 column
		dt[, V1 := NULL]
		return(dt)
	})
	dt <- rbindlist(out)
	dt <- dt[, .(sample, period, method, gas, ret_time, area, height, amount, rel_Area, peak_Type, conc)]
	if(cut) {
		dt <- dt[, .(sample, period, method, gas, conc, height)]
	}
	return(dt)
}


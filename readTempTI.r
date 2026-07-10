
###############################################
###############################################
#####                                     #####
#####    read temperature data from TI    #####
#####                                     #####
###############################################
###############################################


library(data.table)
library(readxl)

source('C:/GitHub_repos/repos_AU/02_Functions/convert_date.r')

##################################################


readTempTI <- function(path, From, To) {
# browser()
ls_files <- list.files(path, pattern = '^K.*xlsx$', recursive = TRUE, full.names = TRUE)

ls_temp <- lapply(ls_files, function(x) {
	temp_xlsx <- read_xlsx(x, na = 'NA')
	temp_raw <- as.data.table(temp_xlsx)
	temp <- temp_raw[12:nrow(temp_raw),]
	temp[, device := substr(names(temp_raw)[1], nchar(names(temp_raw)[1])-4+1, nchar(names(temp_raw)[1]))]
	setnames(temp, c('ID', 'st', 'Temp', 'RH', 'rm1', 'rm2', 'Device'))
	temp[, c('ID', 'rm1', 'rm2') := NULL]
})
out <- rbindlist(ls_temp)
out[, st := convert_date(st, tz = 'CET')]
out[, Temp := as.character(Temp)]
out[, RH := as.character(RH)]
out
}



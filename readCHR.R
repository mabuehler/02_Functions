
##############################################
##############################################
#####                                    #####
#####    Function to read in CHR data    #####
#####                                    #####
##############################################
##############################################

library(data.table)
library(readxl)


readCHR <- function(x){
	# browser()
	if(dir.exists(x)){
		files <- list.files(x,recursive=TRUE,full.names=TRUE)} else {
			files <- x}

	##### data table:
	dt_out <- lapply(files,function(file){
		# browser()
		dt0 <- suppressMessages(as.data.table(read_xls(file)))
		i_skip <- which(dt0[,1] == 'CKR-nr.')
		i_range <- which(dt0[,1] == 'Ejer')
		if(length(i_skip) >1){
			cat("Warning: There are more than one list in the excel document. Just the first 'Besætningsliste' is chosen. \n\n   ")
			i_end <- which(is.na(dt0[i_skip[1]+1:.N,1]))[1]
			dt1 <- suppressMessages(as.data.table(read_xls(file,skip=i_skip[1],n_max=i_end+1)))
		} else {
			dt1 <- suppressMessages(as.data.table(read_xls(file,skip=i_skip[1])))
		}
		setnames(dt1, c('CKR','Birth','Import','Heardnumber_import','Export','Export_place','Heardnumber_export','Sex','Race','CKR_mother'))
		
		# make sure that columns have the same class even if they are empty! --- missing
		dt1[, c('Import','Heardnumber_import','Export_place','Heardnumber_export') := lapply(.SD, as.character), .SDcols = c('Import','Heardnumber_import','Export_place','Heardnumber_export')]
		dt1[,Export := as.POSIXct(Export)]

		##### add info about barn:
		h1 <- suppressMessages(as.data.table(read_xls(file,range=paste0('A1:B',i_range+2),col_names=FALSE)))
		setnames(h1, c('v1','v2'))
		CHRnr <- h1[shift(v1,type='lag') == 'CHR nr.',v1]
		name <- h1[v1== 'Navn',v2]
		address1 <- h1[v1== 'Adresse',v2]
		address2 <- h1[shift(v1,type='lag') == 'Adresse',v2]
		address3 <- h1[shift(v1,type='lag',2) == 'Adresse',v2]
		if(is.na(address3)){
			address <- paste(address1,address2,sep=', ')
		} else {address <- paste(address1,address2,address3,sep=', ')}
		dates <- unlist(regmatches(h1[1], gregexpr("\\b\\d{2}\\.\\d{2}\\.\\d{4}\\b", h1[1])))
		period <- paste0(dates, collapse = " - ")

		##### Combine the information:
		dt1[,CHR := CHRnr]
		dt1[,Name := name]
		dt1[,Address := address]
		dt1[,Period := period]
		

	# calculate age in months based on the middle of the period
	dt1[, Age_days := round(as.numeric(difftime(as.POSIXct(gsub("(.*) - (.*)", "\\1", Period), format = "%d.%m.%Y")+(as.POSIXct(gsub("(.*) - (.*)", "\\2", Period), format = "%d.%m.%Y") - 
	    	as.POSIXct(gsub("(.*) - (.*)", "\\1", Period), format = "%d.%m.%Y")) / 2, Birth, units = "days")),0)]
	dt1
})

	out <- rbindlist(dt_out)
	out
}



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
	
	files <- list.files(Path,recursive=TRUE,full.names=TRUE)

	##### data table:
	dt1 <- suppressMessages(as.data.table(read_xls(files,skip=23)))
	setnames(dt1, c('CKR','Birth','Import','Heardnumber_import','Export','Export_place','Heardnumber_export','Sex','Race','CKR_mother'))

	##### add info about barn:

	h1 <- suppressMessages(as.data.table(read_xls(files,range='A1:B18',col_names=FALSE)))
	setnames(h1, c('v1','v2'))
	CHRnr <- h1[shift(v1,type='lag') == 'CHR nr.',v1]
	name <- h1[v1== 'Navn',v2]
	address1 <- h1[v1== 'Adresse',v2]
	address2 <- h1[shift(v1,type='lag') == 'Adresse',v2]
	address <- paste(address1,address2,sep=', ')
	dates <- unlist(regmatches(h1[1], gregexpr("\\b\\d{2}\\.\\d{2}\\.\\d{4}\\b", h1[1])))
	period <- paste0(dates, collapse = " - ")

	##### Combine the information:
	dt1[,CHR := CHRnr]
	dt1[,Name := name]
	dt1[,Address := address]
	dt1[,Period := period]
	
	print(dt1)
}


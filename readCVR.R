
#####################################
#####################################
#####                           #####
#####    Access CVR register    #####
#####                           #####
#####################################
#####################################


library(httr)
library(jsonlite)
library(data.table)
library(readxl)


# CHR <- as.data.table(read_xlsx('C:/Users/au711252/OneDrive - Aarhus universitet/Desktop/V_4061GR_22_ISKV1_B_DYRERK_6B.xlsx'))
CHR <- as.data.table(read_xlsx('C:/Users/au711252/OneDrive - Aarhus universitet/Desktop/DYRE.xlsx'))
# CHR[Dyreart == 'Kvæg']

# dt <- CHR[Dyreart == 'Kvæg' & Staldtype %in% c('Sengestald med spaltegulv (kanal, bagskyl eller ri','Sengestald med spaltegulv (kanal, bagskyl el. ring
# ','Sengestald med spalter (kanal, bagskyl eller ringk'),]

CVR_nr <- unique(dt[,CVR])

url <- 'https://cvrapi.dk/api?search='
country <- '&country=dk'

CVR_list <- lapply(CVR_nr, function(x){
	v1 <- GET(paste0(url,x,country))
	CVR_raw <- fromJSON(rawToChar(v1$content))
	out <- data.table(CVR = CVR_raw$vat, Name = CVR_raw$name, Address = CVR_raw$address, Zipcode = CVR_raw$zipcode, Phone = CVR_raw$phone, Email = CVR_raw$email
	, Production = CVR_raw$industrydesc)
  	out
})


url2 <- 'https://apicvr.dk/api/v1/'

CVR_list_2 <- lapply(CVR_nr[57:150], function(x){
	v1 <- GET(paste0(url2,x))
	CVR_raw <- fromJSON(rawToChar(v1$content))
	out <- data.table(CVR = CVR_raw$vat, Name = CVR_raw$name, Address = CVR_raw$address, Zipcode = CVR_raw$zipcode, Phone = CVR_raw$phone, Email = CVR_raw$email
	, Production = CVR_raw$industrydesc)
  	out
})

CVR_data_2 <- rbindlist(CVR_list_2,fill=TRUE)

CVR_list_3 <- lapply(CVR_nr, function(x){
	v1 <- GET(paste0(url2,x))
	CVR_raw <- fromJSON(rawToChar(v1$content))
	out <- data.table(CVR = CVR_raw$vat, Name = CVR_raw$name, Address = CVR_raw$address, Zipcode = CVR_raw$zipcode, Phone = CVR_raw$phone, Email = CVR_raw$email
	, Production = CVR_raw$industrydesc)
  	out
})

CVR_data_3 <- rbindlist(CVR_list_3,fill=TRUE)

dt <- CHR
dt[,CHR := as.numeric(gsub("\\.", "", as.character(get('CHR-nr.',.SD))))]
dt[,Heard_nr := as.numeric(gsub("\\.", "", as.character(get('Besætningsnr.',.SD))))]

dt_merge <- merge(dt,CVR_data_3,by='CVR')

fwrite(dt_merge, 'C:/Users/au711252/OneDrive - Aarhus universitet/Desktop/CVR.txt',sep=";")





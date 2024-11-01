
library(data.table)
library(ibts)
library(plotly)
library(httr)
library(jsonlite)

createDMI <- function(API,cellId,req_datetime){
##### Define variables that are always the same:
url <- 'https://dmigw.govcloud.dk/v2/climateData/collections/10kmGridValue/items?' # url for accessing grid data
req_parameter <- c('mean_temp', 'mean_wind_dir','mean_wind_speed','mean_pressure','acc_precip') # parameters I wanna have
timeResolution <- 'timeResolution=hour'
limit <- 'limit=300000' # max data

date_list <- lapply(req_datetime, function(j){
  datetime <- paste0('datetime=',j)
par_list <- lapply(req_parameter, function(x){
  parameterId <- paste0('parameterId=', x)
  v7 <- GET(paste(url,cellId,datetime,parameterId,timeResolution,limit,API,sep='&'))
  data_raw <- fromJSON(rawToChar(v7$content))
  out <- as.data.table(data_raw[[2]][[4]])
  out[,st := as.POSIXct(from, format='%Y-%m-%dT%H:%M:%S',tz='UTC')]
  out[,et := as.POSIXct(to, format='%Y-%m-%dT%H:%M:%S',tz='UTC')]
  out[,.(st,et,parameterId,value)][order(st)]
})
rbindlist(par_list)
})
Climate_data <- rbindlist(date_list)
DMI <- dcast(Climate_data[,.(st,et, parameterId,value)], st + et ~ parameterId)
DMI
}


library(data.table)
library(ibts)
library(httr)
library(jsonlite)

createDMI <- function(API,cellId,req_datetime,req_parameter=c('mean_temp', 'mean_wind_dir', 'mean_wind_speed', 'mean_pressure', 'acc_precip'), 
  url = 'https://dmigw.govcloud.dk/v2/climateData/collections/10kmGridValue/items?', resolution = 'hour') {
  ##### Define variables that are always the same:
  timeResolution <- paste0('timeResolution=', resolution)
  limit <- 'limit=300000' # max data
  date_list <- lapply(req_datetime, function(j) {
    datetime <- paste0('datetime=', j)
    par_list <- lapply(req_parameter, function(x) {
# browser()
      parameterId <- paste0('parameterId=', x)
      v7 <- GET(paste(url, cellId, datetime, parameterId, timeResolution, limit, API, sep = '&'))
      data_raw <- fromJSON(rawToChar(v7$content))
      out <- as.data.table(data_raw[[2]][[4]])
      out[, st := as.POSIXct(from, format = '%Y-%m-%dT%H:%M:%S', tz = 'UTC')]
      out[, et := as.POSIXct(to, format = '%Y-%m-%dT%H:%M:%S', tz = 'UTC')]
      out[, .(st, et, parameterId, value)][order(st)]
    })
    rbindlist(par_list)
  })
  Climate_data <- rbindlist(date_list)
  DMI <- dcast(Climate_data[, .(st, et, parameterId, value)], st + et ~ parameterId)
  DMI
}


stationDMI <- function(API, stationId, req_datetime, req_parameter = c('temp_dry', 'wind_dir','wind_speed','pressure'), 
  url = 'https://dmigw.govcloud.dk/v2/metObs/collections/observation/items?') {
##### Define variables that are always the same:
# browser()
  limit <- 'limit=300000' # max data
  stationId <- paste0('stationId=', stationId)
  date_list <- lapply(req_datetime, function(j) {
    datetime <- paste0('datetime=', j)
# browser()
    par_list <- lapply(req_parameter, function(x) {
  # browser()
      parameterId <- paste0('parameterId=', x)
      v7 <- GET(paste(url, stationId, datetime, parameterId, limit, API, sep = '&'))
      data_raw <- fromJSON(rawToChar(v7$content))
      dt <- as.data.table(data_raw[[2]][[4]])
      dt[, st := as.POSIXct(observed, format = '%Y-%m-%dT%H:%M:%S',tz = 'UTC')]
      dt[, help := shift(st)]
      dt[, et := st + round(mean(help - st, na.rm = TRUE))]
      out <- dt[, .(st, et, parameterId, value)][order(st)]
      out
    }) 
    rbindlist(par_list)
  })
  Climate_data <- rbindlist(date_list)
  DMI <- dcast(Climate_data[, .(st, et, parameterId, value)], st + et ~ parameterId)
  DMI
}

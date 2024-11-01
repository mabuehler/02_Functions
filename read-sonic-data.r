
###################################################################################
###################################################################################
#####                                                                         #####
#####    Functions to read in different type of ultrasonic anemometer data    #####
#####                                                                         #####
###################################################################################
###################################################################################


library(data.table)
library(ibts)


#### To Do:
# - neue readWindMaster Routine
# - Plotting & anderes Gheu aufräumen
read_windmaster_ascii <- function(FilePath, tz = "Etc/GMT-1"){

    ##########################################################################################
    ##########################################################################################
    #####                                                                                #####
    #####    This function was written by Christoph Häni and adapted by Marcel Bühler    #####
    #####    The original script can be accessed on:                                     #####
    #####    https://github.com/hafl-gel/gel-scripts/blob/main/read-sonic-data.r         #####
    #####                                                                                #####
    ##########################################################################################
    ##########################################################################################
    
      ### get Date
      bn <- basename(FilePath)
      if(!grepl("^data_", bn)){
            # run old script
            return(read_windmaster_old_ascii(FilePath, tz))
      }
    if (grepl('[.]gz$', bn)) {
        if (!any(grepl('R.utils', installed.packages()[, 'Package']))) {
            stop('package "R.utils" must be installed to process gz files!')
        }
    }
    # be verbose
    cat("File:", path.expand(FilePath), "- ")
      Date <- gsub("^data_.*_([0-9]{8})_.*", "\\1", bn)
      ### read File
    raw <- readLines(FilePath, warn = FALSE)
    # filter out erroneous multibyte strings
    raw <- raw[grepl('^\\d{2}[0-9.:]+,[^,]+,([0-9.+-]+,){3}M,([0-9.+-]+,){2}[^,]+$', raw, 
        useBytes = TRUE)]
    out <- fread(text = raw, header = FALSE, na.strings = '999.99', showProgress = FALSE, 
        blank.lines.skip = TRUE)
    # check if file is empty
      if(nrow(out) == 0){
        cat('no valid data\n')
            return(NULL)
      }
    # check which columns to convert columns if necessary
    vnums <- paste0('V', c(3, 4, 5, 7))
    is.char <- out[, sapply(.SD, is.character), .SDcols = vnums]
    # convert to numeric
    out[, vnums[is.char] := {
        lapply(.SD, as.numeric)
    }, .SDcols = vnums[is.char]]
    # remove NA lines that come from conversion
    out <- na.omit(out)
    if (out[, .N == 0]) {
        cat('no valid data\n')
        return(NULL)
    }
    # be verbose and print sonic names:
    sonic_label <- out[, sub('^[^A-Z]*', '', unique(V2))]
    sonic_label <- unique(sonic_label[!(sonic_label == '')])
    if (length(sonic_label) == 0) stop('sonic label not available!')
    if (length(sonic_label) > 1) stop('more than one unique sonic label!')
    cat(paste0("data recorded by sonic-", tolower(sonic_label), "\n"))
    sonic_file <- sub("data_(.*)_[0-9]{8}_[0-9]{6}([.]gz)?$", "\\1", bn)
    if(sonic_label != toupper(sub("sonic-", "", sonic_file))){
        warning(paste0("Sonic label '", sonic_label, "', and hostname '", sonic_file, "' don't match!"), call. = FALSE)
    }
    # check units
    if(out[, V6[1]] != "M"){
        stop("Units of recorded data not compatible with evaluation script! Column 6 should contain 'M' for m/s!")
    }
    # fix time etc.
    out[, c(
        # remove columns
        'V1', 'V2', 'V6', 'V8', 'V9',
        # add columns
        'sonic', 'Time', 'Hz',
        # replace °C by K
        'V7'
        ) := {
        # set times correctly
        st.dec <- fast_strptime(paste(Date, V1), lt = FALSE, format = "%Y%m%d %H:%M:%OS", tz = "Etc/GMT-1")
        # get Hz (faster than as.factor(sub('[.].*', '', V1)) !
        Hz <- round(median(tabulate(trunc(as.numeric(st.dec) - as.numeric(st.dec[1])))), -1)
        if (
            hour(st.dec[.N]) == 0 && 
            (hour(st.dec[1]) != 0 || .N > (3 * Hz))
        ) {
            ## fix last 3 * Hz entries, where hour == 0
            sub.st <- st.dec[.N - seq_len(3 * Hz) + 1]
            hr <- hour(sub.st)
            st.dec[.N - seq_len(3 * Hz) + 1] <- fifelse(hr == 0, sub.st + 24 * 3600, sub.st)
        }
        # return list
        list(
            # remove columns
            NULL, NULL, NULL, NULL, NULL,
            # sonic
            sonic_label,
            # Time
            st.dec,
            # Hz
            Hz,
            # T
            V7 + 273.15
            )
    }]
    ### set Output names and order
    setnames(out, c("u", "v", "w", "T", "sonic", "Time","Hz"))
    setcolorder(out,c("Time","Hz","u","v","w","T", "sonic"))
    # check if GillBug affected sonic
    setattr(out, 'GillBug', sonic_label %in% c('C', 'D'))
    # return
    out
}

read_windmaster_old_ascii <- function(FilePath, tz = "Etc/GMT-1"){

    ##########################################################################################
    ##########################################################################################
    #####                                                                                #####
    #####    This function was written by Christoph Häni                                 #####
    #####    The original script can be accessed on:                                     #####
    #####    https://github.com/hafl-gel/gel-scripts/blob/main/read-sonic-data.r         #####
    #####                                                                                #####
    ##########################################################################################
    ##########################################################################################

      ### get Date
      bn <- basename(FilePath)
      Date <- gsub("^..._([0-9]{6})_.*","\\1",bn)
    ### check if rg is available
    use_rg <- try(system('rg -V', intern = TRUE), silent = TRUE)
    # use_rg <- length(system('command -v rg', intern = TRUE)) > 0
      ### read File
      # browser()
    if (inherits(use_rg, 'try-error')) {
        suppressWarnings(out <- fread(cmd=paste0("grep -v -e ',,' -e '[A-Za-z]' '",path.expand(FilePath),"'"),fill = TRUE,blank.lines.skip = TRUE))
    } else {
        suppressWarnings(out <- fread(cmd=paste0("rg -v -e ',,' -e '[A-Za-z]' '",path.expand(FilePath),"'"),fill = TRUE,blank.lines.skip = TRUE))
    }
      if(nrow(out) == 0){
            cat("File empty:",path.expand(FilePath),"\n")
            return(NULL)
      }
      # remove first (empty) column
    if ('V1' %in% names(out)) out[, V1 := NULL]
    # remove NAs
      out <- na.omit(out)
    ## get Hz
    Hz <- out[, .N, by = V2][, round(median(N), -1)]
    out[, Hz := Hz]
      ## set times
      out[, st.dec := fast_strptime(paste0(Date,V2),lt = FALSE,format = "%y%m%d%H:%M:%S",tz = "Etc/GMT-1")+V3][,c("V2","V3"):=NULL]
    ## fix end of day
    if (out[, 
        hour(st.dec[.N]) == 0 && 
        (hour(st.dec[1]) != 0 || .N > (3 * Hz[1]))
        ]) {
        ## fix last 3 * Hz entries, where hour == 0
        out[.N - seq_len(3 * Hz[1]) + 1, st.dec := {
            hr <- hour(st.dec)
            fifelse(hr == 0, st.dec + 24 * 3600, st.dec)
        }]
    }
      ### set Output names and order
      setnames(out,c("u", "v", "w", "T", "Hz", "Time"))
      setcolorder(out,c("Time","Hz","u","v","w","T"))
      ### remove 999.99 entries
      out <- out[!(u%in%999.99|v%in%999.99|w%in%999.99|T%in%999.99),]
      ### change units from °C to K
      out[, T := T + 273.15]
    setattr(out, "GillBug", TRUE)
      out
}

readSonicEVS_csv <- function(FilePath, tz = "Etc/GMT-1"){

    ##########################################################################################
    ##########################################################################################
    #####                                                                                #####
    #####    This function was written by Christoph Häni                                 #####
    #####    The original script can be accessed on:                                     #####
    #####    https://github.com/hafl-gel/gel-scripts/blob/main/read-sonic-data.r         #####
    #####                                                                                #####
    ##########################################################################################
    ##########################################################################################

      ### read File
      suppressWarnings(out <- fread(cmd=paste0("grep -v -e ',,' -e [A-Za-z] '", 
            path.expand(FilePath), "'"), fill = TRUE, blank.lines.skip = TRUE))
      ### remove rows with NA
      out <- na.omit(out)
      ### set times
      out[,st.dec := fast_strptime(paste(V1, V2), lt = FALSE, format = "%d.%m.%Y %H.%M.%OS",
            tz = tz)][,c("V1","V2"):=NULL]
      # get start time
      start_time <- out[, as.POSIXct(trunc(st.dec[1]))]
      out[, dt := trunc(as.numeric(st.dec - start_time, units = "secs"))]
      # correct new day
      out[(seq_len(.N) > 30) & dt == 0, dt := dt + 24 * 3600]
      out[dt < 0, dt := dt + 24 * 3600 - 1]
      # add st column
      out[, st := start_time + dt]
      # add Hz column
      Hz <- out[, .N, by = trunc(dt)][, round(median(N), -1)]
      out[, Hz := Hz]
      # remove columns
      out[, c("st.dec", "dt") := NULL]
      ### set Output names and order
      setnames(out,c("u", "v", "w", "T", "Time","Hz"))
      setcolorder(out,c("Time","Hz","u","v","w","T"))
      ### remove 999.99 entries
      out <- out[!(u%in%999.99|v%in%999.99|w%in%999.99|T%in%999.99),]
      ### change units from °C to K
      out[,T := T + 273.15]
      out
}


readSonicFlavia_csv <- function(FilePath, tz = "Etc/GMT-1"){

####################################################################
####################################################################
#####                                                          #####
#####    This function was written by Marcel Bühler and was    #####
#####    intended to read ultrasonic data from the sonic of    #####
#####    the University of Torino. I'm pretty sure the         ##### 
#####    function doesn't work anymore.                        #####
#####                                                          #####
####################################################################
####################################################################

  ### read File
  # browser()
  suppressWarnings(out <- fread(cmd=paste0("grep -v -e ',,' -e [A-Za-z] '", 
    path.expand(FilePath), "'"), fill = TRUE, blank.lines.skip = TRUE))
  ### remove rows with NA
  out <- na.omit(out)
  ### remove the unnecessary first column
  out[,V1 := NULL]
  ### set times
  out[,st.dec := fast_strptime(V2, lt = FALSE, format = "%Y/%m/%d %H:%M:%S",
    tz = tz)][,c("V2"):=NULL]
  # get start time
  start_time <- out[, as.POSIXct(trunc(st.dec[1]))]
  out[, dt := trunc(as.numeric(st.dec - start_time, units = "secs"))]
  # correct new day
  out[(seq_len(.N) > 30) & dt == 0, dt := dt + 24 * 3600]
  out[dt < 0, dt := dt + 24 * 3600 - 1]
  # add st column
  out[, st := start_time + dt]
  # add Hz column
  diff <- diff(out[,st])
  Hz <- median(diff,na.rm=TRUE)
  out[, Hz := as.numeric(Hz)]
  # remove columns
  out[, c("st.dec", "dt") := NULL]
  ### set Output names and order
  setnames(out,c("u", "v", "w", "T", "Time","Hz"))
  setcolorder(out,c("Time","Hz","u","v","w","T"))
  ### remove 999.99 entries
  out <- out[!(u%in%999.99|v%in%999.99|w%in%999.99|T%in%999.99),]
  ### change units from °C to K
  out[,T := T + 273.15]
  out
}


readSonicAU_txt <- function(FilePath, tz_sonic = "CET"){

    ##################################################################
    ##################################################################
    #####                                                        #####
    #####    This function was written by Marcel Bühler          #####
    #####    The function is based on the                        #####
    #####    "read_windmaster_ascii" function by Christoph Häni  #####
    #####                                                        #####
    ##################################################################
    ##################################################################
    
    ### get Date
  # browser()
    bn <- basename(FilePath)
    # be verbose
    cat("File:", path.expand(FilePath), "- ")
    Date <- gsub("[^0-9]", "", bn)# Remove non-digit characters
    Date <- substr(Date, nchar(Date) - 11, nchar(Date))
    ### read File (I does not seem necessary to filter out errorenous strings
    out <- fread(FilePath, header = FALSE, na.strings = '999.99', showProgress = FALSE, 
        blank.lines.skip = TRUE,colClasses = list(character = 'V1'))
    # check if file is empty
      if(nrow(out) == 0){
        cat('no valid data\n')
            return(NULL)
      }
    # for some fucking reason, the data is doubled.
    out <- unique(out)  
    # # check which columns to convert if necessary to numeric
    # vnums <- paste0('V', c(2:5))
    # is.char <- out[, sapply(.SD, is.character), .SDcols = vnums]
    # # convert to numeric
    # out[, vnums[is.char] := {
    #     lapply(.SD, as.numeric)
    # }, .SDcols = vnums[is.char]]
    # # remove NA lines that come from conversion
    # out <- na.omit(out)
    # if (out[, .N == 0]) {
    #     cat('no valid data\n')
    #     return(NULL)
    # }
    # be verbose and print sonic names:
    sonic_label <- substr(bn,1,6)
    cat(paste0("data recorded by sonic-", tolower(sonic_label), "\n"))
    # fix time etc.
    out[, c(
        # remove columns
        'V1','V2','V5','V9','V11','V12',
        # add columns
        'sonic', 'Time', 'Hz',
        # replace °C by K
        'V10'
        ) := {
# browser()      
        # set times correctly
        st.dec <- fast_strptime(paste(V1,V2*1000,sep='.'), lt = FALSE, format = "%d-%m-%y %H:%M:%OS", tz = tz_sonic)
        # get Hz (faster than as.factor(sub('[.].*', '', V1)) !
        Hz <- round(median(tabulate(trunc(as.numeric(st.dec,unit='secs') - as.numeric(st.dec[1],unit='secs'))))) # it is 16 Hz
        # Hz <- round(median(tabulate(trunc(as.numeric(st.dec,unit='secs') - as.numeric(st.dec[1],unit='secs'))))/5)*5
        # return list
        list(
            # remove columns
            NULL, NULL, NULL, NULL, NULL, NULL,
            # sonic
            sonic_label,
            # Time
            st.dec,
            # Hz
            Hz,
            # T
            V10 + 273.15
            )
    }]
    # browser()
    ### set Output names and order
    setnames(out, c('Temp','Press',"u", "v", "w", "T", "sonic", "Time","Hz"))
    setcolorder(out,c("Time","Hz","u","v","w","T", "sonic",'Temp','Press'))
    # check if GillBug affected sonic
    setattr(out, 'GillBug', sonic_label %in% c('C', 'D')) # <-------------- ask Jesper if Sonic is affected by Gillbug
    # order data
    setkey(out,Time)
    # return
    out
}



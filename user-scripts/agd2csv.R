# Developed by: Vincent van Hees
# Objective: convert ActiGraph .agd files from various generations to csv format that GGIR can then read

library(RSQLite)
library(utils)
library(foreach)

desiredtz = "Europe/Amsterdam"

# .agd location 
datadir =  "D:/agd"

# where to store the resulting csv files?
actigraphdir = "D:/projectoutput"

#===============================================================================
fns = dir(datadir, full.names = TRUE)

cores = parallel::detectCores()
Ncores = cores[1]
if (Ncores > 3) {
  Ncores2use = min(c(Ncores - 1, 10, length(fns)))
  cl <- parallel::makeCluster(Ncores2use) # not to overload your computer
  doParallel::registerDoParallel(cl)
} else {
  stop("Code assumes that there are more than 3 CPU cores available")
}

packages2passon = 'RSQLite'
errhand = 'pass'
`%myinfix%` = foreach::`%dopar%`
output_list = foreach::foreach(i = 1:length(fns), .packages = packages2passon,
                               .export = NULL, .errorhandling = errhand) %myinfix% {
                                 tryCatchResult = tryCatch({
                                   # for (i in 1:length(fns)) {
                                   filename = fns[i]
                                   t0 = Sys.time()
                                   # Following 10 lines derived from
                                   # https://github.com/github-js/acc/blob/master/R/readCounts.R
                                   # but modified as their code does not recognise that it is a uniaxial GT3Xplus
                                   con = DBI::dbConnect(RSQLite::SQLite(), dbname = filename)
                                   settings <- DBI::dbReadTable(con, "settings")
                                   tables = dbListTables(con, ".tables")
                                   settings$settingName = tolower(settings$settingName)
                                   deviceName <- settings$settingValue[settings$settingName == "devicename"]
                                   deviceVersion <- settings$settingValue[settings$settingName == "deviceversion"]
                                   serialNumber <- settings$settingValue[settings$settingName == "deviceserial"]
                                   epochlength <- as.numeric(as.character(settings$settingValue[settings$settingName == "epochlength"]))
                                   startdatetime <- settings$settingValue[settings$settingName ==  "startdatetime"]
                                   softwareVersion <- settings$settingValue[settings$settingName ==  "softwareversion"]
                                   filter <- settings$settingValue[settings$settingName ==  "filter"]
                                   datetimeformat <- settings$settingValue[settings$settingName ==  "datetimeformat"]
                                   if (length(softwareVersion) == 0) softwareVersion = ""
                                   
                                   startdatetime2 <- as.POSIXlt((as.numeric(startdatetime)/1e+07),
                                                                origin = "0001-01-01 00:00:00", tz = "GMT")
                                   startDate <- substring(startdatetime2, 1, 10)
                                   counts <- DBI::dbReadTable(con, "data")
                                   dbDisconnect(con)
                                   counts <- counts[complete.cases(counts), ]
                                   timeline = (0:as.integer((dim(counts)[1]) - 1) * epochlength)
                                   rawTimeStamp = rep(startdatetime2, dim(counts)[1])
                                   rst = gsub(" GMT", "", as.POSIXlt(rawTimeStamp, tz = "GMT") + timeline)
                                   if (all(c("axis1", "axis2", "axis3") %in% colnames(counts))) {
                                     type <- "triaxial"
                                     counts = counts[, c("axis1", "axis2", "axis3")]
                                   } else {
                                     type <- "uniaxial"
                                     counts = counts[, "axis1"]
                                   }
                                   data = data.frame(TimeStamp = as.vector(rst), counts = counts)
                                   if (length(data) > 0) {
                                     # Extract windowsize
                                     tmp = as.POSIXlt(data$TimeStamp[1:2], format = "%Y-%m-%d %H:%M:%S", tz = desiredtz)
                                     epochSize = as.numeric(difftime(tmp[2], tmp[1], units = "secs"))
                                     if (length(epochSize) == 0) epochSize = epochlength
                                     # Create file header for the Actigraph file
                                     start = tmp[1]
                                     starttime = strftime(start, format = "%H:%M:%S")
                                     startdate = paste0(start$mon + 1, "/", start$mday, "/", start$year + 1900) #month day year 
                                     header = c(paste0("------------ Data File Converted from agd to csv for ActiGraph ",
                                                       deviceName, " ActiLife ", softwareVersion, " Firmware v", deviceVersion,
                                                       " date format ", datetimeformat, " Filter ", filter , " -----------"),
                                                paste0("Serial Number: ", serialNumber), 
                                                paste0("Start Time ", starttime), 
                                                paste0("Start Date ", startdate), 
                                                paste0("Epoch Period (hh:mm:ss) 00:00:",
                                                       ifelse(test = nchar(as.character(epochSize)) == 1, yes = "0", no = ""), epochSize),
                                                "Download Time ***",
                                                "Download Date ***", 
                                                "Current Memory Address: 0",
                                                "Current Battery Voltage: *.**     Mode = **",
                                                "--------------------------------------------------")
                                     
                                     # Store to csv file
                                     fname = unlist(strsplit(basename(filename), "[.]agd"))
                                     fname_full = paste0(actigraphdir, "/", fname, "_", type, ".csv")
                                     way_to_save = "original" #"original" # to aid investigating whether way to save file matters
                                     if (way_to_save == "original") {
                                       cat(paste(header, "\n", collapse = ""), file = fname_full)
                                       write.table(data, file = fname_full, row.names = FALSE, 
                                                   col.names = FALSE, sep = ",", fileEncoding = "UTF-8", append = TRUE)
                                     } else if (way_to_save == "ruben") {
                                       sink(fname_full)
                                       for (hi in 1:length(header)) {
                                         cat(paste0(header[hi],"\n"))
                                       }
                                       sink()
                                       write.table(data, fname_full, append = TRUE, col.names = FALSE, row.names = FALSE, sep = ',')
                                     }
                                   }
                                   t1 = Sys.time()
                                   cat(paste0(round(as.numeric(difftime(time1 = t1, time2 = t0, units = "secs")), digits = 2), "secs "))
                                   # }
                                 })
                                 return(tryCatchResult)
                               }
on.exit(parallel::stopCluster(cl))

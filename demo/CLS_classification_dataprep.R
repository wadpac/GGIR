rm(list=ls())
graphics.off()

# In this file I match activity log data with accelerometer data and 
# then store cleaned log data with a reference to the corresponding accelerometer file.
# This is only needed for the pilot study data, in the real study the data will be in a more convenient format

app = "D:/dropbox/Dropbox/Accelerometry/GGIR/development/input_cls/time_use_app_withIM_m.sav"
onlinepaper = "D:/dropbox/Dropbox/Accelerometry/GGIR/development/input_cls/time_use_online_paper_m.sav"
selectdaysfile = "D:/dropbox/Dropbox/Accelerometry/GGIR/development/input_cls/wear_codes.csv"
pathraw = "D:/dropbox/Dropbox/Accelerometry/GGIR/development/output_Millenium dress rehearsal/meta/raw"
require(foreign)

#=======================================
# Variables interpreation of online and paper log:
# "slot"  1:144
# "acttype" character with 45 categories incl NA
# "activity"  yes or no
# "activity2" no or NA
# "tudday"  day of measurement 1 or 2
# "tudm"  method: online paper
# "ddate1"  date in dd/mm/yyyy format for first day in measurement
# "ddate"    UNCLEAR
#  "fq1"   Outdoor (3) Indoors but not at home (2) or at home (1)
# "fq2a ... fq2f"   UNCLEAR
#"fq3"      seems some kind of satisfaction survey????
# "Monitor" Serial number device
#---------------------------------------
# Variable interpretation app log:
# "cnum" filled with ones
# "episode" number of activity in a day
#"stime"
#"etime"
#"activity" names of activity types
#"actp" names of activity types
#"ACTP0" UNCLEAR, THIS IS EMPTY? MAYBE SOMETHING WENT WRONG WITH LOADING

#-------------------------------------------------------
# Extract data from app logs
D = read.spss(file=app,to.data.frame = TRUE,trim.factor.names = TRUE)
D = D[,c(3:9,20)]
# Extract data from online log and merge with app log
D2 = read.spss(file=onlinepaper,to.data.frame = TRUE,trim.factor.names = TRUE)
D2 = D2[which(D2$activity == "Yes"),]
D2 = aggregate(D2,by=list(D2$acttype,D2$activity2,D2$tudday,D2$tudm,D2$Monitor),function(x){x[1]})
D2$stime = D2$slot
D2$etime = c(D2$slot[2:nrow(D2)] - 1,144)
D2 = D2[,c(12,10,11,23,24,7,22)] 
D2$actp = 0
colnames(D2)[which(colnames(D2)=="acttype")] = "activity"
# reformat time stamps to be in line with form of app log
epoch2time = function(x) {
  etm = x * 10
  etm  = etm + (4*60) - 10
  hrs = floor(etm/60)
  min = etm - (hrs*60)
  if (hrs > 23) hrs = hrs - 24
  hrs0 = which(hrs == 0); hrs = as.character(hrs)
  if (length(hrs0) > 0) hrs[hrs0] = "00"
  min0 = which(min == 0); min = as.character(min)
  if (length(min0) > 0) min[min0] = "00"
  epoch2time = paste0(hrs,":",min,":00")
}
swappedtimes = which(D2$stime > D2$etime)
is = which(colnames(D2) == "stime")
ie = which(colnames(D2) == "etime")
if (length(swappedtimes) > 0) {
  D2[swappedtimes,c(is:ie)] = D2[swappedtimes,c(ie:is)]
}
D2$stime = epoch2time(D2$stime)
D2$etime = epoch2time(D2$etime)
D = rbind(D,D2)

#-
D$activity = gsub(" ", "",D$activity, fixed = TRUE)
D$actp = gsub(" ", "",D$actp, fixed = TRUE)
D$ddate1num = as.numeric(as.POSIXct(D$ddate1,format="%d/%m/%Y"))  #add numeric time value to ease calculation
# Now read 'selectdaysfile' which is the file that helps to link accelerometer data to dates and to log files
S = read.csv(selectdaysfile,stringsAsFactors=FALSE) # load availble knowledge about when monitor was worn (selectdaysfile)
S$day1num = as.numeric(as.POSIXct(S$Day1,format="%d/%m/%Y")) #add numeric time value to ease calculation
S$day2num = as.numeric(as.POSIXct(S$Day2,format="%d/%m/%Y")) #add numeric time value to ease calculation
# merge the information
D3 = merge(D,S,by="Monitor") # Merge selectdaysfile and logs, based on matching monitor ID
# Aggregate to only look at unique combinations of Monitor number, and days
K = aggregate(D3,by=list(D3$Monitor,D3$ddate1,D3$ddate1num,D3$day1num,D3$day2num),FUN="mean")
# the dates of accelerometer data reflect first day of measurement which may not match date in sleep log
# so, calculate distances between dates and use this to find matching data
# Calculate distance between dates on log and dates on which accelerometer was supposed to be worn
K$distance = pmin(abs(K$ddate1num - K$day1num),abs(K$ddate1num - K$day2num)) 
# Filter by closest distances (if monitor used multiple measurements, then closest date is used)
K2 = do.call(rbind,lapply(split(K,K$Monitor,K$ddate1num),
                          function(chunk) chunk[which.min(chunk$distance),]))
#K2 is now an overview of Monitor ids and dates from logs for which there is a match with selectfile
#tidy up original log data by taking subset of the data for which we are supposed to have accelerometer data based on K2
D4 = D[which(D$Monitor %in% K2$Monitor ==TRUE & D$ddate1num %in% K2$ddate1num == TRUE),]
# Now look at available accelerometer files and investigate which files match information in D4
fnames = dir(pathraw)
labels = c()
for (i in 1:length(fnames)) { #
  load(paste0(pathraw,"/",fnames[i]))
  sn = as.character(I$header[which(rownames(I$header) == "Device_Unique_Serial_Code"),1])
  # print(starttime)
  D5 = D4[which(D4$Monitor == as.numeric(sn)),] #subset matching serial numbers
  t0 = as.numeric(starttime)
  D5$distance = abs(D5$ddate1num - t0) #distance between date of diary and starttime of measurement
  day  = as.numeric(unlist(strsplit(unlist(strsplit(fnames[i],"day"))[2],"[.]RDa"))[1])
  w2w = which(D5$distance < (2*7*1440*60) & D5$tudday == day)# within two weeks window
  if (length(w2w) > 0) {
    # print(fnames[i])
    D5 = D5[w2w,] # only measurements withint 2 week window
    D5$filename = fnames[i]
    labels = rbind(labels,D5)
  } else {
    print(paste0("matching log not found for: day",day," file ",fnames[i]," Monitor ",sn))
  }
  rm(D5)
}
#look up for which files there was valid accelerometer data
ws = read.csv("D:/dropbox/Dropbox/Accelerometry/GGIR/development/output_Millenium dress rehearsal/results/part2_windowsummary.csv")
#only keep files for which there is a matching date (in windowsummary) and filename
strippedfilenames = unlist(lapply(as.character(labels$filename),function(x) unlist(strsplit(x,"_day"))[1]))
index = which(strippedfilenames %in% ws$filename) #labels$ddate1 %in% ws$time == TRUE & 
write.csv(labels[index,],"D:/dropbox/Dropbox/Accelerometry/GGIR/development/input_cls/data_annotations.csv",row.names=FALSE)

rm(list=ls())
graphics.off()
require(GENEAread)
require(mmap)
library(tools)

# specify working directory
setwd("D:/dropbox/Dropbox/Accelerometry/GGIR/development")

#==================================================================
# INPUT NEEDED:

# specify file number to start and end with, fill in c() if unknown
f0 = c() #file to start with if used in serial analyses
f1 = Inf #c() #file to end with if used in serial analyses (modify accordingly, if infinite then it will process until last file)
mode= c(1,2)    #What part of the analysis needs to be done (options: 1,2,3,4 and 5)
#datadir=  c() #Where is the raw accelerometer data? (leave as c() if you work with milestone data and mode > 1

datadir = "D:/sharedfolder/first5/output_first5/meta/raw" #"D:/dropbox/Dropbox/Accelerometry/GGIR/development/output_Millenium dress rehearsal/meta/raw"

outputdir= "D:/sharedfolder/first5" #Name directory where output needs to be stored
studyname="first5"  #name of study, only needed if datadir is a list of filenames
selectdaysfile = "D:/sharedfolder/first5/wearcodes.csv"

#=====================================================================================
# load functions from functions folder (replace by require(GGIR) once package is updated)

library(GGIR)
# ffnames = dir("functions") # creating list of filenames of scriptfiles to load
# for (i in 1:length(ffnames)) {
#   source(paste("functions/",ffnames[i],sep="")) #loading scripts for reading geneactiv data
# }
# ffnames5 = dir("functions_part5") # creating list of filenames of scriptfiles to load
# for (i in 1:length(ffnames5)) {
#   source(paste("functions_part5/",ffnames5[i],sep="")) #loading scripts for reading geneactiv data
# }
# ffnames = dir("functions_cls") # creating list of filenames of scriptfiles to load
# for (i in 1:length(ffnames)) {
#   source(paste("functions_cls/",ffnames[i],sep="")) #loading scripts for reading geneactiv data
# }
stt <- Sys.time()
print(paste0("Starting at: ", stt))

#=====================================================================================
g.shell.GGIR(#=======================================
  # INPUT NEEDED:
  #-------------------------------
  # General parameters
  #-------------------------------
  mode=mode, #specify above
  datadir=datadir, #specify above
  outputdir=outputdir, #specify above
  studyname=studyname, #specify above
  f0=f0, #specify above
  f1=f1, #specify above
  overwrite = TRUE, #overwrite previous milestone data?
  csv.struc=c(), #csv.struc=c(5,6), #
  do.imp=TRUE, # Do imputation? (recommended)
  idloc=1, #id location (1 = file header, 2 = filename)
  print.filename=FALSE,
  storefolderstructure = FALSE,
  selectdaysfile=selectdaysfile,
  # diaryfile=diaryfile,
  #-------------------------------
  # Part 1 parameters:
  #-------------------------------
  # Key functions: reading file, auto-calibration, and extracting features
  windowsizes = c(5,900,3600), #Epoch length, non-wear detection resolution, non-wear detection evaluation window
  do.cal=TRUE, # Apply autocalibration? (recommended)
  do.enmo = TRUE, #Needed for physical activity analysis
  do.anglez=TRUE, #Needed for sleep detection
  do.angley=TRUE, #Needed for sleep detection
  do.anglex=TRUE, #Needed for sleep detection
  chunksize=0.8, #size of data chunks to be read (value = 1 is maximum)
  desiredtz = "Europe/London",
  printsummary=TRUE,
  minloadcrit=48,
  epochvalues2csv = FALSE,
  #-------------------------------
  # Part 2 parameters:
  #-------------------------------
  # Key functions: Non-wear detection, imputation, and basic descriptives
  strategy = 1, #Strategy (see tutorial for explanation)
  ndayswindow=7, #only relevant when strategy = 3
  hrs.del.start = 0, # Only relevant when strategy = 2. How many HOURS need to be ignored at the START of the measurement?
  hrs.del.end = 0, # Only relevant when strategy = 2. How many HOURS need to be ignored at the END of the measurement?
  maxdur = 3, # How many DAYS of measurement do you maximumally expect?
  includedaycrit = 16, # number of minimum valid hours in a day to attempt physical activity analysis
  L5M5window = c(0,24), #window over which to calculate L5 and M5
  M5L5res = 10, #resolution in minutes of M5 and L5 calculation
  winhr = 5, # size of M5 and L5 (5 hours by default)
  qlevels = c(), #c(c(1380/1440),c(1410/1440)), #quantiles to calculate, set value at c() if you do not want quantiles
  qwindow=c(0,24), #window over which to calculate quantiles
  ilevels = c(),#c(seq(0,400,by=50),8000), #acceleration values (metric ENMO) from which a frequency distribution needs to be derived, set value at c() if you do not want quantiles
  mvpathreshold = c(100), #MVPA (moderate and vigorous physical activity threshold
  window.summary.size = 10,
  dayborder = 4, # dayborder is the hour at which one day becomes the next day
  mvpa.2014 = TRUE,
  closedbout=FALSE,
  #-----------------------------------
  # Report generation
  #-------------------------------
  # Key functions: Generating reports based on meta-data
  do.report=c(2), #for what parts does and report need to be generated?)
  visualreport = FALSE) 

fnsh <- Sys.time()

print(paste0("DONE. At: ", fnsh))
print(paste0("Took ", difftime(fnsh, stt, units = "mins"), " minutes"))
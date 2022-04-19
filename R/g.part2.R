g.part2 = function(datadir = c(), metadatadir = c(), f0 = c(), f1 = c(),
                   myfun=c(), params_cleaning = c(), params_247 = c(),
                   params_phyact = c(), params_output = c(), params_general = c(), ...) {
  
  #----------------------------------------------------------
  # Extract and check parameters
  input = list(...)
  params = extract_params(params_cleaning = params_cleaning,
                          params_247 = params_247,
                          params_phyact = params_phyact,
                          params_output = params_output,
                          params_general = params_general, input = input,
                          params2check = c("cleaning", "247", "phyact", "output", "general")) # load default parameters
  params_cleaning = params$params_cleaning
  params_247 = params$params_247
  params_phyact = params$params_phyact
  params_output = params$params_output
  params_general = params$params_general

  #-----------------------------
  if (is.numeric(params_247[["qwindow"]])) {
    params_247[["qwindow"]] = params_247[["qwindow"]][order(params_247[["qwindow"]])]
  } else if (is.character(params_247[["qwindow"]])) { 
    params_247[["qwindow"]] = g.conv.actlog(params_247[["qwindow"]], params_247[["qwindow_dateformat"]])
    # This will be an object with numeric qwindow values for all individuals and days
  }  
  #---------------------------------
  # Specifying directories with meta-data and extracting filenames
  path = paste0(metadatadir,"/meta/basic/")  #values stored per long epoch, e.g. 15 minutes
  fnames = dir(path)
  if (f1 > length(fnames)) f1 = length(fnames)
  # create output folders
  ffdone = c()
  ms2.out = "/meta/ms2.out"
  if (file.exists(paste0(metadatadir,ms2.out)) == FALSE) {
    dir.create(file.path(metadatadir,ms2.out))
  }
  csvfolder = "/meta/csv"
  if (params_output[["epochvalues2csv"]] == TRUE) {
    if (file.exists(paste0(metadatadir, csvfolder)) == FALSE) {
      dir.create(file.path(metadatadir,csvfolder))
    }
  }
  ffdone = dir(paste0(metadatadir,ms2.out))
  #---------------------------------
  # house keeping variables
  pdfpagecount = 1 # counter to keep track of files being processed (for pdf)
  pdffilenumb = 1 #counter to keep track of number of pdf-s being generated
  daySUMMARY = c()
  if (length(f0) ==  0) f0 = 1
  if (length(f1) ==  0) f1 = length(fnames)
  #--------------------------------
  # get full file path and folder name if requested by end-user and keep this for storage in output
  foldername = fullfilenames = folderstructure = referencefnames = c() 
  if (params_output[["storefolderstructure"]] == TRUE) {
    extractfilenames = function(x) {
      x2 = as.character(unlist(strsplit(x,".RDa"))[1])
      x3 = as.character(unlist(strsplit(x2,"eta_"))[2])
      return(x3)
    }
    referencefnames = sapply(fnames,extractfilenames)
    folderstructure = getfolderstructure(datadir,referencefnames)
    fullfilenames = folderstructure$fullfilenames
    foldername = folderstructure$foldername
  }
  #--------------------------------
  # Loop through all the files
  fnames = sort(fnames)
  if (f1 > length(fnames)) f1 = length(fnames)
  if (f0 > f1) f0 = 1
  
  #---------------------------------------
  cnt78 = 1
  
  #=========================================================
  # Declare core functionality, which at the end of this g.part2 is either
  # applied to the file in parallel with foreach or serially with a loop
  main_part2 = function(i, ffdone, fnames, 
                        metadatadir = c(), 
                        myfun=c(), params_cleaning = c(), params_247 = c(),
                        params_phyact = c(), params_output = c(), params_general = c(),
                        path, ms2.out, foldername, fullfilenames, folderstructure, referencefnames,
                        daySUMMARY, pdffilenumb, pdfpagecount, csvfolder, cnt78) {
    if (length(ffdone) > 0) {
      if (length(which(ffdone == as.character(unlist(strsplit(fnames[i], "eta_"))[2]))) > 0) {
        skip = 1 #skip this file because it was analysed before")
      } else {
        skip = 0 #do not skip this file
      }
    } else {
      skip = 0
    }
    if (params_general[["overwrite"]] == TRUE) skip = 0
    if (skip == 0) {
      M = c()
      filename_dir = c()
      filefoldername = c()
      file2read = paste0(path,fnames[i])
      load(file2read) #reading RData-file
      if (M$filecorrupt == FALSE & M$filetooshort == FALSE) {
        #-----------------------
        # If required by user, ignore specific timewindows for imputation and set them to zeroinstead:
        TimeSegments2Zero = c() # set defaul
        # Check whether csv file exists with start-end end times of timewindows to be ignored (where 0 movement will be assumed)
        if (length(params_cleaning[["TimeSegments2ZeroFile"]]) > 0) {
          TimeSegments2ZeroAll = read.csv(params_cleaning[["TimeSegments2ZeroFile"]])
          # Check whether this individual is part of the file
          filei = which(TimeSegments2ZeroAll$filename == as.character(unlist(strsplit(fnames[i], "eta_"))[2]))
          if (length(filei) > 0) {
            # If yes, load the timestamps that indicate the windows to be ignored
            TimeSegments2Zero = TimeSegments2ZeroAll[filei,]
            # Check that they fall withint the measurement
            TimeSegments2Zero$windowstart = as.POSIXlt(TimeSegments2Zero$windowstart,tz = params_general[["desiredtz"]])
            TimeSegments2Zero$windowend = as.POSIXlt(TimeSegments2Zero$windowend,tz = params_general[["desiredtz"]])
            timespan0 = iso8601chartime2POSIX(M$metashort$timestamp[1], tz = params_general[["desiredtz"]])
            timespan1 = iso8601chartime2POSIX(M$metashort$timestamp[nrow(M$metashort)],
                                              tz = params_general[["desiredtz"]])
            validtimes = which(TimeSegments2Zero$windowstart > timespan0 &
                                 TimeSegments2Zero$windowend < timespan1)
            if (length(validtimes) > 0) {
              TimeSegments2Zero = TimeSegments2Zero[validtimes,c("windowstart","windowend")]
              
            } else {
              TimeSegments2Zero = c()
            }
          }
        }
        #------------
        if (length(myfun) > 0) {
          if (myfun$outputtype == "character") {
            # At the moment we do not have a strategy in place on how to impute categorical variables
            # produced by external functions. Therefore, for the moment ignore these variables until
            # there is a plan.
            M$metashort = M$metashort[,-which(names(M$metashort) %in% myfun$colnames == TRUE)]
          }
        }
        IMP = g.impute(M, I, 
                       params_cleaning = params_cleaning,
                       dayborder = params_general[["dayborder"]],
                       desiredtz = params_general[["desiredtz"]],
                       TimeSegments2Zero = TimeSegments2Zero)
        if (params_cleaning[["do.imp"]] == FALSE) { #for those interested in sensisitivity analysis
          IMP$metashort = M$metashort
          IMP$metalong = M$metalong
        }
        SUM = g.analyse(I, C, M, IMP,
                        params_247 = params_247,
                        params_phyact = params_phyact,
                        dayborder = params_general[["dayborder"]],
                        desiredtz = params_general[["desiredtz"]],
                        idloc = params_general[["idloc"]],
                        includedaycrit = params_cleaning[["includedaycrit"]],
                        selectdaysfile = params_cleaning[["selectdaysfile"]],
                        myfun = myfun)
        name = as.character(unlist(strsplit(fnames[i], "eta_"))[2])
        if (params_output[["epochvalues2csv"]] == TRUE) {
          if (length(IMP$metashort) > 0) {
            write.csv(IMP$metashort, paste0(metadatadir, "/", csvfolder, "/", name, ".csv"), row.names = FALSE)
          }
        }
        if (M$filecorrupt == FALSE & M$filetooshort == FALSE) {
          if (cnt78 == 1) {
            SUMMARY = SUM$summary
            daySUMMARY = SUM$daysummary
            if (length(params_cleaning[["selectdaysfile"]]) > 0) {
              winSUMMARY = SUM$windowsummary[,which(
                is.na(colnames(SUM$windowsummary)) == FALSE)] # added for Millenium cohort
            }
            cnt78 = 2
          } else {
            if (length(params_cleaning[["selectdaysfile"]]) > 0) {
              # windowsummary
              winSUMMARY2 = SUM$windowsummary[,which(is.na(colnames(SUM$windowsummary)) == FALSE)]
              if (ncol(winSUMMARY) != ncol(winSUMMARY2)) {
                winSUMMARY2 = cbind(winSUMMARY2,matrix(" ",1,(ncol(winSUMMARY) - ncol(winSUMMARY2))))
                colnames(winSUMMARY2) = colnames(winSUMMARY)
              }
              if (length(which(colnames(winSUMMARY) != names(winSUMMARY2)) ) > 0) {
                names(winSUMMARY2) =   colnames(winSUMMARY)
              }
              if (length(which(colnames(daySUMMARY) != names(SUM$daysummary)) ) > 0) {
                names(SUM$windowsummary) =   colnames(winSUMMARY2)
              }
            }
          }
        }
        if (length(unlist(strsplit(name,"[.]RD"))) == 1) { # to avoid getting .RData.RData
          filename = paste0(name,".RData")
        }
        if (params_output[["storefolderstructure"]] == TRUE) { # newly added 20-2-2019
          SUM$daysummary$filename_dir = fullfilenames[i] #full filename structure
          SUM$daysummary$foldername = foldername[i] #store the lowest foldername
        }
        
        save(SUM,IMP,file = paste0(metadatadir, ms2.out, "/", name)) #IMP is needed for g.plot in g.report.part2
      }
      if (M$filecorrupt == FALSE & M$filetooshort == FALSE) rm(IMP)
      rm(M); rm(I)
    }
  } # end of main_part2
  
  #--------------------------------------------------------------------------------
  # Run the code either parallel or in serial (file index starting with f0 and ending with f1)
  if (params_general[["do.parallel"]] == TRUE) {
    cores = parallel::detectCores()
    Ncores = cores[1]
    if (Ncores > 3) {
      if (length(params_general[["maxNcores"]]) == 0) params_general[["maxNcores"]] = Ncores
      Ncores2use = min(c(Ncores - 1, params_general[["maxNcores"]], (f1 - f0) + 1))
      cl <- parallel::makeCluster(Ncores2use) #not to overload your computer
      doParallel::registerDoParallel(cl)
    } else {
      cat(paste0("\nparallel processing not possible because number of available cores (",Ncores,") < 4"))
      params_general[["do.parallel"]] = FALSE
    }
    cat(paste0('\n Busy processing ... see ', metadatadir, ms2.out, ' for progress\n'))
    
    # check whether we are indevelopment mode:
    GGIRinstalled = is.element('GGIR', installed.packages()[,1])
    packages2passon = functions2passon = NULL
    GGIRloaded = "GGIR" %in% .packages()
    if (GGIRloaded) { #pass on package
      packages2passon = 'GGIR'
      errhand = 'pass'
    } else {
      # pass on functions
      functions2passon = c("g.analyse", "g.impute", "g.weardec", "g.detecmidnight",
                           "g.extractheadervars", "g.analyse.avday", "g.getM5L5", "g.IVIS",
                           "g.analyse.perday", "g.getbout", "g.analyse.perfile", "g.intensitygradient",
                           "iso8601chartime2POSIX", "extract_params", "load_params", "check_params")
      errhand = 'stop'
    }
    fe_dopar = foreach::`%dopar%`
    fe_do = foreach::`%do%`
    i = 0 # declare i because foreach uses it, without declaring it
    `%myinfix%` = ifelse(params_general[["do.parallel"]], fe_dopar, fe_do) # thanks to https://stackoverflow.com/questions/43733271/how-to-switch-programmatically-between-do-and-dopar-in-foreach
    output_list = foreach::foreach(i = f0:f1, .packages = packages2passon,
                                  .export = functions2passon, .errorhandling = errhand, .verbose = F) %myinfix% {
                                    tryCatchResult = tryCatch({
                                      main_part2(i, ffdone, fnames, metadatadir,
                                                 myfun, params_cleaning, params_247,
                                                 params_phyact, params_output, params_general,
                                                 path, ms2.out, foldername, fullfilenames,
                                                 folderstructure, referencefnames,
                                                 daySUMMARY, pdffilenumb, pdfpagecount, csvfolder, cnt78)
                                      
                                    })
                                    return(tryCatchResult)
                                  }
    on.exit(parallel::stopCluster(cl))
    for (oli in 1:length(output_list)) { # logged error and warning messages
      if (is.null(unlist(output_list[oli])) == FALSE) {
        cat(paste0("\nErrors and warnings for ",fnames[oli]))
        print(unlist(output_list[oli])) # print any error and warnings observed
      }
    }
  } else { 
    for (i in f0:f1) {
      cat(paste0(i, " "))
      main_part2(i, ffdone, fnames, metadatadir, 
                 myfun, params_cleaning, params_247,
                 params_phyact, params_output, params_general,
                 path, ms2.out, foldername, fullfilenames, 
                 folderstructure, referencefnames,
                 daySUMMARY, pdffilenumb, pdfpagecount, csvfolder, cnt78)
    }
  }
}

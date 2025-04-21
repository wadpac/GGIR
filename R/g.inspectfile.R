g.inspectfile = function(datafile, desiredtz = "", params_rawdata = c(),
                         configtz = c(), ...) {
  #get input variables
  input = list(...)
  if (length(input) > 0 || length(params_rawdata) == 0) {
    # or if params_[...] aren't specified (so need to be filled with defaults).
    # So, inside GGIR this will not be used, but it is used when g.inspectfile is used on its own
    # as if it was still the old g.inspectfile function
    params = extract_params(params_rawdata = params_rawdata,
                            input = input,
                            params2check = "rawdata") # load default parameters
    params_rawdata = params$params_rawdata
    rm(params)
  }
  
  # note that if the file is an RData file then this function will not be called
  # the output of this function for the original datafile is stored inside the RData file in the form of object I
  getbrand = function(filename = c(), datafile = c()) {
    sf = c(); isitageneactive = c(); mon = c(); dformat = c() #generating empty variables
    extension = unlist(strsplit(filename,"[.]"))
    if (tolower(extension[length(extension)]) == "gz") {
      extension = extension[length(extension) - 1]
    } else {
      extension = extension[length(extension)]
    }
    switch(extension,
           "bin" = ,
           "BIN" = { dformat = FORMAT$BIN },
           "cwa" = ,
           "CWA" = { mon = MONITOR$AXIVITY
           dformat = FORMAT$CWA
           },
           "gt3x" = { mon = MONITOR$ACTIGRAPH
           dformat = FORMAT$GT3X
           },
           "GT3X" = { mon = MONITOR$ACTIGRAPH
           dformat = FORMAT$GT3X
           if (file.access(datafile, 2) == 0) { # test for write access to file
             # rename file to be lower case gt3x extension
             file.rename(from = datafile, to = gsub(pattern = ".GT3X", replacement = ".gt3x", x = datafile))
             datafile = gsub(pattern = ".GT3X", replacement = ".gt3x", x = datafile)
             warning("\nWe have renamed the GT3X file to gt3x because GGIR dependency read.gt3x cannot handle uper case extension")
           } else {
             stop("\nGGIR needs to change the file extension from GT3X to gt3x, but it does not seem to have write permission to the file.")
           }
           },
           "csv" = { dformat = FORMAT$CSV
           testheader = read.csv(datafile, nrow = 1, skip = 0, header = FALSE)
           
           if (grepl("ActiGraph", testheader[1], fixed=TRUE)) {
             mon = MONITOR$ACTIGRAPH
           } else {
             testcsv = read.csv(datafile, nrow = 10, skip = 10)
             testcsvtopline = read.csv(datafile, nrow = 2,skip = 1)
             if (ncol(testcsv) == 2 && ncol(testcsvtopline) < 4) {
               mon = MONITOR$GENEACTIV
             } else if (ncol(testcsv) >= 4 && ncol(testcsvtopline) >= 4) {
               mon = MONITOR$AXIVITY
             } else {
               stop(paste0("\nError processing ", filename, ": unrecognised csv file format.\n"))
             }
           }
           },
           "wav" = { stop(paste0("\nError processing ", filename, ": Axivity .wav file format is no longer supported.\n")) },
           { stop(paste0("\nError processing ", filename, ": unrecognised file format.\n")) }
    )
    
    if (ismovisens(datafile)) {
      dformat = FORMAT$BIN
      mon = MONITOR$MOVISENS
      sf = 64
      header = "no header"
    } else if (dformat == FORMAT$BIN) { # .bin and not movisens, could be GENEActiv or Parmay Matrix
      mon = inspect_binFile_brand(filename = datafile)
      if (mon == MONITOR$PARMAY_MTX && utils::packageVersion("GGIRread") < "1.0.4") {
        stop("Please update R package GGIRread to version 1.0.4 or higher", call. = FALSE)
      }
      # try read the file as if it is a geneactiv and store output in variable 'isitageneactive'
      if (mon == MONITOR$GENEACTIV) {
        if (all(names(isitageneactive) %in% c("header", "data.out") == TRUE)) {
          isitageneactive = GGIRread::readGENEActiv(filename = datafile, start = 0, end = 1)
          tmp = unlist(strsplit(unlist(as.character(isitageneactive$header$SampleRate))," "))[1]
          # occasionally we'll get a decimal seperated by comma; if so, replace the comma with a dot
          tmp = sub(",", ".", tmp, fixed = TRUE)
          sf = as.numeric(tmp)
          
          #also try to read sf from first page header
          sf_r = sf
          csvr = c()
          suppressWarnings(expr = {
            try(expr = {csvr = as.matrix(read.csv(datafile, nrow = 10,
                                                  skip = 200, sep = ""))
            }, silent = TRUE)
          })
          if (length(csvr) > 1) {
            for (ii in 1:nrow(csvr)) {
              tmp3 = unlist(strsplit(as.character(csvr[ii,1]),"quency:")) #part of 'frequency'
              if (length(tmp3) > 1) {
                # occasionally we'll get a decimal seperated by comma; if so, replace the comma with a dot
                tmp3 = sub(",", ".", tmp3, fixed = TRUE)
                sf_r = as.numeric(tmp3)
              }
            }
            if (length(sf_r) > 0 && !is.na(sf_r)) {
              if (sf_r != sf && abs(sf_r - sf) > 5) { # use pageheader sample frequency if it is not the same as header sample frequency
                sf = sf_r
                warning(paste0("sample frequency used from page header: ", sf, " Hz"))
              }
            }
          }
        } else {
          stop(paste0("\nError processing ", filename, ": possibibly a corrupt GENEActive file"))
        }
      } else if (mon == MONITOR$PARMAY_MTX) {
        header = NULL
        sf = GGIRread::readParmayMatrix(datafile, output = "sf")
      } else {
        stop(paste0("\nError processing ", filename, ": unrecognised .bin file"))
      }
    } else if (dformat == FORMAT$CSV) { #no checks for corrupt file yet...maybe not needed for csv-format?
      if (mon == MONITOR$GENEACTIV) {
        tmp = read.csv(datafile, nrow = 50, skip = 0)
        tmp = as.character(tmp[which(as.character(tmp[,1]) == "Measurement Frequency"),2])
        tmp = as.numeric(unlist(strsplit(tmp," "))[1])
        # occasionally we'll get a decimal seperated by comma; if so, replace the comma with a dot
        tmp = sub(",", ".", tmp, fixed = TRUE)
        sf = as.numeric(tmp)
        
      } else if (mon == MONITOR$ACTIGRAPH) {
        tmp = read.csv(datafile, nrow = 9, skip = 0)
        tmp = colnames(tmp)
        tmp = as.character(unlist(strsplit(tmp,".Hz"))[1])
        # tmp3 = as.character(unlist(strsplit(tmp2,"yy.at."))[2])
        # following suggestion by XInyue on github https://github.com/wadpac/GGIR/issues/102 replaced by:
        tmp = as.character(unlist(strsplit(tmp, ".at.",fixed = T))[2])
        # occasionally we'll get a decimal seperated by comma; if so, replace the comma with a dot
        tmp = sub(",", ".", tmp, fixed = TRUE)
        sf = as.numeric(tmp)
      } else if (mon == MONITOR$AXIVITY) {
        # sample frequency is not stored
        tmp = read.csv(datafile, nrow = 100000, skip = 0)
        tmp = as.numeric(as.POSIXct(tmp[, 1], origin = "1970-01-01"))
        sf = length(tmp) / (tmp[length(tmp)] - tmp[1])
        sf = floor((sf) / 5 ) * 5 # round down to nearest integer of 5, we never want to assume that there is more frequency content in a signal than there truly is
      }
    } else if (dformat == FORMAT$CWA) {
      PP = GGIRread::readAxivity(datafile, start = 1, end = 10, desiredtz = desiredtz)
      H = PP$header
      sf = H$frequency
    } else if (dformat == FORMAT$GT3X) {
      info = try(expr = {read.gt3x::parse_gt3x_info(datafile, tz = desiredtz)},silent = TRUE)
      if (inherits(info, "try-error") == TRUE || is.null(info)) {
        warning(paste0("\nFile info could not be extracted from ", datafile), call. = FALSE)
        sf = NULL # set to NULL in order to tell other GGIR functions that file was corrupt
      } else {
        info = info[lengths(info) != 0] # remove odd NULL in the list
        sf = info[["Sample Rate"]]
      }
    }
    invisible(list(dformat = dformat, mon = mon, sf = sf, datafile = datafile))
  }
  #======================================================================
  # main script
  filename = unlist(strsplit(as.character(datafile),"/"))
  filename = filename[length(filename)]
  monnames = c("genea", "geneactive", "actigraph", "axivity", "movisens", "verisense", "parmay_mtx") #monitor names
  fornames = c("bin", "csv", "wav", "cwa", "csv", "gt3x", "BIN") #format names
  
  if (length(filename) == 0) {
    warning("no files to analyse", call. = FALSE)
  }
  
  if (length(params_rawdata[["rmc.firstrow.acc"]]) == 0) {
    INFI = getbrand(filename, datafile)
    mon = INFI$mon
    dformat = INFI$dformat
    sf = INFI$sf
    datafile = INFI$datafile
  } else {
    dformat = FORMAT$AD_HOC_CSV
    mon = MONITOR$AD_HOC
    Pusercsvformat = read.myacc.csv(rmc.file = datafile,
                                    rmc.nrow = 5,
                                    rmc.dec = params_rawdata[["rmc.dec"]],
                                    rmc.firstrow.acc = params_rawdata[["rmc.firstrow.acc"]],
                                    rmc.firstrow.header = params_rawdata[["rmc.firstrow.header"]],
                                    rmc.header.length = params_rawdata[["rmc.header.length"]],
                                    rmc.col.acc = params_rawdata[["rmc.col.acc"]],
                                    rmc.col.temp = params_rawdata[["rmc.col.temp"]],
                                    rmc.col.time = params_rawdata[["rmc.col.time"]],
                                    rmc.unit.acc = params_rawdata[["rmc.unit.acc"]],
                                    rmc.unit.temp = params_rawdata[["rmc.unit.temp"]],
                                    rmc.unit.time = params_rawdata[["rmc.unit.time"]],
                                    rmc.format.time = params_rawdata[["rmc.format.time"]],
                                    rmc.bitrate = params_rawdata[["rmc.bitrate"]],
                                    rmc.dynamic_range = params_rawdata[["rmc.dynamic_range"]],
                                    rmc.unsignedbit = params_rawdata[["rmc.unsignedbit"]],
                                    rmc.origin = params_rawdata[["rmc.origin"]],
                                    rmc.desiredtz = params_rawdata[["rmc.desiredtz"]],
                                    rmc.configtz = params_rawdata[["rmc.configtz"]],
                                    rmc.sf = params_rawdata[["rmc.sf"]],
                                    rmc.headername.sf = params_rawdata[["rmc.headername.sf"]],
                                    rmc.headername.sn = params_rawdata[["rmc.headername.sn"]],
                                    rmc.headername.recordingid = params_rawdata[["rmc.headername.sn"]],
                                    rmc.header.structure = params_rawdata[["rmc.header.structure"]],
                                    rmc.check4timegaps = params_rawdata[["rmc.check4timegaps"]],
                                    rmc.scalefactor.acc = params_rawdata[["rmc.scalefactor.acc"]],
                                    desiredtz = desiredtz,
                                    configtz = configtz)
    if (inherits(Pusercsvformat$header, "character") && Pusercsvformat$header == "no header") {
      sf = params_rawdata[["rmc.sf"]]
    } else {
      sf = as.numeric(Pusercsvformat$header["sample_rate",1])
    }
    if (is.null(sf) || is.na(sf)) {
      stop("\nFile header doesn't specify sample rate. Please provide rmc.sf value to process ", datafile)
    } else if (sf == 0) {
      stop("\nFile header doesn't specify sample rate. Please provide a non-zero rmc.sf value to process ", datafile)
    }
  }
  
  if (mon == MONITOR$GENEACTIV && dformat == FORMAT$CSV) {
    stop(paste0("The GENEActiv csv reading functionality is deprecated in",
                " GGIR from version 2.6-4 onwards. Please, use either",
                " the GENEActiv bin files or try to read the csv files with",
                " GGIR::read.myacc.csv"), call. = FALSE)
  }
  
  if (dformat == FORMAT$BIN) {
    if (mon == MONITOR$GENEACTIV) {
      H = GGIRread::readGENEActiv(filename = datafile, start = 0, end = 1)$header
    } else if (mon == MONITOR$MOVISENS) {
      H = "file does not have header" # these files have no header
      xmlfile = paste0(dirname(datafile), "/unisens.xml")
      if (file.exists(xmlfile)) {
        # as we are only interested in two character fields
        # try extract value by reading xml as text file to avoid having to add
        # software dependencies
        header = as.character(read.csv(xmlfile, nrow = 1))
        tmp1 = unlist(strsplit(header, "measurementId="))[2]
        ID = gsub(pattern = " ",replacement = "",  unlist(strsplit(tmp1, " timestampStart"))[1])
        
        header = paste0(read.csv(xmlfile, nrow = 10, skip = 2), collapse = " ")
        tmp1 = unlist(strsplit(header, "sensorSerialNumber value="))[2]
        SN = unlist(strsplit(tmp1, "/>"))[1]
        header = data.frame(serialnumber = SN, ID = ID)
        if (length(header) > 1) {
          H = t(header)
        }
        filename = unlist(strsplit(as.character(datafile),"/"))
        filename = filename[length(filename) - 1]
      }
    } else if (mon == MONITOR$PARMAY_MTX) {
      H = "file does not have header" # these files have no header
    }
  } else if (dformat == FORMAT$CSV) {
    if (mon == MONITOR$ACTIGRAPH) {
      H = read.csv(datafile, nrow = 9, skip = 0)
    } else if (mon == MONITOR$AXIVITY) {
      H = "file does not have header" # these files have no header
    }
  } else if (dformat == FORMAT$CWA) {
    PP = GGIRread::readAxivity(datafile, start = 1, end = 10, desiredtz = desiredtz)
    H = PP$header
    
  } else if (dformat == FORMAT$AD_HOC_CSV) { # csv data in a user-specified format
    header = Pusercsvformat$header
  } else if (dformat == FORMAT$GT3X) { # gt3x
    info = try(expr = {read.gt3x::parse_gt3x_info(datafile, tz = desiredtz)},silent = TRUE)
    if (inherits(info, "try-error") == TRUE || is.null(info)) {
      warning(paste0("\nFile info could not be extracted from ", datafile), call. = FALSE)
      sf = NULL # set to NULL in order to tell other GGIR functions that file was corrupt
      H = NULL
      header = NULL
    } else {
      info = info[lengths(info) != 0] # remove odd NULL in the list
      
      H = matrix("", length(info), 2)
      H[, 1] = names(info)
      for (ci in 1:length(info)) {
        if (inherits(info[[ci]], "POSIXct") == TRUE) {
          H[ci, 2] = format(info[[ci]])
        } else {
          H[ci, 2] = as.character(info[[ci]])
        }
      }
      sf = as.numeric(H[which(H[,1] == "Sample Rate"), 2])
    }
  }
  if (is.null(sf) || sf == 0) {
    warning(paste0("\nSample frequency not recognised in ", basename(datafile)), call. = FALSE)
  }
  
  if (dformat != FORMAT$AD_HOC_CSV && is.null(sf) == FALSE) {
    H = as.matrix(H)
    if (ncol(H) == 3 && dformat == FORMAT$CSV && mon == MONITOR$ACTIGRAPH) {
      if (length(which(is.na(H[,2]) == FALSE)) == 0) {
        H = as.matrix(H[,1])
      }
    }
    if (ncol(H) == 1 && dformat == FORMAT$CSV) {
      if (mon == MONITOR$ACTIGRAPH) {
        vnames = c("Number:","t Time","t Date",":ss)","d Time","d Date","Address:","Voltage:","Mode =")
        Hvalues = Hnames = rep(" ",length(H))
        firstline = colnames(H)
        for (run in 1:length(H)) {
          for (runb in 1:length(vnames)) {
            tmp = unlist(strsplit(H[run],vnames[runb]))
            if (length(tmp) > 1) {
              Hnames[run] = paste(tmp[1], vnames[runb], sep = "")
              Hvalues[run] = paste(tmp[2], sep = "")
            }
          }
        }
        H = cbind(Hnames,Hvalues)
        H = rbind(c("First line",firstline),H)
      } else {
        H = cbind(c(1:length(H)),H)
      }
    }
    if (dformat == FORMAT$CWA) {
      header = data.frame(value = H, row.names = rownames(H), stringsAsFactors = TRUE)
    } else {
      if ((mon == MONITOR$GENEACTIV && dformat == FORMAT$BIN) || (mon == MONITOR$MOVISENS && length(H) > 0)) {
        varname = rownames(as.matrix(H))
        H = data.frame(varname = varname,varvalue = as.character(H), stringsAsFactors = TRUE)
      } else if (dformat != FORMAT$AD_HOC_CSV) {
        if (length(H) > 1 && class(H)[1] == "matrix") H = data.frame(varname = H[,1],varvalue = H[,2], stringsAsFactors = TRUE)
      }
    }
    if (dformat != FORMAT$CWA && length(H) > 1 && (class(H)[1] == "matrix" || class(H)[1] == "data.frame")) {
      RowsWithData = which(is.na(H[,1]) == FALSE)
      header = data.frame(value = H[RowsWithData, 2], row.names = H[RowsWithData, 1], stringsAsFactors = TRUE)
    }
    if (H[1,1] == "file does not have header") { #no header
      header = "no header"
    }
    if (mon == MONITOR$ACTIGRAPH && dformat != FORMAT$GT3X) {
      verisense_check = substr(colnames(read.csv(datafile,nrow = 1)[1]), start = 36, stop = 44)
      if (identical('Verisense', toString(verisense_check))) {
        mon = MONITOR$VERISENSE
      }
    }
  }
  if (!is.null(sf)) {
    # detect dot or comma separator in the data file
    op <- options(warn = -1) # turn off warnings
    on.exit(options(op))
    suppressWarnings(expr = {decn = g.dotorcomma(datafile, dformat, mon,
                                                 rmc.dec = params_rawdata[["rmc.dec"]])})
    options(warn = 0) # turn on warnings
  } else {
    decn = "."
  }
  monc = mon
  monn = ifelse(mon > 0, monnames[mon], "unknown")
  dformc = dformat
  dformn = fornames[dformat]
  invisible(list(header = header, monc = monc, monn = monn,
                 dformc = dformc, dformn = dformn, sf = sf, decn = decn, filename = filename))
}

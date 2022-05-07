g.inspectfile = function(datafile, desiredtz = "", params_rawdata = c(),
                         configtz = c(), ...) {
  #get input variables
  input = list(...)
  if (any(names(input) %in% c("datafile", "desiredtz", "params_rawdata", "configtz")) == FALSE) {
    # Extract and check parameters if user provides more arguments than just the parameter arguments
    # So, inside GGIR this will not be used, but it is used when g.inspectfile is used on its own
    # as if it was still the old g.inspectfile function
    params = extract_params(params_rawdata = params_rawdata,
                            input = input) # load default parameters
    params_rawdata = params$params_rawdata
  }
  
  #get input variables (relevant when read.myacc.csv is used
  input = list(...)
  if (length(input) > 0) {
    for (i in 1:length(names(input))) {
      txt = paste0(names(input)[i], "=", input[i])
      if (is(unlist(input[i]), "character")) {
        txt = paste0(names(input)[i], "='", unlist(input[i]), "'")
      }
      eval(parse(text = txt))
    }
  }
  # Although also documented in the package manual files, here 
  # for convenience the monitor codes (mon):
  # 0 - ad-hoc file (currently only .csv format)
  # 3 - Actigraph; 4 - Axivity (AX3, AX6)
  # 5 - Movisense; 6 - Verisense
  # data formats:
  # 1 - bin; 2 - csv; 3 - wav
  # 4 - cwa; 5 - ad-hoc .csv
  # 6 - gt3x
  
  
  # note that if the file is an RData file then this function will not be called
  # the output of this function for the original datafile is stored inside the RData file in the form of object I
  getbrand = function(filename = c(), datafile = c()) {
    sf = c(); isitageneactive = c();  isitagenea = c();  mon = c(); dformat = c() #generating empty variables
    tmp1 = unlist(strsplit(filename,"[.]cs"))
    tmp2 = unlist(strsplit(filename,"[.]b"))
    tmp3 = unlist(strsplit(filename,"[.]w"))
    tmp4 = unlist(strsplit(filename,"[.]r"))
    tmp5 = unlist(strsplit(filename,"[.]cw"))
    tmp6 = unlist(strsplit(filename,"[.]gt3"))
    tmp7 = unlist(strsplit(filename,"[.]GT3"))
    if (tmp1[length(tmp1)] == "v" | tmp1[length(tmp1)] == "v.gz") { #this is a csv file
      dformat = 2 #2 = csv
      testcsv = read.csv(datafile, nrow = 10, skip = 10)
      testcsvtopline = read.csv(datafile, nrow = 2,skip = 1)
      if (ncol(testcsv) == 2 & ncol(testcsvtopline) < 4) { #it is a geneactivefile
        mon = 2
      } else if (ncol(testcsv) >= 3 & ncol(testcsvtopline) < 4) {	#it is an actigraph file
        mon = 3
      } else if (ncol(testcsv) >= 4 & ncol(testcsvtopline) >= 4) { # it is an AX3 file
        mon = 4
      }
    } else if (tmp2[length(tmp2)] == "in") { #this is a bin file
      dformat = 1 #1 = binary
    } else if (tmp3[length(tmp3)] == "av") { #this is a wav file
      dformat = 3 #3 = wav
      mon = 4 # Axivity
    } else if (tmp5[length(tmp5)] == "a") { #this is a cwa file
      dformat = 4 #4 = cwa
      mon = 4 # Axivity
    } else if (tmp6[length(tmp6)] == "x") { #this is a gt3x file
      dformat = 6 #6 = gt3x
      mon = 3 # actigraph
    } else if (tmp7[length(tmp7)] == "X") { #this is a gt3x file from Centerpoint
      if (file.access(datafile, 2) == 0) { # test for write access to file
        # rename file to be lower case gt3x extension
        file.rename(from = datafile, to = gsub(pattern = ".GT3X", replacement = ".gt3x", x = datafile))
        datafile = gsub(pattern = ".GT3X", replacement = ".gt3x", x = datafile)
        warning("\nWe have renamed the GT3X file to gt3x because GGIR dependency read.gt3x cannot handle uper case extention")
        dformat = 6 #6 = gt3x
        mon = 3 # actigraph
      } else {
        stop("\nGGIR wants to change the file extension from GT3X to gt3x, but it does not seem to have write permission to the file.")
      }
    }
    is.mv = ismovisens(datafile)
    if (is.mv == TRUE) {
      dformat = 1
      sf = 64
      mon = 5
      header = "no header"
    }
    if (dformat == 1 & is.mv == FALSE) { # .bin and not movisens
      # try read the file as if it is a geneactiv and store output in variable 'isitageneactive'
      if ("GENEAread" %in% rownames(installed.packages()) == FALSE) {
        cat("\nWarning: R package GENEAread has not been installed, please install it before continuing")
      }
      suppressWarnings(try(expr = {isitageneactive = GENEAread::header.info(binfile = datafile, more = F)}, silent = TRUE))
      # try read the file as if it is a genea and store output in variable 'isitagenea'
      try(expr = {isitagenea = g.binread(datafile, 0, 1)} , silent = TRUE)
      #size and content of variables 'isitagenea' and 'isitageneactive' will now tell us what it is
      if (length(isitagenea) > 1) {
        mon = 1 #mon = 1 is code for saying that it is a genea
        H = isitagenea$header
        tmp = strsplit(H[which(H[,1] == "Sample_Rate"),2],"Hz")
        tmp2 = unlist(strsplit(as.character(tmp[1]),","))
        if (length(tmp2) > 1) { #decimals seperated by comma
          sf = as.numeric(tmp2[1])
          sf = sf + (as.numeric(tmp2[2])) / 10
        } else { #decimals seperated by dot
          sf = as.numeric(tmp[1])
        }
        if (sf == 0 | is.na(sf) == T) {
          skip = 1 #reconsider decision to analyse this file as it is possibly corrupt
          print("Possibibly corrupt genea File")
        }
      } else if (length(as.matrix(isitageneactive)) >= 1 & length(isitagenea) == 0) {
        ppp = unlist(isitageneactive)
        if (ppp[2] != "NA:NA") {
          mon = 2 #mon = 1 is code for saying that it is a geneactive
          H = isitageneactive
          tmp = unlist(strsplit(unlist(H[2,1])," "))
          tmp2 = unlist(strsplit(as.character(tmp[1]), ","))
          if (length(tmp2) > 1) { #decimals seperated by comma
            sf = as.numeric(tmp2[1])
            sf = sf + (as.numeric(tmp2[2]))/10
          } else { #decimals seperated by dot
            sf = as.numeric(tmp[1])
          }
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
                sf_r = tmp3[2]#a s.numeric(tmp3[2]) + as.numeric(csvr[ii,]/10) #sample frequency from the page header
              }
            }
            #check whether it is comma separated
            tmp4 = unlist(strsplit(as.character(sf_r),","))
            if (length(tmp4) > 1) { #comma
              sf_r = as.numeric(tmp4[1]) + as.numeric(tmp4[2]) / 10
            } else { #dot
              sf_r = as.numeric(sf_r)
            }
            if (length(sf_r) > 0) {
              if (is.na(sf_r) == FALSE) {
                if (sf_r != sf & abs(sf_r - sf) > 5) { #use pageheader sample frequency if it is not the same as header sample frequency
                  sf = sf_r
                  print(paste("sample frequency used from page header: ", sf, " Hz", sep = ""))
                }
              }
            }
          }
        } else {
          print("Possibibly corrupt geneactive File")
        }
      }
    } else if (dformat == 2) { #no checks for corrupt file yet...maybe not needed for csv-format?
      if (mon == 2) {
        tmp = read.csv(datafile, nrow = 50, skip = 0)
        sf = as.character(tmp[which(as.character(tmp[,1]) == "Measurement Frequency"),2])
        tmp = as.numeric(unlist(strsplit(sf," "))[1])
        tmp2 = unlist(strsplit(as.character(tmp[1]),","))
        if (length(tmp2) > 1) { #decimals seperated by comma
          sf = as.numeric(tmp2[1])
          sf = sf + (as.numeric(tmp2[2]))/10
        } else { #decimals seperated by dot
          sf = as.numeric(tmp[1])
        }
      } else if (mon == 3) {
        tmp0 = read.csv(datafile, nrow = 9, skip = 0)
        tmp = colnames(tmp0)
        tmp2 = as.character(unlist(strsplit(tmp,".Hz"))[1])
        # tmp3 = as.character(unlist(strsplit(tmp2,"yy.at."))[2])
        # following suggestion by XInyue on github https://github.com/wadpac/GGIR/issues/102 replaced by:
        tmp3 = as.character(unlist(strsplit(tmp2, ".at.",fixed = T))[2])
        tmp5 = unlist(strsplit(tmp3,","))
        if (length(tmp5) > 1) { #decimals seperated by comma
          sf = as.numeric(tmp5[1])
          sf = sf + (as.numeric(tmp5[2])) / 10
        } else { #decimals seperated by dot
          sf = as.numeric(tmp3[1])
        }
      } else if (mon == 4) {
        # sample frequency is not stored
        tmp0 = read.csv(datafile, nrow = 100000, skip = 0)
        tmp1 = as.numeric(as.POSIXlt(tmp0[, 1]))
        sf = length(tmp1) / (tmp1[length(tmp1)] - tmp1[1])
        sf = round((sf) / 5 ) * 5 # round to nearest integer of 5
      }
    } else if (dformat == 3) { # wav
      H = tuneR::readWave(datafile,from = 1, to = 10,units = c("seconds"), header = TRUE)
      sf = H$sample.rate
    } else if (dformat == 4) { # cwa
      PP = g.cwaread(datafile,start = 1, end = 10, desiredtz = desiredtz)
      H = PP$header
      sf = H$frequency
    } else if (dformat == 6) { # gt3
      info = try(expr = {read.gt3x::parse_gt3x_info(datafile, tz = desiredtz)},silent = TRUE)
      info = info[lengths(info) != 0] # remove odd NULL in the list
      sf = info[["Sample Rate"]]
    }
    invisible(list(dformat = dformat, mon = mon, sf = sf, datafile = datafile))
  }
  #======================================================================
  # main script
  filename = unlist(strsplit(as.character(datafile),"/"))
  filename = filename[length(filename)]
  monnames = c("genea", "geneactive", "actigraph", "axivity", "movisens", "verisense") #monitor names
  fornames = c("bin", "csv", "wav", "cwa", "csv", "gt3x") #format names
  
  if (length(filename) == 0) {
    print("no files to analyse")
  }
  
  if (length(params_rawdata[["rmc.firstrow.acc"]]) == 1) {
    dformat = 5
    mon = 0
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
                                    rmc.desiredtz = params_rawdata[["rmc.desiredtz"]], rmc.sf = params_rawdata[["rmc.sf"]],
                                    rmc.headername.sf = params_rawdata[["rmc.headername.sf"]],
                                    rmc.headername.sn = params_rawdata[["rmc.headername.sn"]],
                                    rmc.headername.recordingid = params_rawdata[["rmc.headername.sn"]],
                                    rmc.header.structure = params_rawdata[["rmc.header.structure"]],
                                    rmc.check4timegaps = params_rawdata[["rmc.check4timegaps"]])
    if (Pusercsvformat$header != "no header") {
      sf = Pusercsvformat$header$sample_rate
    } else {
      sf = params_rawdata[["rmc.sf"]]
    }
  } else if (length(params_rawdata[["rmc.firstrow.acc"]]) == 0) {
    INFI = getbrand(filename, datafile)
    mon = INFI$mon
    dformat = INFI$dformat
    sf = INFI$sf
    datafile = INFI$datafile
  }
  if (dformat == 1) { #binary data
    if (mon == 1) { # genea
      genea = g.binread(datafile,0,1)
      H = genea$header
    } else if (mon == 2) { #geneactive
      H = GENEAread::header.info(binfile = datafile, more = F)
    } else if (mon == 5) { #movisens
      H = "file does not have header" # these files have no header
    }
  } else if (dformat == 2) { #csv data
    if (mon == 2) { # geneactiv
      H = read.csv(datafile,nrow = 20, skip = 0) #note that not the entire header is copied
      # cat("\nGENEACTIV csv files support is deprecated in GGIR v2.6-2 onwards. Please, either use the GENEACTIV bin files or the read.myacc.csv function on the csv files")
    } else if (mon == 3) { #actigraph
      H = read.csv(datafile, nrow = 9, skip = 0)
    } else if (mon == 4) { #ax3 (axivity)
      H = "file does not have header" # these files have no header
    }
  } else if (dformat == 3) { #wav data
    header = c()
    try(expr = {header = rownames(read.csv(datafile, nrow = 15, header = TRUE))}, silent = TRUE)
    if (length(header) == 0) {
      header = rownames(read.csv(datafile, skipNul = TRUE, nrow = 15, header = TRUE, fileEncoding = "WINDOWS-1252"))
    }
    if (length(header) == 0) {
      header = rownames(read.csv(datafile, skipNul = TRUE, nrow = 15, header = TRUE, fileEncoding = "UTF-8"))
    }
    if (length(header) == 0) {
      header = rownames(read.csv(datafile, skipNul = TRUE, nrow = 15, header = TRUE, fileEncoding = "latin1"))
    }
    if (length(which(header %in% paste0(1:15, sep = "") == TRUE)) == 15) { #
      header = read.csv(datafile, skipNul = TRUE, nrow = 15, skip = 1, header = FALSE)
      if (ncol(header) == 2) {
        ii = which(is.na(header[,2]) == FALSE)
        if (length(ii) > 0) header = header[-ii,]
        header = header[,-2]
      }
    }
    if (length(header) <= 5) {
      header = rownames(read.csv(datafile, skipNul = TRUE, nrow = 15, header = TRUE))
    }
    H = sapply(header,function(x) {
      tmp = as.character(unlist(strsplit(as.character(x), ": ")))
      if (length(tmp) == 1) {
        tmp = c(tmp, NA)
      }
      tmp
    })
    if (is.list(H) == TRUE) {
      conv = c()
      for (jj in 1:length(H)) {
        conv = rbind(conv,H[[jj]], stringsAsFactors = TRUE)
      }
      H = as.data.frame(conv, stringsAsFactors = TRUE)
    } else {
      H = as.data.frame(t(H), stringsAsFactors = TRUE)
    }
    names(H) = c("hnames","hvalues")
  } else if (dformat == 4) { #cwa data
    PP = g.cwaread(datafile,start = 1, end = 10, desiredtz = desiredtz)
    H = PP$header
    
  } else if (dformat == 5) { # csv data in a user-specified format
    
    H = header = Pusercsvformat$header
    if (Pusercsvformat$header != "no header") {
      H = data.frame(name = row.names(header), value = header, stringsAsFactors = TRUE)
    }
    sf = params_rawdata[["rmc.sf"]]
  } else if (dformat == 6) { # gt3x
    info = read.gt3x::parse_gt3x_info(datafile, tz = desiredtz)
    info = info[lengths(info) != 0] # remove odd NULL in the list
    H = as.data.frame(info)
    H = data.frame(name = names(H), value = as.character(H), stringsAsFactors = FALSE)
    sf = as.numeric(H$value[which(H$name == "Sample.Rate")])
  }
  H = as.matrix(H)
  if (ncol(H) == 3 & dformat == 2 & mon == 3) {
    if (length(which(is.na(H[,2]) == FALSE)) == 0) {
      H = as.matrix(H[,1])
    }
  }
  if (ncol(H) == 1 & dformat == 2) {
    if (mon == 3) {
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
  if (dformat == 4) {
    header = data.frame(value = H, row.names = rownames(H), stringsAsFactors = TRUE)
  } else {
    if (mon == 2 & dformat == 1) {
      varname = rownames(as.matrix(H))
      H = data.frame(varname = varname,varvalue = as.character(H), stringsAsFactors = TRUE)
    } else {
      if (length(H) > 1 & class(H)[1] == "matrix") H = data.frame(varname = H[,1],varvalue = H[,2], stringsAsFactors = TRUE)
    }
  }
  if (dformat != 4 & length(H) > 1 & (class(H)[1] == "matrix" | class(H)[1] == "data.frame")) {
    RowsWithData = which(is.na(H[,1]) == FALSE)
    header = data.frame(value = H[RowsWithData, 2], row.names = H[RowsWithData, 1], stringsAsFactors = TRUE)
  }
  if (H[1,1] == "file does not have header") { #no header
    header = "no header"
  }
  if (mon == 3 & dformat != 6) {
    verisense_check = substr(colnames(read.csv(datafile,nrow = 1)[1]), start = 36, stop = 44)
    if (identical('Verisense', toString(verisense_check))) {
      mon = 6
    }
  }
  monc = mon
  monn = ifelse(mon > 0, monnames[mon], "unknown")
  dformc = dformat
  dformn = fornames[dformat]
  invisible(list(header = header, monc = monc, monn = monn,
                 dformc = dformc, dformn = dformn, sf = sf, filename = filename))
}

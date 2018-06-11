# GGIR used to depend on CRAN package GENEAread as developed by Joss Langford and Zhou Fang.
# Now GENEAread is depricated the essential parts of the code has been copied to GGIR to ensure
# ongoing functionality.
#-----------------------------------------------------------
#reads binary accelerometer data
#mmap: use mmap to read - potentially much faster for large files - by default only for 64 bit
#blocksize = number of pages to read at a time
read.bin <- function (binfile, outfile = NULL, start = NULL, end = NULL, 
                      verbose = TRUE, do.temp = TRUE,do.volt = TRUE, calibrate = TRUE, 
                      downsample = NULL, blocksize , virtual = FALSE,
                      mmap.load = (.Machine$sizeof.pointer >= 8), pagerefs = TRUE, ...) {
  #internal function for read.bin
  convert.hexstream <- function (stream) {
    maxint <- 2^(12 - 1)
    #packet <- as.integer(paste("0x",stream,sep = "")) #strtoi is faster
    packet <-bitops::bitShiftL(strtoi(stream, 16),4*(2:0))
    packet<-rowSums(matrix(packet,ncol=3,byrow=TRUE))
    packet[packet>=maxint] <- -(maxint - (packet[packet>=maxint] - maxint))
    packet<-matrix(packet,nrow=4)
    light <- bitops::bitShiftR(packet[4,],2)
    button <-bitops::bitShiftR(bitops::bitAnd(packet[4,],2),1)
    packet<-rbind(packet[1:3,],light,button)
    packet
  }
  
  convert.intstream <- function(stream) {
    maxint <- 2^(12 - 1)
    stream = stream - 48 - 7 * (stream > 64)
    packet<- drop(matrix(stream, ncol = 3, byrow = T) %*% 16^(3: 1 - 1))
    packet[packet>=maxint] <- -2*maxint + (packet[packet>=maxint] )
    packet<-matrix(packet,nrow=4)
    light = floor(packet[4,] / 4 -> ltmp)
    rbind(packet[1:3,], light, (ltmp-light) >0.49)
  }
  invisible(gc()) # garbage collect
  # if (mmap.load) require(mmap)
  # optional argument initialization as NULL. Arguments assigned
  # if they appear in the function call.
  opt.args<-c("gain","offset","luxv","voltv", "warn")
  
  warn <- FALSE
  gain<-offset<-NULL
  luxv<-voltv<-NULL
  
  argl<-as.list(match.call())
  
  argind<-pmatch(names(argl),opt.args)
  argind<-which(!is.na(argind))
  
  if(length(argind)>0){
    called.args<-match.arg(names(argl),opt.args,several.ok=TRUE)
    for(i in 1:length(called.args)){
      assign(called.args[i],eval(argl[[argind[i]]]))
    }
  }
  
  #variables for positions and record lengths in file
  nobs <- 300
  reclength <- 10
  position.data <- 10
  position.temperature <- 6
  position.volts <- 7
  orig.opt <- options(digits.secs = 3)
  #initialise some variables
  pos.rec1 <- firstpage <- npages <- t1c <- t1midnight <- pos.inc<-headlines<- NA
  
  #get header and calibration info using header.info.
  header = header.info(binfile, more = T)
  commasep = unlist(header)[17] == ","   #decimal seperator is comma?
  
  H = attr(header, "calibration")
  for (i in 1:length(H)) assign(names(H)[i], H[[i]])
  
  if ((!exists("pos.rec1")) || (is.na(pos.rec1))) mmap.load = FALSE
  
  #temporary workaround.... calculate pagerefs
  if ((firstpage != 0) && (mmap.load == T) && (length(pagerefs) < 2) ) pagerefs = TRUE
  
  if (missing(blocksize)){
    blocksize = Inf
    if (npages > 10000) blocksize = 10000
  }
  freqint = round(freq)
  if (!is.null(downsample)) {
    if (verbose) {
      cat("Downsampling to ", round(freq/downsample[1],2) , " Hz \n")
      if (nobs %% downsample[1] != 0) cat("Warning, downsample divisor not factor of ", nobs, "!\n")
      if ( downsample[1] != floor( downsample[1]) ) cat("Warning, downsample divisor not integer!\n")
    }
  }
  if (verbose) {
    cat("Number of pages in binary file:", npages, "\n")
  }
  freqseq <- seq(0, by = 1/freq, length = nobs)
  timespan <- nobs/freq
  #    t1 <- t1[2:length(t1)]
  #   t1[1] <- substr(t1[1], 6, nchar(t1[1]))
  timestampsc <- seq(t1c, by = timespan, length = npages)
  timestamps <- seq(t1, by = timespan, length = npages)
  tnc <- timestampsc[npages]
  tn <- timestamps[npages]
  if (is.null(start)) {
    start <- 1
  }
  if (is.null(end)) {
    end <- npages
  }
  
  
  #goal is to end up with start, end as page refs
  if (is.numeric(start)) {
    if ((start[1] > npages)) {
      stop(cat("Please input valid start and end times between ", 
               t1c, " and ", tnc, " or pages between 1 and ", 
               npages, ".\n\n"), call. = FALSE)
    } else if (start[1] < 1) {
      #specify a proportional point to start
      start = pmax(floor( start * npages),1)
    }
  }
  if (is.numeric(end)) {
    if ((end[1] <= 1)) {
      #specify a proportional point to end
      end= ceiling(end * npages)
    }
    else {
      end <- pmin(end, npages)
    }
  }
  #parse times, including partial times, and times with day offsets
  if (is.character(start)) {
    start <- parse.time(start, format = "seconds", start = t1, startmidnight = t1midnight)
    start = findInterval(start-0.5, timestamps, all.inside = T)#which(timestamps >= start-(0.5))[1]
    t1 = timestamps[start+1]
  }
  if (is.character(end)) {
    end <- parse.time(end, format = "seconds", start = t1, startmidnight = t1midnight)
    end = findInterval(end, timestamps, all.inside = T) +1#max(which(timestamps<= (end+0.5) ))
  }
  
  index <-  NULL
  for (i in 1:length(start)){
    index = c(index, start[i]:end[i])
  }
  
  #    d1 <- max(which((timestamps - start) <= 0))
  #   index <- unique(c(d1, index))
  if (length(index) == 0) {
    if (npages > 15) {
      stop("No pages to process with specified timestamps.  Please try again.\n", 
           call. = FALSE)
    }
    else {
      stop("No pages to process with specified timestamps.  Please try again. Timestamps in binfile are:\n\n", 
           paste(timestampsc, collapse = " \n"), " \n\n", 
           call. = FALSE)
    }
  }
  if (do.temp) {
    temperature <- NULL
  }
  if (calibrate) {
    if (!is.null(gain)) {
      if (!is.numeric(gain)) {
        stop("Please enter 3 valid values for the x,y,z gains.\n")
      }
      else {
        xgain <- gain[1]
        ygain <- gain[2]
        zgain <- gain[3]
      }
    }
    if (!is.null(offset)) {
      if (!is.numeric(offset)) {
        stop("Please enter 3 valid values for the x,y,z offsets.\n")
      }
      else {
        xoffset <- offset[1]
        yoffset <- offset[2]
        zoffset <- offset[3]
      }
    }
    if (!is.null(voltv)) {
      if (!is.numeric(voltv)) {
        stop("Please enter a valid value for the volts.\n")
      }
      else {
        volts <- voltv
      }
    }
    if (!is.null(luxv)) {
      if (!is.numeric(luxv)) {
        stop("Please enter a valid value for the lux.\n")
      }
      else {
        lux <- luxv
      }
    }
  }
  nstreams <- length(index)
  if(warn){
    if (nstreams > 100) {
      cat("About to read and process", nstreams, "datasets.  Continue?  Press Enter or control-C to quit.\n")
      scan(, quiet = TRUE)
    }
  }
  
  data <- NULL
  
  
  invisible(gc()) # garbage collect
  if (mmap.load) {
    #function to get numbers from ascii codes
    numstrip <- function(dat, size = 4, sep = "." ){
      apply(matrix(dat, size), 2,
            function(t) as.numeric(sub(sep, ".", 
                                       rawToChar(as.raw(t[t != 58])), fixed = TRUE)))
    }
    offset =  pos.rec1 - 2#findInterval(58,cumsum((mmapobj[1:3000] == 13)))+ 1 #TODO
    rec2 = offset + pos.inc
    
    if ((identical(pagerefs , FALSE)) || is.null(pagerefs)){
      pagerefs = NULL
    } else if (length(pagerefs) < max(index)){
      #calculate pagerefs!
      textobj = mmap::mmap(binfile, mmap::char())
      if (mmap::is.mmap(textobj)){
        startoffset = max(pagerefs, offset) + pos.inc
        if (identical(pagerefs, TRUE)) pagerefs = NULL
        numblocks2 = 1
        blocksize2 = min(blocksize, max(index+1))*3600
        if ( (length(textobj) -startoffset) > blocksize2 ){
          numblocks2 = ceiling((length(textobj) - startoffset) /blocksize2)
        }
        curr = startoffset
        for (i in 1:numblocks2){
          pagerefs = c(pagerefs, grepRaw("Recorded Data", 
                                         textobj[curr + 1: min(blocksize2, length(textobj) - curr)],
                                         all = T)+ curr-2)
          curr = curr + blocksize2
          if (length(pagerefs) >= max(index)) break
        }
        if (curr >= length(textobj)){    # pagerefs = c(pagerefs, length(textobj)  -1)
          pagerefs = c(pagerefs, 
                       length(textobj) - grepRaw("[0-9A-Z]",
                                                 rev(textobj[max(pagerefs):length(textobj)]))+2)
        }
        if (verbose) cat("Calculated page references... \n")
        mmap::munmap(textobj)
        invisible(gc()) # garbage collect
      } else {
        pagerefs = NULL
        warning("Failed to compute page refs")
      }
    }
    mmapobj = mmap::mmap(binfile, mmap::uint8())
    if (!mmap::is.mmap(mmapobj)){
      warning("MMAP failed, switching to ReadLine. (Likely insufficient address space)")
      mmap.load = FALSE
    }
    
    
    #if (firstpage != 0) pos.inc = pos.inc - floor(log10(firstpage))
    
    #getindex gives either the datavector, or the pos after the tail of the record
    if (is.null(pagerefs)){
      print("WARNING: Estimating data page references. This can fail if data format is unusual!") #better warn about this, it can come up
      digitstring = cumsum(c(offset,10*(pos.inc), 90 *(pos.inc + 1) ,
                             900 *( pos.inc +2 ), 9000*(pos.inc +3) , 
                             90000*(pos.inc +4) , 900000*(pos.inc +5), 9000000 * (pos.inc + 6)))
      digitstring[1] = digitstring[1] + pos.inc #offset a bit since 10^0 = 1
      getindex = function(pagenumbers, raw = F   ){
        digits = floor(log10(pagenumbers))
        if (raw){
          return(   digitstring[digits+1]+(pagenumbers - 10^digits)*(pos.inc+digits  ))
        } else {
          return( rep(digitstring[digits+1]+(pagenumbers - 10^digits)*
                        (pos.inc+digits),each =  nobs * 12)  -((nobs*12):1))
        }
      }
      #if (firstpage != 0){
      ##where offset must have been to give page at the right place
      #	offset = offset - (getindex(firstpage+1, raw = T) - rec2)
      ##redefine new getindex
      #	digitstring = cumsum(c(offset,10*(pos.inc), 90 *(pos.inc + 1) , 900 *( pos.inc +2 ), 9000*(pos.inc +3) , 90000*(pos.inc +4) , 900000*(pos.inc +5), 9000000 * (pos.inc + 6)))
      #	digitstring[1] = digitstring[1] + pos.inc #offset a bit since 10^0 = 1
      #	getindex = function(pagenumbers, raw = F   ){
      #pagenumbers = pagenumbers + firstpage
      #		digits = floor(log10(pagenumbers))
      #	if (raw){
      #		return(   digitstring[digits+1]+(pagenumbers - 10^digits)*(pos.inc+digits  ))
      #	} else {
      #		return( rep(digitstring[digits+1]+(pagenumbers - 10^digits)*(pos.inc+digits),each =  nobs * 12)  -((nobs*12):1))
      #	}
      #	}
      #
      #
      #}
      #
      
    } else {
      getindex = function(pagenumbers, raw = F){
        if (raw){
          return(pagerefs[pagenumbers]) 
        }else{
          return(rep(pagerefs[pagenumbers], each = nobs * 12 )  -((nobs*12):1))
        }
      }
    }
    
  } 
  
  if (mmap.load != TRUE) {
    
    fc2 = file(binfile, "rt")
    #skip to start of data blocks
    #skip header
    tmpd <- readLines(fc2, n = headlines)
    
    #skip unneeded pages
    replicate ( min( index - 1 ), is.character(readLines(fc2, n=reclength)))
  }
  
  numblocks = 1
  blocksize = min(blocksize, nstreams)
  if (nstreams > blocksize ){
    if (verbose) cat("Splitting into ", ceiling(nstreams/blocksize), " chunks.\n") 
    numblocks = ceiling(nstreams/blocksize)
  }
  
  Fulldat = NULL
  Fullindex = index#matrix(index, ncol = numblocks)
  index.orig = index
  
  if (verbose)	    {
    cat("Processing...\n")
    pb <- txtProgressBar(min = 0, max = 100,style=1)
  }
  start.proc.time <- Sys.time()
  if(!is.null(downsample)){
    downsampleoffset = 1
    if (length(downsample) == 2){
      downsampleoffset = downsample[2]
      downsample = downsample[1]
    }
  }
  
  if (virtual){
    if (is.null(downsample)) downsample = 1
    if (verbose) close(pb)
    if (exists("fc2")) close(fc2)
    if (exists("mmapobj")) mmap::munmap(mmapobj)
    #todo...
    Fulldat = timestamps[index]
    if (verbose) cat("Virtually loaded", length(Fulldat)*length(freqseq)/downsample, 
                     "records at", round(freq/downsample,2), "Hz (Will take up approx "
                     , round(56 * as.double(length(Fulldat) * length(freqseq)/downsample )/1000000) ,"MB of RAM)\n")
    if (verbose) cat(format.GRtime(Fulldat[1], format = "%y-%m-%d %H:%M:%OS3 (%a)"),
                     " to ", format.GRtime(utils::tail(Fulldat,1) + nobs /freq,format = "%y-%m-%d %H:%M:%OS3 (%a)"), "\n")
    output = list(data.out = Fulldat, page.timestamps = timestampsc[index.orig], 
                  freq= as.double(freq)/downsample , 
                  filename =utils::tail(strsplit(binfile, "/")[[1]],1), 
                  page.numbers = index.orig, call = argl,
                  nobs = floor(length(freqseq)/downsample) ,
                  pagerefs = pagerefs, header = header)
    class(output) = "VirtAccData"
    return(invisible( output  ))
  }
  
  voltages = NULL
  lastread = min(index) -1
  for (blocknumber in 1: numblocks){
    index = Fullindex[1:min(blocksize, length(Fullindex))]
    Fullindex = Fullindex[-(1:blocksize)]
    proc.file <- NULL
    if (!mmap.load){
      tmpd <- readLines(fc2, n = (max(index) -lastread) * reclength  )
      bseq = (index - lastread -1 ) * reclength
      lastread = max(index)
      if (do.volt){
        vdata = tmpd[bseq + position.volts]
        if (commasep) vdata = sub(",", ".", vdata, fixed = TRUE)
        voltages = c(voltages, as.numeric(substring(vdata, 17, nchar(vdata)))) 
      }
      if (is.null(downsample)){
        data <- strsplit(paste(tmpd[ bseq + position.data], collapse = ""), "")[[1]]
        
        if (do.temp) {	
          tdata <- tmpd[bseq + position.temperature]
          if (commasep) tdata = sub(",", ".", tdata, fixed = TRUE)
          temp <- as.numeric(substring(tdata, 13, nchar(tdata))) 
          temperature <- rep(temp, each = nobs)
        }
        # line below added for future beneficial gc
        rm(tmpd)
        #  data <- check.hex(data) #removed checks because taking too long, convert.hexstream should throw an error anyway.
        proc.file <- convert.hexstream(data)
        
        nn <- rep(timestamps[index], each = length(freqseq)) + freqseq
        ##So we are downsampling
      } else {
        
        data <- strsplit(paste(tmpd[ bseq + position.data], collapse = ""), "")[[1]]
        if (do.temp) {	
          tdata <- tmpd[bseq + position.temperature]
          if (commasep) tdata = sub(",", ".", tdata, fixed = TRUE)
          temp <- as.numeric(substring(tdata, 13, nchar(tdata))) 
          temperature <- rep(temp, each = nobs)
        }
        # line below added for future beneficial gc
        rm(tmpd)
        #  data <- check.hex(data) #removed checks because taking too long, convert.hexstream should throw an error anyway.
        proc.file <- convert.hexstream(data)
        nn <- rep(timestamps[index], each = length(freqseq)) + freqseq
        positions = downsampleoffset + (0: floor(( nobs * length(index)  - downsampleoffset )/downsample)) * downsample
        proc.file = proc.file[, positions]
        if (do.temp){
          temperature = temperature[positions]
        }
        nn  = nn[positions]
        #	freq = freq * ncol(proc.file)/ (nobs * (length(index)))
        downsampleoffset = downsample - (nobs*blocksize - downsampleoffset  )%% downsample 
      }	
      
    } else {
      #mmap reads
      ####################
      #read from file
      tmp = mmapobj[getindex(index)]
      proc.file = convert.intstream(tmp)
      # remember that getindex(id , raw = T) gives the byte offset after the end of
      # each data record. Seems like battery voltages and temperatures can vary in
      # terms of the number of bytes they take up... which is annoying
      # new plan:
      # try and discover where the byte offsets are...
      pageindices = getindex(index, raw = T)
      firstrec = as.raw(mmapobj[pageindices[1]:pageindices[2]])
      a = grepRaw("Temperature:", firstrec)
      b = grepRaw(ifelse(commasep, ",", "."), firstrec, offset = a, fixed = TRUE)
      c = grepRaw("Battery voltage:", firstrec, offset = b)
      d = grepRaw("Device", firstrec, offset = c)
      tind = (b-2):(c-2) - length(firstrec)
      vind = (c+16):(d-2) - length(firstrec)
      
      if (do.temp){
        temperature = rep(numstrip(mmapobj[rep(pageindices,
                                               each = length(tind)) + tind ], 
                                   size = length(tind), 
                                   sep = ifelse(commasep, ",", ".") ), 
                          each = nobs) #lets hope this doesn't slow things too much
      }
      nn <- rep(timestamps[index], each = length(freqseq)) + freqseq
      if (!is.null(downsample)){
        positions = downsampleoffset + 
          (0: floor(( nobs * length(index)  - downsampleoffset )/downsample)) * downsample
        proc.file = proc.file[, positions]
        nn  = nn[positions]
        if (do.temp){
          temperature = temperature[positions]
        }
        #	freq = freq * ncol(proc.file)/ (nobs * (length(index)))
        downsampleoffset = downsample - (nobs*blocksize - downsampleoffset  )%% downsample 
      }
      if (do.volt){
        voltages = c(voltages, numstrip(mmapobj[rep(pageindices, each = length(vind)) + vind ], size = length(vind) , sep = ifelse(commasep, ",", ".")) ) 
      }
    }
    if (verbose)	setTxtProgressBar(pb, 100 *  (blocknumber-0.5) / numblocks )
    if (calibrate) {
      proc.file[1, ] <- (proc.file[1, ] * 100 - xoffset)/xgain
      proc.file[2, ] <- (proc.file[2, ] * 100 - yoffset)/ygain
      proc.file[3, ] <- (proc.file[3, ] * 100 - zoffset)/zgain
      proc.file[4, ] <- proc.file[4, ] * lux/volts
    }
    proc.file <- t(proc.file)
    proc.file <- cbind(nn, proc.file)
    #    rownames(proc.file) <- paste("obs.", 1:nrow(proc.file)) # strip out row labels - waste of memory
    cnames <- c("timestamp", "x", "y", "z", "light", "button")
    if (do.temp) {
      proc.file <- cbind(proc.file, temperature)
      colnames(proc.file) <- c(cnames, "temperature")
    }
    else {
      colnames(proc.file) <- cnames
    }
    
    
    Fulldat= rbind(Fulldat, proc.file)
    if (verbose)	setTxtProgressBar(pb, 100 *  blocknumber / numblocks)
    
  }
  if (verbose) close(pb)
  
  freq = freq * nrow(Fulldat) / (nobs *  nstreams)
  end.proc.time <- Sys.time()
  cat("Processing took:", format(round(as.difftime(end.proc.time - 
                                                     start.proc.time), 3)), ".\n")
  cat("Loaded", nrow(Fulldat), "records (Approx ", round(utils::object.size(Fulldat)/1000000) ,"MB of RAM)\n")
  cat(format.GRtime( Fulldat[1,1], format = "%y-%m-%d %H:%M:%OS3 (%a)")," to ",
      format.GRtime(utils::tail(Fulldat[,1],1) , 
                    format = "%y-%m-%d %H:%M:%OS3 (%a)"), "\n")
  
  if (!mmap.load){
    close(fc2)
  } else {
    mmap::munmap(mmapobj)
  }
  processedfile <- list(data.out = Fulldat, page.timestamps = timestampsc[index.orig],
                        freq= freq, filename =utils::tail(strsplit(binfile, "/")[[1]],1), 
                        page.numbers = index.orig, call = argl,
                        page.volts = voltages, pagerefs = pagerefs, header = header)
  class(processedfile) = "AccData"
  if (is.null(outfile)) {
    return(processedfile)
  }
  else {
    save(processedfile, file = outfile)
  }
}
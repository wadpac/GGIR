g.binread = function(binfile,start=0,end=0) {
  int8 = function(x) {
    x[x < -128] = -128
    x[x > 127] = 127
    round(x)
  }
  int16 = function(x) {
    x[x < -32768] = -32768
    x[x > 32767] = 32767
    round(x)
  }
  int32 = function(x) {
    x[x < -2147483648] = -2147483648
    x[x > 2147483648] = 2147483647 
    round(x)
  }
  uint8 = function(x) {
    x[x < 0] = 0
    x[x > 255] = 255
    round(x)
  }
  uint16 = function(x) {
    x[x < 0] = 0
    x[x > 65535] = 65535
    round(x)
  }
  uint32 = function(x) {
    x [x < 0] = 0
    x [x > 4294967295] = 4294967295
    round(x)
  }
  getheader = function(fid) {
    ## Read file header and return as a struct. Fail if version string is not present
    seek(fid,0,origin="start")
    idstr = readChar(fid,5,useBytes=TRUE)
    if (all(idstr=="GENEA")) {
      verstr = readLines(fid,n=1)
      ## TODO: check file format version	
      header_items_raw = readBin(fid,"raw",n=10*onebyte)
      ## Find field ends (search for newline chars
      ends = c(0,matlab::find(header_items_raw==10))
      header_end = ends[min(matlab::find(diff(ends)==2))]+2
      header_items_raw = header_items_raw[1:header_end]
      ends = ends[ends<header_end]
      hlen = length(ends)
      for (i in 1:(hlen-1)) {
        ## Extract CR-LF-terminated strings. Ens given the posion of LF's, so
        ## to start after one LF up to before the next CR	
        hitem = rawToChar(header_items_raw[(1+ends[i]):(ends[i+1]-2)])
        ## Split field label from value by the first ":" in the string
        separ = min(unlist(gregexpr(":",hitem)))
        ## Trap blank line (the end-of-header marker)
        if (!matlab::isempty(hitem)) {
          hnames[i] = gsub("-","_",substr(hitem,1,(separ-1)))
        }
        if (!matlab::isempty(hnames[i])) hvalues[i] = substr(hitem,separ+1,nchar(hitem))
      }
      seek(fid,header_end+12,origin="start")
    } else {
      seek(fid,0,origin="start") 
    }
    header_end = header_end + 12
    invisible(list(header_end=header_end,hnames = hnames,hvalues = hvalues))
  }
  getaccelcalib = function(hnames,hvalues) {
    ## Parse accelerometer calibration information from header or substitute defaults
    if ((i=grep("Accel_Gains",hnames))>0) accel_gains = hvalues[i] else accel_gains = rep(8192,3)
    accel_gains = int32(as.integer(unlist(strsplit(accel_gains," "))))
    if ((i=grep("Accel_Offsets",hnames))>0) accel_offsets = hvalues[i] else accel_offsets = rep(0,3)
    accel_offsets = int16(as.integer(unlist(strsplit(accel_offsets," "))))
    invisible(list(accel_gains = accel_gains,accel_offsets = accel_offsets))
  }
  find_pages = function(fid,header_end,filelen,start,end) {
    npages = floor(filelen-header_end)/2048
    ## Find timestamps corresponding to start and end
    # somewhere between header_end and filelen
    output = find_timest(fid,header_end,0,npages,start)
    pagestart = output$npage
    output = find_timest(fid,header_end,0,npages,end)
    pageend = output$npagebound
    invisible(list(pagestart = pagestart,pageend = pageend))
  }
  find_timest = function(fid,noff,l,u,t) {
    npages  = u
    ## Check for default parameters
    done = 0
    while (done == 0) {
      if (t == 0) {
        npage = 0
        npagebound = l
        done = 1
      }
      if (t == Inf) {
        npage = Inf
        npagebound = Inf
        done = 1
      } ## Initialise everything
      pagesize = 2048
      nchunks = floor(pagesize/6)
      while (done == 0) {
        ## choose a page in middle of the range
        n = l + floor((u-l)/2)
        ## Go to n'th page and read it in
        seek(fid,(n*pagesize + noff),origin='start')
        page = readBin(fid,"raw",pagesize)
        page = as.matrix(as.integer(page))
        ## Sort into 6-byte chunk, dropping trailing padding bytes
        page = matlab::reshape(as.matrix(page[(1:(6*nchunks)),]),6,nchunks)
        ## Unpack timestamps
        chunktypes = getchunktypes(page)
        current_timest = unpack_timestamps(page,chunktypes)
        if (!matlab::isempty(current_timest)) {
        } else {
          ## If not timestamps in this page, go to nearby page and retry
          if (n < (u-1)) {
            n = n+1
          } else if (n > l) {
            n=n-1
          } else {
            ## If n=l=i-1 then this is the only possible page
            pagestart = n
            pageend = n+1
            done = 1
          }
        }
        if (current_timest[length(current_timest)] > t) {
          u = n
        } else if (current_timest[1] >= t) {
          u = n + 1
        }
        if (current_timest[1] <= t) {
          l = n
        }
        if (l >= (u-1)) {
          npage = l
          npagebound = min(l+1,npages)
          done = 1
        }
      }
    }
    invisible(list(npage = npage,npagebound = npagebound))
  }
  doread = function(fid,pagestart,pageend,accel_gains,accel_offsets) {
    ## Load bulk of data from the file
    pagesize = 2048
    ## Preallocate array, big enough for every 6-byte chunk in the file
    ## to carry data samples
    rawxyz = matrix(int16(0),(341*ceiling(((pageend-pagestart)/twobytes))),3)
    ## One timestamp per accelerometer triple
    timest = matrix(double(0),nrow(rawxyz),1)
    ## One battery voltage sample per page
    vbat_Vbat = matrix(double(ceiling( ((pageend-pagestart)/pagesize))),1)
    vbat_t = matrix(double(ceiling( ((pageend-pagestart)/pagesize))),1)
    accidx = batidx = 1
    ## Use NaN as value of unknown timestamp
    preceding_timest = NaN
    ## Try loading 10 pages at a time
    npages = 2000
    minacct = round(npages * 341 * 0.6) # minimum number of acceleration chunktypes required per page
    nchunks = floor(pagesize/6)	
    ## Press display
    counter = 0
    nextmeg = 1024^2
    done = 0
    lastprogress = 0
    while (!done) {
      ## Read next npages pages
      pages = readBin(fid,"raw",min(npages*pagesize,(pageend-pagestart)-counter))
      ## Progress meter
      counter = counter + length(pages) 
      if ((counter>=(pageend-pagestart)) || (length(pages)<npages*pagesize)) {
        ## Last page
        done  = 1 # Exit main loop next time
        npages = floor(length(pages)/pagesize)
        pages = pages[1:(npages*pagesize)]
      }
      pages = as.matrix(as.integer(pages))
      pages = matlab::reshape(as.matrix(pages),pagesize,npages)
      ## Sort into 6-byte chunk, dropping trailing padding bytes
      pages = matlab::reshape(as.matrix(pages[1:(6*nchunks),]),6,nchunks*npages)
      ## Find chunk types (which identify data vs. timestamp etc.)
      chunktypes = bitops::bitShiftR(pages[2,],4)
      ## Unpack 12-bit data == identical to matlab
      ct0 = (chunktypes== 0)
      n_acc_chunks = sum(ct0)
      xyzidx = accidx:(accidx+n_acc_chunks-1)
      if (sum(ct0) < minacct) {
        done = 1
      }
      rawxyz[xyzidx,1] = pages[3,ct0] + bitops::bitShiftL(bitops::bitAnd(pages[2,ct0],15),8)
      rawxyz[xyzidx,2] = bitops::bitShiftL(pages[4,ct0],4) + bitops::bitShiftR(pages[5,ct0],4)
      rawxyz[xyzidx,3] = bitops::bitShiftL(bitops::bitAnd(pages[5,ct0],15),8) + pages[6,ct0]
      nktypes = bitops::bitShiftR(pages[2,],4)
      
      ## Unpack timestamp data
      current_timest = unpack_timestamps(pages,chunktypes)
      ## Lookup table to map back to preceding timestamp
      timestamp_lookup = cumsum(chunktypes == 2)
      ## Prepend timestamp from before the current block
      current_timest = c(preceding_timest,current_timest)
      ## Unpack bat voltage data
      out = unpack_vbat(vbat_Vbat,vbat_t,batidx,pages,chunktypes,current_timest,timestamp_lookup)
      vbat_Vbat = out$vbat_Vbat
      vbat_t = out$vbat_t
      batidx= out$batidx
      if (!matlab::isempty(timest)) {
        ## Put copies of the most recent timestamp in an array
        ## Each entry in timest corresponds to an accelerometer sample
        # (chunktype 0). At first it takes the value of
        # preceding _timest from a previous iteration. Later it takes
        # the value of the timestamps unpacked above
        ## Now fill in values
        timest[xyzidx] = current_timest[1+timestamp_lookup[ct0]]
      }
      ## Store most recent timestamp for the next loop iteration
      preceding_timest = current_timest[length(current_timest)]
      ## Cast and calibrate accelerometer data
      rawxyz = convert_xyz(rawxyz,accel_gains,accel_offsets,accidx,n_acc_chunks)
      ## Move indix into main array on
      accidx = accidx + n_acc_chunks
      ## Display progress
      progress = (counter/(pageend-pagestart))*100
      timernext = as.numeric(Sys.time())
      timep =(timernext - timerstart) / 60 # time passed in minutes
      secp = abs(floor((timep - floor(timep)) * 60))
      minp = abs(floor(timep))
      ignore = 0
      if (progress-lastprogress > 5) {
        vprogress = (round(progress * 100))/100
        if (vprogress < 99 | ignore == 1) {
          if (minp < 10 && secp < 10)	{
            cat(paste("0",minp,":0",secp,"   ",vprogress,"%", sep=""),"\n")
          } else if (minp < 10 && secp >= 10) {
            cat(paste("0",minp,":",secp,"   ",vprogress,"%", sep=""),"\n")
          } else if (minp >= 10 && secp < 10) {
            cat(paste(minp,":0",secp,"   ",vprogress,"%", sep=""),"\n")
          } else {
            cat(paste(minp,":",secp,"   ",vprogress,"%", sep=""),"\n")
          }
          ignore = 1
        }
      }
      lastprogress = progress
    }
    invisible(list(accidx = accidx,rawxyz = rawxyz,vbat_Vbat = vbat_Vbat,vbat_t = vbat_t,timest = timest))
  }
  getchunktypes = function(pages) {
    getchunktypes = bitops::bitShiftR(pages[2,],4)
  }
  unpack_vbat = function(vbat_Vbat,vbat_t,batidx,pages,chunktypes,current_timest,timestamp_lookup) {
    ## Get battery voltages
    ct4 = c(chunktypes == 4)
    vbat_Vbat[batidx:(batidx+(sum(ct4)-1))] = pages[3,ct4]
    ## Get timestamps
    vbat_t[batidx:(batidx+(sum(ct4)-1))] = current_timest[1+timestamp_lookup[ct4]]
    ## Omcre,emt omdex
    batidx = batidx + sum(ct4)
    invisible(list(vbat_Vbat = vbat_Vbat,vbat_t = vbat_t, batidx =  batidx))
  }
  unpack_timestamps = function(pages,chunktypes) {
    ct2 = (chunktypes==2)
    s24 = bitops::bitShiftL(uint32(pages[3,ct2]),24)
    s16 = bitops::bitShiftL(uint32(pages[4,ct2]),16)
    s8 = bitops::bitShiftL(uint32(pages[5,ct2]),8)
    s0 = uint32(pages[6,ct2])
    unpack_timestamps = s24+s16+s8+s0
  }
  convert_xyz = function(rawxyz,accel_gains,accel_offsets,accidx,n_acc_chunks) {
    ## Cpmvert 12-bit two's complement data to 16-bit signed int
    ## (no built-in cast to do this, so have to do it by hand)
    xyzidx = accidx:(accidx+n_acc_chunks-1)
    rawxyz[xyzidx,] = rawxyz[xyzidx,] - int16(4096*(rawxyz[xyzidx,]>2047))
    ## Insert x and z axes
    rawxyz[xyzidx,c(1,3)] = -rawxyz[xyzidx,c(1,3)]
    ## Apply calibration seetings
    rawxyz[xyzidx,] = rawxyz[xyzidx,] + matlab::repmat(accel_offsets,n_acc_chunks,1)
    rawxyz[xyzidx,] = int16((int32(rawxyz[xyzidx,]) * matlab::repmat(accel_gains,n_acc_chunks,1))/8192)
    ## Convert to milliG
    rawxyz[xyzidx,] = int16((int32(rawxyz[xyzidx,]) * 1000)/340)
    convert_xyz = rawxyz
  }
  reformat_timestamps = function(timest,endofdata) {
    ## Interpolate timestamp values
    ## Find positions where timestamps changes
    ## (which includes the position of the earliest timestamp)
    ts_steps = matlab::find(diff(timest) > 0) +1
    rate = (diff(timest[ts_steps-1])) / diff(ts_steps)
    for (stepidx in 1:(length(ts_steps)-1)) {
      rg = ts_steps[stepidx]:(ts_steps[stepidx+1]-1)
      timest[rg] = timest[rg] + (rate[stepidx]*(rg - rg[1]))
    }
    ## Fill in before the first timestamp
    timest[1:(ts_steps[1] - 1)] = timest[ts_steps[1]] - diff(timest[ts_steps[1:2]]) / diff(ts_steps[1:2]) * seq((ts_steps[1]-1),1,by= -1)
    ## Fill in after the last timestamp
    LST = length(ts_steps)
    timest[ts_steps[LST]:endofdata] = timest[ts_steps[LST]]
    + diff(timest[ts_steps[(LST-1):LST]]) / diff(ts_steps[(LST-1):LST]) * (0:(endofdata - ts_steps[LST]))
    ## Pad end of timestamp array with last value of timestamp
    timest[endofdata:length(timest)] = timest[endofdata-1]
    reformat_timestamps = timest
  }
  reformat_vbat_timestamps = function(vbat_Vbat,vbat_t) {
    ## Fill in timestamps before the first
    idx = 1
    while (is.nan(vbat_t[idx])) {
      idx = idx +1
      if (idx > length(vbat_t)) {
        ## All NaN, so nothing to do
        break
      }
      first_ts = vbat_t[idx]
      ## Previous timestamp must be first_ts-1, so fill in this
      ## missing value
      vbat_t[1:idx-1] = first_ts-1
    }
    invisible(list(vbat_Vbat = vbat_Vbat,vbat_t = vbat_t))
  }
  #==========================================================================================
  ## Parse input arguments
  timerstart = as.numeric(Sys.time())
  nargin = length(as.list(match.call())) - 1
  if (nargin < 1) {
    binfile = "out.bin"
  }
  ## Start from start of file
  if (nargin < 2) {
    start = 0
    pagestart = 0
  }
  ## Continue to end of file
  if (nargin < 3) {
    end = Inf
    pageend = Inf
  }
  timest = Inf
  onebyte = 1024
  twobytes = 2*onebyte
  fourbytes = 4*onebyte
  eightbytes = 8*onebyte
  megabyte = onebyte*onebyte
  pagesize = 2048
  #G = 1
  hnames = hvalues = vector()
  fid = file(binfile,"rb")
  filelen = file.info(binfile)$size
  ## Get time arguments into a standard form
  t_start = as.numeric(as.POSIXct(start,origin="1970-01-01",tz="Europe/London")) # should be in format "year-month-day hr:min:sec"
  t_end = as.numeric(as.POSIXct(end,origin="1970-01-01",tz="Europe/London")) # should be in format "year-month-day hr:min:sec"
  ## read header
  out = getheader(fid)
  hvalues = out$hvalues
  hnames = out$hnames
  header_end = out$header_end
  skipstart = skipend = 0
  if (is.numeric(start)) {
    pagestart = start * pagesize
    skipstart = 1
  }
  if (is.numeric(end)) {
    if (end != Inf){
      pageend = end * pagesize
      skipend = 1
    }
  }
  ## Search for start and end timestamps
  if (skipstart == 0) {
    out = find_pages(fid,header_end,filelen,t_start,t_end)
    pagestart = out$pagestart * pagesize
  }
  if (skipend == 0) {
    pageend = out$pageend * pagesize
  }
  ## Move file pointer to start of data of interest
  seek(fid,pagestart+header_end,'start')
  ## Extract accel calibration == identical to matlab
  out = getaccelcalib(hnames,hvalues)
  accel_gains = out$accel_gains
  accel_offsets = out$accel_offsets
  ## Find endpoint
  pageend = min(pageend,filelen)
  ## Read file (with or without timestamps as required)
  if (!matlab::isempty(timest)) {
    out = doread(fid,pagestart,pageend,accel_gains,accel_offsets)
    accidx = out$accidx
    rawxyz = out$rawxyz
    vbat_Vbat = out$vbat_Vbat
    vbat_t = out$vbat_t
    timest = out$timest
    rm(out)
  } else {
    out = doread(fid,pagestart,pageend,accel_gains,accel_offsets)
    accidx = out$accidx
    rawxyz = out$rawxyz
    vbat_Vbat = out$vbat_Vbat
    vbat_t = out$vbat_t
    timest = out$timest
    rm(out)
  }
  vbat_Vbat = vbat_Vbat *3.3*11/1024
  ## Get timestamps into useful format (interpolating as necessary)
  if (!matlab::isempty(timest)) {
    timest = reformat_timestamps(timest,accidx)
  }
  if (!matlab::isempty(vbat_Vbat)) {
    out = reformat_vbat_timestamps(vbat_Vbat,vbat_t)
    vbat_Vbat = out$vbat_Vbat
    vbat_t = out$vbat_t
    rm(out)
  }
  close(fid)
  cut = which(timest == mean(max(timest)))
  timest = as.matrix(timest[1:cut[1]])
  rawxyz = as.matrix(rawxyz[1:cut[1],])
  datetime = timest
  class(datetime) = c("POSIXt","POSIXct")
  vbat_dt = vbat_t
  class(vbat_dt) = c("POSIXt","POSIXct")
  fs = as.numeric(unlist(strsplit(hvalues[which(hnames == "Sample_Rate")],"Hz")))
  t = timest
  vt = vbat_t
  ts = (t-t[1])
  t = ts / 3600
  vt = (vt-vt[1]) / 3600
  invisible(list(rawxyz=rawxyz,header=cbind(hnames,hvalues),timestamps1=timest,timestamps2 = datetime,batt.voltage=cbind(vbat_t,vbat_Vbat,vbat_dt)))
}
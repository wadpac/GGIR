get_nw_clip_block_params = function(chunksize, dynrange, monc, rmc.noise=c(), sf, dformat,
                                    rmc.dynamic_range) {
  blocksize = round(14512 * (sf/50) * chunksize)
  if (monc == 1) blocksize = round(21467 * (sf/80)  * chunksize)
  if (monc == 3 & dformat == 2) blocksize = round(blocksize)#round(blocksize/5) # Actigraph
  if (monc == 3 & dformat == 6) blocksize = (24 * 3600) * chunksize
  if (monc == 4 & dformat == 3) blocksize = round(1440 * chunksize)
  if (monc == 4 & dformat == 4) blocksize = round(blocksize * 1.0043)
  if (monc == 4 & dformat == 2) blocksize = round(blocksize)
  if (monc == 5) blocksize = sf * 60 * 1440
  if (monc == 6 & dformat == 2) blocksize = round(blocksize)

  #Clipping threshold: estimate number of data points of clipping based on raw data at about 87 Hz
  if (length(dynrange) > 0) {
    clipthres = dynrange - 0.5
  } else {
    if (monc == 1) {
      clipthres = 5.5
    } else if (monc == 2) {
      clipthres = 7.5
    } else if (monc == 3) {
      clipthres = 7.5 # hard coded assumption that dynamic range is 8g
    } else if (monc == 4) {
      clipthres = 7.5 # hard coded assumption that dynamic range is 8g
    } else if (monc == 5) {
      clipthres = 15.5 # hard coded assumption that dynamic range is 16g
    } else if (monc == 6) {
      clipthres = 7.5
    } else if (monc == 0) {
      clipthres = rmc.dynamic_range
    }
  }
  # Nonwear threshold: #non-wear criteria are monitor specific
  racriter = 0.15 #very likely irrelevant parameters, but leave in for consistency
  if (monc == 1) {
    sdcriter = 0.003
    racriter = 0.05
  } else if (monc == 2) {
    sdcriter = 0.013
  } else if (monc == 3) {
    sdcriter = 0.013
  } else if (monc == 4) {
    sdcriter = 0.013
  } else if (monc == 5) {
    sdcriter = 0.013
  } else if (monc == 6) {
    sdcriter = 0.013
    racriter = 0.20
  } else if (monc == 0) {
    if (length(rmc.noise) == 0) {
      stop("Argument rmc.noise not specified, please specify expected noise level in g-units")
    }
    sdcriter = rmc.noise * 1.2
  }
  invisible(list(clipthres=clipthres, blocksize=blocksize, sdcriter=sdcriter, racriter=racriter))
}

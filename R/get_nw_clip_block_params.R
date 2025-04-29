get_nw_clip_block_params = function(monc, dformat, deviceSerialNumber = "", sf,
                                    params_rawdata) {
  blocksize = round(14512 * (sf/50) * params_rawdata[["chunksize"]])
  if (monc == MONITOR$GENEA) blocksize = round(21467 * (sf/80)  * params_rawdata[["chunksize"]])
  if (monc == MONITOR$ACTIGRAPH && dformat == FORMAT$CSV) blocksize = round(blocksize)#round(blocksize/5)
  if (monc == MONITOR$ACTIGRAPH && dformat == FORMAT$GT3X) blocksize = (24 * 3600) * params_rawdata[["chunksize"]]
  if (monc == MONITOR$AXIVITY && dformat == FORMAT$CWA) {
    if (utils::packageVersion("GGIRread") >= "0.3.1") {
      # 24-hour block.
      # CWA data blocks can have 40, 80 or 120 samples each; we'll take 80 as the average number.
      blocksize = round(24 * 3600 * sf / 80 * params_rawdata[["chunksize"]])      
    } else {
      blocksize = round(blocksize * 1.0043)
    }
  }
  if (monc == MONITOR$AXIVITY && dformat == FORMAT$CSV) blocksize = round(blocksize)
  if (monc == MONITOR$MOVISENS) blocksize = sf * 60 * 1440
  if (monc == MONITOR$VERISENSE && dformat == FORMAT$CSV) blocksize = round(blocksize)
  if (monc == MONITOR$PARMAY_MTX) blocksize = round(1440 / 2 * params_rawdata[["chunksize"]])

  dynrange = params_rawdata[["dynrange"]]
  if (monc == MONITOR$ACTIGRAPH) {
    # If Actigraph then try to specify dynamic range based on Actigraph model
    if (length(grep(pattern = "CLE", x = deviceSerialNumber)) == 1) {
      dynrange = 6
    } else if (length(grep(pattern = "MOS", x = deviceSerialNumber)) == 1) {
      dynrange = 8
    } else if (length(grep(pattern = "NEO", x = deviceSerialNumber)) == 1) {
      dynrange = 6
    }
  }

  # Clipping threshold
  if (length(dynrange) > 0) {
    clipthres = dynrange - 0.5
  } else {
    clipthres = 7.5 # hard-coded assumption that dynamic range is 8g
    #if (monc == MONITOR$GENEA) clipthres = 5.5
    if (monc == MONITOR$MOVISENS) {
      clipthres = 15.5 # hard coded assumption that dynamic range is 16g
    } else if (monc == MONITOR$AD_HOC) {
      clipthres = params_rawdata[["rmc.dynamic_range"]]
    }
  }
  # Nonwear threshold: non-wear criteria are monitor-specific
  racriter = params_rawdata[["nonwear_range_threshold"]] / 1000
  sdcriter = 0.013
  #if (monc == MONITOR$GENEA) { sdcriter = 0.003;  racriter = 0.05 }
  if (monc == MONITOR$VERISENSE) {
    racriter = 0.20
  } else if (monc == MONITOR$AD_HOC) {
    if (length(params_rawdata[["rmc.noise"]]) == 0) {
      stop("Argument rmc.noise not specified, please specify expected noise level in g-units")
    }
    sdcriter = params_rawdata[["rmc.noise"]] * 1.2
  }
  invisible(list(clipthres=clipthres, blocksize=blocksize, sdcriter=sdcriter, racriter=racriter))
}

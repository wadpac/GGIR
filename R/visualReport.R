visualReport = function(metadatadir = c(), 
                        f0 = c(), f1 = c(), overwrite = FALSE,
                        desiredtz = "",
                        verbose = TRUE,
                        part6_threshold_combi = NULL,
                        GGIRversion = NULL,
                        params_sleep = NULL) {
  if (!file.exists(paste0(metadatadir, "/results/file summary reports"))) {
    dir.create(file.path(paste0(metadatadir, "/results"), "file summary reports"))
  }
  
  # Declare functions:
  panelplot = function(mdat, ylabels_plot2, binary_vars,
                       BCN, BCC, title = "", NhoursPerRow = NULL, plotid = 0, legend_items = NULL, lux_available = FALSE,
                       step_count_available = FALSE, epochSize = 60) {
    window_duration = mdat$timenum[nrow(mdat)] - mdat$timenum[1]
    signalcolor = "black"

    # Prepare time tick points
    date = paste0(as.numeric(format(mdat$timestamp, "%d")), " ", format(mdat$timestamp, "%b"))
    hour = as.numeric(format(mdat$timestamp, "%H"))
    min = as.numeric(format(mdat$timestamp, "%M"))
    sec = as.numeric(format(mdat$timestamp, "%S"))
    ticks = which(min == 0 & sec == 0 & hour %in% seq(0, 24, by = 1))
    atTime = mdat$timestamp[ticks]
    datLIM = as.Date(min(mdat$timestamp, na.rm = TRUE), tz = desiredtz)
    XLIM = as.POSIXct(paste0(datLIM[1], " 00:00:00"), tz = desiredtz)
    XLIM[2] = XLIM[1] + NhoursPerRow * 3600
    
    if (step_count_available == TRUE) {
      steps_per_hour = round(zoo::rollsum(x = mdat$step_count, k = 60, fill = NA) / 100) # assumes 1 minute epoch
      steps_per_hour = as.character(steps_per_hour)
      steps_per_hour = gsub(pattern = "0", replacement = "", x = steps_per_hour)
    }
    if (lux_available == TRUE) {
      lux_per_hour = round(zoo::rollmax(x = mdat$lightpeak, k = 60, fill = NA) / 1000) # assumes 1 minute epoch
      lux_per_hour = as.character(lux_per_hour)
      lux_per_hour = gsub(pattern = "0", replacement = "", x = lux_per_hour)
    }
    
    correctRect = function(starti, endi, NR, epochSize) {
      if (endi[length(endi)] > NR) {
        if (starti[length(starti)] < endi[length(endi)] - 1) {
          endi[length(endi)] = endi[length(endi)] - epochSize
        } else {
          starti = starti[1:(length(starti) - 1)]
          endi = endi[1:(length(endi) - 1)]
        }
      }
      invisible(list(starti = starti, endi = endi))
    }
    
    
    par(mar = c(3, 0, 1.5, 2.5))
    #============================================
    # Acceleration and angle signal
    # scale 0 - 100, where bottom half is used for angle and top half for acceleration
    Ymax = 100
    Ymin = 0
    accy = (mdat$ACC / 10) + 50
    
    # identify angle columns
    anglecols = sort(grep(pattern = "angle", x = colnames(mdat), value = TRUE))
    Nangles = length(anglecols)
    if (Nangles == 1) {
      ang1 = (((mdat[, anglecols] + 90) * 40) / 180)
    } else if (Nangles == 2) {
      ang1 = (((mdat[, anglecols[1]] + 90) * 40) / 180)
      ang2 = (((mdat[, anglecols[1]] + 90) * 40) / 180) #+ 20
    } else if (Nangles == 3) {
      ang1 = (((mdat[, anglecols[1]] + 90) * 40) / 180)
      ang2 = (((mdat[, anglecols[2]] + 90) * 40) / 180) #+ 13
      ang3 = (((mdat[, anglecols[3]] + 90) * 40) / 180) #+ 26
    }

    if (lux_available == TRUE) {
      luxy = ceiling(pmin(mdat$lightpeak + 1, 20000) / 400) * 2
    }
    plot(0:1, 0:1,
         ylim = c(0, Ymax), xlim = XLIM, 
         xaxt = 'n', axes = FALSE,
         xlab = "", ylab = "")
    
    if (NhoursPerRow <= 36) {
      cex_axis = 0.5
    } else {
      cex_axis = 0.5
    }
    
    changepoints = function(x) {
      Nvalues = length(x)
      if (Nvalues > 3) {
        changes = which(x[2:Nvalues] != x[1:(Nvalues - 1)])
        changes = sort(unique(c(1, changes, changes + 1, Nvalues)))
        if (length(changes) == 0) {
          changes = 1:Nvalues
        }
      } else {
        changes = 1:Nvalues
      } 
      return(changes)
    }  
    if (NhoursPerRow <= 36) {
      cex_mtext = 0.4
    } else {
      cex_mtext = 0.35 #25
    }
    
    line = 0
    line_delta = 0.5
    # assign timestamp axis:
    mtext(text = hour[ticks], side = 1, line = line, cex = cex_mtext, at = atTime)
    mtext(text = "hour", side = 1, line = line, cex = cex_mtext,
          at = atTime[length(atTime)] + 5000, adj = 0)
    
    line = line + line_delta
    # with window numbers
    windowNow = mdat$window[ticks]
    changes = changepoints(windowNow)
    mtext(text = windowNow[changes], side = 1, line = line, cex = cex_mtext,
          at = atTime[changes])
    mtext(text = "window", side = 1, line = line, cex = cex_mtext,
          at = atTime[length(atTime)] + 5000, adj = 0)
    
    line = line + line_delta
    # with dates
    dateNow = as.numeric(format(mdat$timestamp[ticks], "%d"))
    
    changes = changepoints(dateNow)
    mtext(text = dateNow[changes], side = 1, line = line, cex = cex_mtext,
          at = atTime[changes])
    mtext(text = "date", side = 1, line = line, cex = cex_mtext,
          at = atTime[length(atTime)] + 5000, adj = 0)
    line = line + line_delta
    
    # optional variables
    if (lux_available == TRUE) {
      mtext(text = lux_per_hour[ticks], side = 1, line = line, cex = cex_mtext,
            at = atTime)
      mtext(text = "kLux", side = 1, line = line, cex = cex_mtext,
            at = atTime[length(atTime)] + 5000, adj = 0)
      line = line + line_delta
    }
    if (step_count_available == TRUE) {
      mtext(text = steps_per_hour[ticks], side = 1, line = line, cex = cex_mtext,
            at = atTime)
      mtext(text = "steps x100", side = 1, line = line, cex = cex_mtext,
            at = atTime[length(atTime)] + 5000, adj = 0)
    }
    
    #============================================
    # add rectangular blocks to reflect classes
    Nlevels = max(legend_items$level, na.rm = TRUE) #sum(Nlevels) - 1 # -1 because invalidepoch will not be a rectangle
    for (labi in 1:length(legend_items$name)) {
      if (legend_items$name[labi] == "invalid") {
        # Will be plotted further down
      } else if (legend_items$name[labi] %in% c("nap", "sib")) {
        bin_ts = mdat$sib
        if (legend_items$name[labi] == "sib") {
          bin_ts[which(bin_ts != 1)] = 0
        } else if (legend_items$name[labi] == "nap") {
          bin_ts[which(bin_ts != 2)] = 0
          bin_ts[which(bin_ts == 2)] = 1
        }
        freqtab = table(bin_ts)
        if (length(freqtab) > 1 || names(freqtab)[1] == "1") {
          starti = which(diff(c(0, bin_ts)) == 1)
          endi = which(diff(c(bin_ts, 0)) == -1) + 1
          newi = correctRect(starti, endi, NR = nrow(mdat), epochSize)
          t0 = mdat$timestamp[starti = newi$starti]
          t1 = mdat$timestamp[endi = newi$endi]
          y0 = 25
          y1 = 50
          col = legend_items$col[labi]
          rect(xleft = t0, xright = t1, ybottom = y0, ytop = y1, col = col, border = FALSE)
        }
      } else {
        if (legend_items$name[labi] %in% c("diary_nap", "diary_nonwear", "diary_sleepwindow")) {
          # Diary-based classes
          tempi = which(mdat$selfreported == legend_items$name[labi])
          y0 = 0
          y1 = 25
        } else {
          # Accelerometer- based classes
          tempi = which(mdat$class_id == legend_items$code[labi])
          y0 = 50
          if (length(grep(pattern = "unbt", x = legend_items$name[labi])) > 0) {
            y1 = 90 # lower rectangle for unbouted behaviour
          } else {
            y1 = 100
          }
        }
        if (length(tempi) > 0) {
          A = rep(0, nrow(mdat))
          A[tempi] = 1
          starti = which(diff(c(0, A)) == 1)
          endi = which(diff(c(A, 0)) == -1) + 1
          newi = correctRect(starti, endi, NR = nrow(mdat), epochSize)
          starti = newi$starti
          endi = newi$endi
          t0 = mdat$timestamp[starti]
          t1 = mdat$timestamp[endi]
          col = legend_items$col[labi]
          rect(xleft = t0, xright = t1, ybottom = y0, ytop = y1 , col = col, border = FALSE)
        }
      }
    }
    
    # Add lines on top
    lines(mdat$timestamp, accy, type = "l",
          col = signalcolor,
          lwd = 0.3)
    angleColor = ifelse(Nangles > 1, yes = "blue", no = signalcolor)
    lines(mdat$timestamp, ang1, type = "l", col = angleColor, lwd = 0.3)
    if (Nangles > 1) {
      lines(mdat$timestamp, ang2, type = "l", col = "red", lwd = 0.3)
    }
    if (Nangles > 2) {
      lines(mdat$timestamp, ang3, type = "l", col = "green", lwd = 0.3)
    }
    text(x = mdat$timestamp[1], y = 60, labels = "Acceleration",
         pos = 4, cex = 0.7, col = signalcolor, font = 2)
    textAngle = ifelse(Nangles > 1, yes = "Angles", no = "Angle")
    text(x = mdat$timestamp[1], y = 40, labels = textAngle,
         pos = 4, cex = 0.7, col = signalcolor, font = 2)
    
    # Highlight invalid epochs as hashed area on top of all rects
    if ("invalid" %in% legend_items$name) {
      freqtab = table(mdat$invalid)
      if (length(freqtab) > 1 || names(freqtab)[1] == "1") {
        starti = which(diff(c(0, mdat$invalid)) == 1)
        endi = which(diff(c(mdat$invalid, 0)) == -1) + 1
        newi = correctRect(starti, endi, NR = nrow(mdat), epochSize)
        t0 = mdat$timestamp[newi$starti]
        t1 = mdat$timestamp[newi$endi]
        col = legend_items$col[which(legend_items$name == "invalid")]
        y0 = 5
        y1 = 95
        transparantWhite = grDevices::adjustcolor(col = "white", alpha.f = 0.8)
        rect(xleft = t0, xright = t1, ybottom = y0, ytop = y1,
             col = transparantWhite, lwd = 0.8, border = "grey")
      }
    }
    
    # onset/wake lines:
    window_edges = which(diff(mdat$SleepPeriodTime) != 0)
    if (length(window_edges) > 0) {
      abline(v = mdat$timestamp[window_edges], col = "black", lwd = 1.5, lty = 2)
      for (wei in 1:length(window_edges)) {
        # onset and wake
        if (mdat$SleepPeriodTime[window_edges[wei]] == 1) {
          toptext = paste0("wake-up ", weekdays(mdat$timestamp[window_edges[wei]]))
        } else {
          toptext = "onset"  
        }
        mtext(text = toptext, side = 3, line = 0, cex = 0.5,
              at = mdat$timestamp[window_edges[wei]])
      }
    }
  }
  
  #---------------------------------------
  # Attempt to load time series directly
  # Identify correct subfolder
  
  expected_ts_path = paste0(metadatadir, "/meta/ms5.outraw/", part6_threshold_combi)
  expected_ms5raw_path = paste0(metadatadir, "/meta/ms5.outraw")
  if (dir.exists(expected_ts_path)) {
    fnames.ms5raw = dir(expected_ts_path, pattern = "[.]RData")
    N_ts_files = length(fnames.ms5raw)
    if (f1 > N_ts_files) f1 = N_ts_files # this is intentionally ms3 and not ms4, do not change!
    if (f1 == 0) f1 = N_ts_files
    
    ts_exists = ifelse(length(fnames.ms5raw) > 0, yes = TRUE, no = FALSE) 
    
    mdat = NULL
    if (ts_exists) {
      # Extract behavioural class names and codes:
      legendfiles = list.files(path = paste0(metadatadir, "/meta/ms5.outraw"),
                               pattern = "codes", full.names = TRUE)
      df = file.info(legendfiles)
      legendfiles = rownames(df)[which.max(df$mtime)]
      legendF = read.csv(rownames(df)[which.max(df$mtime)])
      BCN = legendF$class_name # behavioural class names (characters)
      BCC = legendF$class_id # behavioural class codes (numeric)

      
      neworder = c(grep(pattern = "sleep", x = BCN), grep(pattern = "IN", x = BCN),
                   grep(pattern = "LIG", x = BCN), grep(pattern = "MOD", x = BCN),
                   grep(pattern = "VIG", x = BCN), grep(pattern = "MVPA", x = BCN)) 
      BCN = BCN[neworder]
      BCC = BCC[neworder]
      # class2remove = grep(pattern = "spt_wake", x = BCN, invert = FALSE, value = FALSE)
      # BCN = BCN[-class2remove]
      # BCC = BCC[BCC %in% BCC[class2remove] == FALSE]
      BCN = gsub(pattern = "day_|spt_", replacement = "", x = BCN)
      BCN = gsub(pattern = "sleep", replacement = "spt_sleep", x = BCN)
      BCN = gsub(pattern = "wake_IN", replacement = "spt_wake_inactive", x = BCN)
      BCN = gsub(pattern = "wake_LIG", replacement = "spt_wake_lipa", x = BCN)
      BCN = gsub(pattern = "wake_MOD", replacement = "spt_wake_moderate", x = BCN)
      BCN = gsub(pattern = "wake_VIG", replacement = "spt_wake_vigorous", x = BCN)
      BCN = tolower(BCN)
      BCN = gsub(pattern = "lig_", replacement = "lipa_", x = BCN)
      BCN = gsub(pattern = "in_bts", replacement = "inactive_bts", x = BCN)
      BCN = gsub(pattern = "in_unbt", replacement = "inactive_unbt", x = BCN)
      # move unbouted to the end for logical order
      neworder = c(grep(pattern = "unbt", x = BCN, invert = TRUE),
                   grep(pattern = "unbt", x = BCN)) 
      BCN = BCN[neworder]
      BCC = BCC[neworder]
      # loop through files
      for (i in f0:f1) {
        load(file = paste0(metadatadir, "/meta/ms5.outraw/",
                           part6_threshold_combi, "/", fnames.ms5raw[i]))
        if (length(mdat) == 0) next
        if (nrow(mdat) == 0) next
        names(mdat)[which(names(mdat) == "sibdetection")] = "sib"
        names(mdat)[which(names(mdat) == "invalidepoch")] = "invalid"
        mdat$sib[which(mdat$SleepPeriodTime == 1)] = 0
        if (i == f0) {
          ylabels_plot2 = NULL
          if ("selfreported" %in% colnames(mdat) &&
              !is.null(params_sleep) &&
              !is.null(params_sleep[["loglocation"]])) {
            ylabels_plot2 = c("nap", "nonwear", "sleeplog")
          }
          binary_vars = c("SleepPeriodTime", "sibdetection", "invalidepoch")
          ylabels_plot2 = c(binary_vars, ylabels_plot2)
          ylabels_plot2 = gsub(pattern = "invalidepoch", replacement = "invalid", x = ylabels_plot2)
          ylabels_plot2 = gsub(pattern = "SleepPeriodTime", replacement = "spt", x = ylabels_plot2)
          ylabels_plot2 = gsub(pattern = "sibdetection", replacement = "sib", x = ylabels_plot2)
          ylabels_plot2 = gsub(pattern = "nap", replacement = "diary_nap", x = ylabels_plot2)
          ylabels_plot2 = gsub(pattern = "nonwear", replacement = "diary_nonwear", x = ylabels_plot2)
          ylabels_plot2 = gsub(pattern = "sleeplog", replacement = "diary_sleepwindow", x = ylabels_plot2)
          ylabels_plot2 = tolower(ylabels_plot2)
        }
        if ("selfreported" %in% colnames(mdat)) {
          levels(mdat$selfreported) <- c(levels(mdat$selfreported), "diary_sleepwindow", "diary_nap", "diary_nonwear")
          mdat$selfreported[which(mdat$selfreported == "sleeplog")] = "diary_sleepwindow"
          mdat$selfreported[which(mdat$selfreported == "nap")] = "diary_nap"
          mdat$selfreported[which(mdat$selfreported == "nonwear")] = "diary_nonwear"
        }

        if ("lightpeak" %in% colnames(mdat)) {
          lux_available = TRUE
        } else {
          lux_available = FALSE
        }
        
        if ("step_count" %in% colnames(mdat)) {
          step_count_available = TRUE
        } else {
          step_count_available = FALSE
        }
        
        epochSize = diff(mdat$timenum[1:2])
        
        # Define legend
        legend_items = list(col = NULL, name = NULL, code = NULL, level = NULL)
        gen_col_names = function(BCN, BCC = NULL, name, legend_items, colour = NULL,
                                 level = NULL, reverse = TRUE) {
          vars = grep(pattern = name, x = BCN, value = TRUE)
          Nitems = length(vars)
          if (Nitems > 0) {
            legend_items$name = c(legend_items$name, vars)
            legend_items$level = c(legend_items$level, rep(level, Nitems))
            if (!is.null(BCC)) {
              legend_items$code = c(legend_items$code, BCC[which(BCN %in% vars)])
            } else {
              legend_items$code = c(legend_items$code, rep(-1, Nitems))
            }
            col = rep(colour, Nitems)
            for (ci in 1:Nitems) {
              col[ci] = grDevices::adjustcolor(col = col[ci],
                                               alpha.f = 0.2 + (ci / Nitems) * 0.8) #1 / (ci + 0.4)
            }
            if (reverse == TRUE) col = rev(col)
            legend_items$col = c(legend_items$col, col)
          }
          return(legend_items)
        }
        # Sleep diary
        legend_items = gen_col_names(ylabels_plot2, name = "diary",
                                     legend_items = legend_items,
                                     colour = "gray23", level = 1, reverse = TRUE) #"#222255"
        # SIB (day time)
        legend_items$col = c(legend_items$col, "#56B4E9") #"#D55E00""#E69F00") "#56B4E9"
        legend_items$name = c(legend_items$name, "sib")
        legend_items$code = c(legend_items$code, -1)
        legend_items$level = c(legend_items$level, 2)
        
        legend_items$col = c(legend_items$col, "blue3") # "#D55E00""#E69F00") "#56B4E9" "#0072B2"
        legend_items$name = c(legend_items$name, "nap")
        legend_items$code = c(legend_items$code, -1)
        legend_items$level = c(legend_items$level, 2)
        
        # Sleep in SPT
        legend_items = gen_col_names(BCN, BCC = BCC, name = "spt_sleep", #sleep_in_spt
                                     legend_items = legend_items, colour = "white", #"#F0E442"
                                     level = 2)
        # Wake in SPT
        legend_items = gen_col_names(BCN, BCC = BCC, name = "spt_wake", #sleep_in_spt
                                     legend_items = legend_items, colour = "yellow3",
                                     level = 2, reverse = FALSE)
        # Inactivity
        not_spt = grep(pattern = "spt", x = BCN, invert = TRUE)
        legend_items = gen_col_names(BCN = BCN[not_spt], BCC = BCC[not_spt], name = "inactive",
                                     legend_items = legend_items, colour = "#CC79A7",
                                     level = 2, reverse = FALSE)
        # LIPA
        legend_items = gen_col_names(BCN = BCN[not_spt], BCC = BCC[not_spt], name = "lipa",
                                     legend_items = legend_items, colour = "#009E73",
                                     level = 2, reverse = FALSE)
        # MVPA
        legend_items = gen_col_names(BCN = BCN[not_spt], BCC = BCC[not_spt], name = "mod|vig|mvpa",
                                     legend_items = legend_items, colour = "#D55E00",
                                     level = 2, reverse = FALSE)
        # Invalid
        legend_items$col = c(legend_items$col, "grey") # ""
        legend_items$name = c(legend_items$name, "invalid")
        legend_items$code = c(legend_items$code, -1)
        legend_items$level = c(legend_items$level, 0)
        
        
        simple_filename = gsub(pattern = ".RData", replacement = "", x = fnames.ms5raw[i] ) #"patientID12345"
        pdf(paste0(metadatadir, "/results/file summary reports/Time_report_",
                   simple_filename, ".pdf"), paper = "a4",
            width = 0, height = 0)
        
        NhoursPerRow = 36
        midnightsi = which(format(mdat$timestamp, "%H") == "00" &
                             format(mdat$timestamp, "%M") == "00" &
                             format(mdat$timestamp, "%S") == "00")
        subploti = c(1, midnightsi + 1)
        subploti = cbind(subploti,
                         c(midnightsi + (NhoursPerRow - 24) * 60, nrow(mdat)))
        
        invalid = which(mdat$invalidepoch == 1)
        subploti[which(subploti[,2] > nrow(mdat)), 2] = nrow(mdat)
        
        NdaysPerPage = 8
        par(mfrow = c(NdaysPerPage, 1), mgp = c(2, 0.8, 0), omi = c(0, 0, 0, 0), bty = "n")
        if (nrow(subploti) > 0) {
          for (ani in 1:nrow(subploti)) {
            if (ani == 1 || round(ani / NdaysPerPage) == (ani / NdaysPerPage)) {
              
              par(mar = c(3, 0, 1.5, 2.5))
              plot.new()
              # Top plot plot
              legendnames = legend_items$name
              legendnames[which(legendnames == "sib")] = "no movement (sib daytime)"
              legendnames[which(legendnames == "nap")] = "nap"
              legendnames[which(legendnames == "sleep_in_spt")] = "sleep"
              legendnames = gsub(pattern = "_", replacement = " ", x = legendnames)
              boutvars = grep(pattern = "bts", x = legendnames, value = FALSE)
              for (bvi in 1:length(boutvars)) {
                tmp_split = unlist(strsplit(legendnames[boutvars[bvi]], " "))
                if (length(tmp_split) == 3) {
                  legendnames[boutvars[bvi]] = paste0(tmp_split[1], " ", tmp_split[2], " >=", tmp_split[3])
                } else {
                  legendnames[boutvars[bvi]] = paste0(tmp_split[1], " ",tmp_split[2], " [", tmp_split[3], ",", tmp_split[4], ")")
                }
                
              }
              legendnames[boutvars] = paste0(legendnames[boutvars], " mins")
              legendcolors = legend_items$col
              # legenddensity = rep(-1, length(legendnames))
              # legenddensity[] = 20
              # Nitems = length(legendnames)
              # lty = rep("p", Nitems)
              # pch = rep(15, Nitems)
              # lty[which(legendnames == "invalid")] = "l"
              # pchlty[which(legendnames == "invalid")] = "l"p
              
              not_invalid = which(legendnames != "invalid")
              legendpch = rep(15, length(legendnames))
              legendpch[which(legendnames == "invalid")] = 0
              legendcolors[which(legendnames == "invalid")] = "grey" #"white"
              legendnames[which(legendnames == "invalid")] = "ignored/imputed"
              legend("topright", legend = legendnames,
                     col = legendcolors, 
                     ncol = length(legend_items$name) %/% 6 + 1, cex = 0.8, 
                     pch = legendpch, pt.cex = 2, bty = "n", title = "Legend:",
                     title.font = 2, title.adj = 0)
              if (is.null(GGIRversion)) GGIRversion = "Not identified"
              
              RecDuration = round((nrow(mdat) * epochSize) / (24 * 3600), digits = 1)
              RecDurUnit = "days"
              if (RecDuration < 1) {
                RecDuration * 24
                RecDurUnit = "hours"
              } 
              if (desiredtz == "") desiredtz = Sys.timezone()
              
              if (nchar(simple_filename) > 10) {
                added_text1 = "Start of filename: "
                added_text2 = "..."
              } else {
                added_text1 = "Filename: "
                added_text2 = ""
                
              }
              legend("topleft", legend = c(paste0(added_text1,
                                                  substr(x = simple_filename, start = 1, stop = 10), added_text2), 
                                           paste0("Start date: ", as.Date(mdat$timestamp[1])),
                                           paste0("Duration: ", RecDuration, " ", RecDurUnit),
                                           paste0("GGIR version: " , GGIRversion),
                                           paste0("Timezone: ", desiredtz)),
                     cex = 0.9, 
                     bty = "n", title = "Info:",
                     title.font = 2, title.adj = 0)
              
              mtext(text = "Accelerometer report generated by R package GGIR",
                    side = 3, line = 0.2, cex = 1.2, font = 2)
              
            }
            if (subploti[ani, 2] - subploti[ani, 1] > 60) { # we need at least 1 hour for the ticks
              panelplot(mdat[(subploti[ani, 1] + 1):subploti[ani, 2], ],
                        ylabels_plot2, binary_vars,
                        BCN, BCC, title = "", NhoursPerRow = NhoursPerRow, plotid = ani,
                        legend_items = legend_items, lux_available = lux_available,
                        step_count_available = step_count_available, epochSize = epochSize)
            }
            
          }
        }
        dev.off()
      }
    }
  }
}
g.plot5 = function(metadatadir = c(), dofirstpage = FALSE, viewingwindow = 1,
                   f0 = c(), f1 = c(), overwrite = FALSE,
                   metric = "ENMO", desiredtz = "", threshold.lig = 30,
                   threshold.mod = 100, threshold.vig = 400,
                   visualreport_without_invalid = TRUE,
                   includedaycrit = 0.66, includenightcrit = 0.66,
                   verbose = TRUE) {
  if (file.exists(paste0(metadatadir, "/results/file summary reports"))) {
    fnames.fsr = sort(dir(paste0(metadatadir, "/results/file summary reports")))
    ffdone = fnames.fsr #ffdone is now a vector of filenames that have already been processed by g.part5
  } else {
    dir.create(file.path(paste0(metadatadir, "/results"), "file summary reports"))
    ffdone = c()
  }
  N_milestone_data_p3 = length(dir(paste0(metadatadir,"/meta/ms3.out")))
  if (f1 > N_milestone_data_p3) f1 = N_milestone_data_p3 # this is intentionally ms3 and not ms4, do not change!
  if (f1 == 0) f1 = N_milestone_data_p3
  # directories
  tail_expansion_log = NULL
  ms1dir = paste0(metadatadir, "/meta/basic")
  ms2dir = paste0(metadatadir,"/meta/ms2.out")
  ms3dir = paste0(metadatadir, "/meta/ms3.out")
  ms4dir = paste0(metadatadir, "/meta/ms4.out")
  results = paste0(metadatadir, "/results")
  # names of files in those directories
  fname_ms1 = dir(ms1dir)
  fname_ms3 = dir(ms3dir)
  fname_ms2 = dir(ms2dir)
  fname_ms4 = dir(ms4dir)
  removeRDa = function(x) as.character(unlist(strsplit(x,".RDa")))[1]
  fname_ms1_stripped = apply(as.matrix(as.character(fname_ms1)), MARGIN = c(1), FUN = removeRDa)
  removeEta = function(x) as.character(unlist(strsplit(x, "eta_")))[2]
  fnames_ms1_stripped = apply(as.matrix(as.character(fname_ms1_stripped)), MARGIN = c(1), FUN = removeEta) # part 2 milestone data files
  fnames_ms3_withoutRDa = apply(as.matrix(as.character(fname_ms3)), MARGIN = c(1), FUN = removeRDa) # part 3 milestone data files
  # create list of day names
  wdaynames = c("Sunday", "Monday", "Tuesday", "Wednesday",
                "Thursday", "Friday", "Saturday")
  checkfiles = dir(results)
  ## load summary spreadsheets for this study (no longer used as we now use milestone data)
  #if (!file.exists(paste0(results, "/part2_daysummary.csv"))) {
  # stop("Warning: File part2_daysummary.csv not generated yet")
  #}
  #P2daysummary = read.csv(paste0(results, "/part2_daysummary.csv"))
  
  M = c()
  
  if (f1 - f0 > 50 & verbose == TRUE) {
    cat(paste0("\nGGIR is now creating a visualreport (pdf) for each of the ", f1 - f0,
               " recordings as a final step in the GGIR pipeline. This can take a while as",
               " it is done file-by-file. If you do not want the visualreports then you",
               " can kill the process (Ctrl-C or ESC) as that will not affect the",
               " rest of the analyses and reports. To avoid this next time,",
               " set parameter 'old_visualreport' to FALSE to only derive the new",
               " reports. Alternatively, set 'visualreport' to FALSE which", 
               " suppresses both report types."))
  }
  # loop through files
  for (i in f0:f1) {
    if (length(ffdone) > 0) {
      if (length(which(ffdone == paste0("Report_", fnames_ms3_withoutRDa[i], ".pdf"))) > 0) {
        skip = 1 #skip this file because it was analysed before")
      } else {
        skip = 0 #do not skip this file
      }
    } else {
      skip = 0
    }
    if (overwrite == TRUE) skip = 0
    if (skip == 0) {
      sel = which(fnames_ms1_stripped == fnames_ms3_withoutRDa[i])
      ms2_file_index = which(fname_ms2 == fname_ms2[i])
      ms2_filepath = paste(ms2dir, "/", fname_ms2[ms2_file_index], sep = "")
      P2daysummary_tmp = IMP = SUM = NULL
      load(ms2_filepath)
      P2daysummary_tmp = SUM$daysummary
      P2daysummary_tmp$measurementday = as.numeric(P2daysummary_tmp$measurementday)
      if (length(sel) > 0) {
        ms4_file_index = which(fname_ms4 == fname_ms3[i])
        if (length(ms4_file_index) == 1) {
          nightsummary = c()
          ms4_filepath = paste0(ms4dir, "/", fname_ms4[ms4_file_index])
          load(ms4_filepath) #to load summary sleep
          nightsummary$night = as.numeric(nightsummary$night)
          summarysleep_tmp = nightsummary
        } else {
          warning(
            paste0("\nVisual report not generated for ",
                   fnames_ms1_stripped[sel], " because part 4 output was not available."
            )
          )
          next
        }
       
        sib.cla.sum = c()
        load(paste0(ms3dir,"/",fname_ms3[i]))
        load(paste0(ms1dir,"/",fname_ms1[sel]))
        ws3 = M$windowsizes[1]
        ws2 = M$windowsizes[2]
        if (nrow(sib.cla.sum) == 0) next
        pdf(paste0(metadatadir, "/results/file summary reports/old_report_",
                   fnames_ms1_stripped[sel], ".pdf"), paper = "a4",
            width = 0, height = 0)
        
        # P2daysummary_tmp = P2daysummary[which(P2daysummary$filename == fnames_ms1_stripped[sel]),]
        # note that the reports are generated from the raw sleep classification (part4) and no attempt is made to clean it up,
        # by deleting nights for which no diary was available or not enough accelerometer data was available
        if (length(unique(summarysleep_tmp$acc_def)) > 1) {
          if (length(which(unique(summarysleep_tmp$acc_def) == "T5A5")) == 1) {
            della = which(summarysleep_tmp$acc_def == "T5A5")
          } else {
            della = which(summarysleep_tmp$acc_def == unique(summarysleep_tmp$acc_def)[1])
          }
          if (length(della) > 0) summarysleep_tmp = summarysleep_tmp[-della,]
        }
        threshold_hrs_of_data_per_day = 0.5
        if (visualreport_without_invalid == TRUE) {
          incrementNight = 0
          # do not include days with no meaningful data
          if (includedaycrit < 1) includedaycrit = includedaycrit * 24
          if (includenightcrit > 1) includenightcrit = includenightcrit / 24
          d2excludeb = d2exclude = which(P2daysummary_tmp$`N valid hours` < max(c(includedaycrit,
                                                                                  threshold_hrs_of_data_per_day)))
          n2excludeb = n2exclude = which(summarysleep_tmp$fraction_night_invalid > (1 - includenightcrit)
                                         | summarysleep_tmp$SptDuration == 0)
          
          if (length(d2exclude) > 0) {
            d2excludeb = P2daysummary_tmp$measurementday[d2exclude]
            P2daysummary_tmp = P2daysummary_tmp[-d2exclude,] #ignore days with non-wear
            d2exclude = d2excludeb
          }
          if (length(n2exclude) > 0) {
            n2excludeb = summarysleep_tmp$night[n2exclude]
            summarysleep_tmp = summarysleep_tmp[-n2exclude,]
            n2exclude = n2excludeb
          }
          # nights missing in sleep summary?
          if (length(summarysleep_tmp$night) > 0) {
            allnights = 1:max(summarysleep_tmp$night)
            missingNights = which(allnights %in% summarysleep_tmp$night == FALSE)
          } else {
            missingNights = NULL
          }
          if (length(missingNights) > 0) {
            n2exclude = sort(unique(c(n2exclude, missingNights)))
          }
          # Account for shift in nights relative to windows
          if (nrow(P2daysummary_tmp) > 0) {
            # nights missing in sleep summary?
            allnights = 1:max(summarysleep_tmp$night)
            missingNights = which(allnights %in% summarysleep_tmp$night == FALSE)
            if (length(missingNights) > 0) {
              n2exclude = sort(unique(c(n2exclude, missingNights)))
            }
            if (P2daysummary_tmp$`N hours`[1] < 24 & P2daysummary_tmp$`N hours`[1] > 12 & length(n2exclude) > 0) {
              # First calendar day is between 12 and 24 hours
              # this means that the first night viewindow (=2) may include some data but
              # is not reflected by the sleep reportswindow
              # so our night counts should be increased by one
              n2excludeb = n2exclude = n2exclude + 1
              incrementNight = 1
            }
          }
          
          if (length(tail_expansion_log) != 0) { # then keep timing of sleeponset to plot
            lastnight = max(summarysleep_tmp$night) + incrementNight
            if (lastnight %in% n2exclude) {
              n2exclude = n2exclude[-which(n2exclude == lastnight)]
            }
          }
        } else {
          n2exclude = d2exclude = NULL
        }
        # detect which column is mvpa
        n45 = names(P2daysummary_tmp)
        varsVPA = grep(pattern = "VPA",x = n45)
        vars02 = grep(pattern = "_0-2|_0.2|_24",x = n45)
        c45 = varsVPA[which(varsVPA %in% vars02 == TRUE)]
        c45 = c45[length(c45)]
        #######################
        # First page of the report
        MainMetric_1 = paste0("mean_", metric, "_mg_24hr")
        MainMetric_2 = paste0("mean_", metric, "_mg_0-24hr")
        MainMetric_3 = paste0("mean_", metric, "_mg_0.24hr")
        MainMetric = NULL
        if (length(which(colnames(P2daysummary_tmp) == MainMetric_1)) == 1) {
          MainMetric = MainMetric_1
        } else if (length(which(colnames(P2daysummary_tmp) == MainMetric_2)) == 1) {
          MainMetric = MainMetric_2
        } else if (length(which(colnames(P2daysummary_tmp) == MainMetric_3)) == 1) {
          MainMetric = MainMetric_3
        }
        if (is.null(MainMetric) == TRUE) {
          dofirstpage = FALSE
        } else {
          if (length(which(is.na(P2daysummary_tmp[,MainMetric]) == FALSE)) <= 1) {
            dofirstpage = FALSE
          }
        }
        if (dofirstpage == TRUE & length(which(is.na(P2daysummary_tmp[,c45]) == FALSE)) > 1
            & nrow(summarysleep_tmp) > 0) {
          # abbreviate names of days
          days1 = P2daysummary_tmp$weekday
          daynames = as.character(days1)
          days_PA = g.abr.day.names(daynames)
          days2 = summarysleep_tmp$weekday
          daynames = as.character(days2)
          days_SLEEP = g.abr.day.names(daynames)
          # extract variables
          lengthnight = summarysleep_tmp$SptDuration #including wake periods
          nocsleepdur = summarysleep_tmp$SleepDurationInSpt
          sleepefficiency = (nocsleepdur / lengthnight) * 100
          f01 = P2daysummary_tmp[,c45]
          f02 = P2daysummary_tmp[,MainMetric]
          f05 = matrix(NA, nrow = 2, ncol = length(summarysleep_tmp$SptDuration))
          f05[1,] = summarysleep_tmp$SleepDurationInSpt
          f05[2,] = summarysleep_tmp$SptDuration - summarysleep_tmp$SleepDurationInSpt
          f05_2 = summarysleep_tmp$SptDuration
          f06 = sleepefficiency
          # f07 = P2daysummary_tmp$N.valid.hours # Previously needed when reading csv-report Part 2
          f07 = P2daysummary_tmp$`N valid hours`
          # allocate colours
          CLS = c("white", "black")
          CLS_A = rep(CLS[1], length(days_PA))
          CLS_B = rep(CLS[1], length(days_SLEEP))
          CLS_A[which(days_PA == "SUN" | days_PA == "SAT")] = CLS[2]
          CLS_B[which(days_SLEEP == "SUN" | days_SLEEP == "SAT")] = CLS[2]
          # headers
          vars = c(paste0("Time spent in moderate or vigorous activity (average is ",
                          round(mean(f01, na.rm = TRUE)), " minutes per day)"),
                   paste0("Total physical activity (average per day is ",
                          round(mean(f02, na.rm = TRUE)), " mg)"),
                   paste0("Sleep period time (average is ",
                          round(mean(f05_2, na.rm = TRUE), digits = 1), " hours per night)"),
                   paste0("Sleep efficiency (average is ", round(mean(f06, na.rm = TRUE)), "% per night)"),
                   paste0("Duration monitor worn (hours per day)"))
          # plot data
          CEXN = 0.9
          par(mfrow = c(5, 1),
              omi = c(0, 0, 0.2, 0),
              mar = c(3, 2, 2, 2) + 0.1)
          #MVPA
          YXLIM = c(0, (max(f01, na.rm = TRUE) * 1.3))
          if (YXLIM[2] == 0) YXLIM[2] = 60
          B3 = barplot(as.matrix(f01), names.arg = days_PA, beside = TRUE,
                       ylim = YXLIM, cex.names = CEXN, las = 0, col = CLS_A, density = 20)
          abline(h = 30, lty = 2, lwd = 2)
          topp = mean(as.matrix(round(f01))) * 0.1
          text(
            y = as.matrix(round(f01)) + topp + 5,
            x = B3,
            labels = as.character(as.matrix(round(f01))), xpd = TRUE, cex = 1
          )
          text(
            x = 1,
            y = (max(YXLIM) * 0.95),
            labels = vars[1], pos = 4, font = 2, cex = 1.2
          )
          #Acceleration
          YXLIM = c(0, (max(f02, na.rm = TRUE) * 1.3))
          B6 = barplot(as.matrix(f02), names.arg = days_PA, beside = TRUE,
                       ylim = YXLIM, cex.names = CEXN, las = 0,
                       col = CLS_A, density = 20)
          abline(h = 25, lty = 2, lwd = 2)
          topp = mean(as.matrix(round(f02))) * 0.1
          text(
            y = as.matrix(round(f02)) + topp,
            x = B6,
            labels = as.character(as.matrix(round(f02))), xpd = TRUE, cex = 1
          )
          text(
            x = 1,
            y = (max(YXLIM) * 0.95),
            labels = vars[2], pos = 4, font = 2, cex = 1.2
          )
          #Monitor wear duration
          YXLIM = c(0, (max(f07, na.rm = TRUE) * 1.3))
          B8 =  barplot(
            as.matrix(f07),
            names.arg = days_PA,
            beside = TRUE,
            ylim = YXLIM,
            cex.names = CEXN,
            las = 0,
            col = CLS_A,
            density = 20
          )
          topp = mean(as.matrix(round(f07))) * 0.1
          text(
            y = as.matrix(round(f07)) + topp,
            x = B8,
            labels = as.character(as.matrix(round(f07))),
            xpd = TRUE, cex = 1.1
          )
          abline(h = 16, lty = 2, lwd = 2)
          text(
            x = 1,
            y = (max(YXLIM) * 0.95),
            labels = vars[5], pos = 4, font = 2, cex = 1.2
          )
          #Sleep duration
          night_sleep_col <- rgb(0.8, 0.8, 0.8, alpha = 0.3)
          night_wake_col <- rgb(0, 0.8, 0.6, alpha = 0.2)
          space_vec = rep(0, length(f05_2))
          YXLIM = c(0, (max(f05_2, na.rm = TRUE) * 1.3))
          B4 = barplot(as.matrix(f05), names.arg = days_SLEEP,
                       ylim = YXLIM, las = 0,
                       col = c(night_sleep_col, night_wake_col),
                       space = space_vec)
          legend(
            "bottomleft",
            c('Nocturnal Wake', 'Sleep'),
            fill = c(night_wake_col, night_sleep_col),
            bg = 'white'
          )
          abline(h = 6, lty = 2, lwd = 2)
          topp = mean(as.matrix(round(f05_2, digits = 1))) * 0.1
          text(
            y = as.matrix(round(f05_2, digits = 1)) + topp,
            x = B4,
            labels = as.character(as.matrix(round(f05_2, digits = 1))),
            xpd = TRUE, cex = 1
          )
          # not sure why I had to change x to 0 (from 1) in the following line:
          text(
            x = 0,
            y = (max(YXLIM) * 0.95),
            labels = vars[3],
            pos = 4, font = 2, cex = 1.2
          )
          #Sleep efficiency
          YXLIM = c(0, 120)
          B5 = barplot(as.matrix(f06), names.arg = days_SLEEP, beside = TRUE,
                       ylim = YXLIM, cex.names = CEXN, las = 0, col = CLS_B, density = 20)
          abline(h = 60, lty = 2, lwd = 2)
          abline(h = 100,lty = 3, lwd = 1)
          topp = mean(as.matrix(round(f06))) * 0.1
          text(
            y = as.matrix(round(f06)) + topp,
            x = B5,
            labels = as.character(as.matrix(round(f06))), xpd = TRUE, cex = 1
          )
          text(
            x = 1,
            y = (max(YXLIM) - 10),
            labels = vars[4], pos = 4, font = 2, cex = 1.2
          )
          #-----------------------------------------------------------------------------------
          mtext(
            paste0("Activity and sleep report:", fnames_ms1_stripped[sel]),
            side = 3, line = 0, outer = TRUE, font = 2, cex = 0.7
          )
        }
        LWDX = 2.5 #linewidth for coloured lines
        LWDA = 0.2
        BLX = 0.6
        #=================================================
        # Next pages with day specific graphs
        # get variables - activity:
        ACC = as.numeric(as.matrix(M$metashort[, metric])) * 1000
        nonwearscore = as.numeric(as.matrix(M$metalong[,"nonwearscore"]))
        time =  as.character(M$metashort[,1])
        nw_time = as.character(M$metalong[,1])
        if (length(unlist(strsplit(time[1],"T"))) > 1) { # ISO timestamp format
          time = format(iso8601chartime2POSIX(time, desiredtz))
          nw_time = format(iso8601chartime2POSIX(nw_time, desiredtz))
        }
        time_unclassed = unclass(as.POSIXlt(time,desiredtz))
        sec = time_unclassed$sec
        min_vec = time_unclassed$min
        hour = time_unclassed$hour
        NONWEAR = IMP$r5long
        INACT = LIGPA = MODPA = VIGPA = rep(NA, length(ACC))  # PA vectors for plotting
        
        # Find bouts of light-PA (LPA):
        boutdur2 = 10 * (60/ws3)    # 10min bout duration
        boutcriter = 0.8            # 80% bout criteria
        rr_lpa = matrix(0,length(ACC),1)
        p_lpa = which(ACC >= threshold.lig)
        rr_lpa[p_lpa] = 1
        rr1t = rr_lpa
        jlpa = 1
        while (jlpa <= length(p_lpa)) {
          endi = p_lpa[jlpa] + boutdur2
          if (endi <= length(rr_lpa)) { #does bout fall without measurement?
            lengthbout = sum(rr_lpa[p_lpa[jlpa]:endi])
            if (lengthbout > (boutdur2*boutcriter)) { #0.8 => 80% of the bout needs to meet the criteria
              rr1t[p_lpa[jlpa]:endi] = 2 #remember that this was a bout in r1t
            } else {
              rr_lpa[p_lpa[jlpa]] = 0
            }
          } else { #bout does not fall within measurement
            if (length(p_lpa) > 1 & jlpa > 2) {
              rr_lpa[p_lpa[jlpa]] = rr_lpa[p_lpa[jlpa - 1]]
            }
          }
          jlpa = jlpa + 1
        }
        rr_lpa[which(rr1t == 2)] = 1
        LIGPA[which(rr_lpa == 1)] = 1 #moderate as part of 10 minute bouts of lpa
        # Find bouts of MVPA
        rr = matrix(0,length(ACC),1)
        p = which(ACC >= threshold.mod)
        rr[p] = 1
        rr1t = rr
        jmvpa = 1
        while (jmvpa <= length(p)) {
          endi = p[jmvpa] + boutdur2
          if (endi <= length(rr)) { #does bout fall without measurement?
            lengthbout = sum(rr[p[jmvpa]:endi])
            if (lengthbout > (boutdur2 * boutcriter)) { #0.9 => 90% of the bout needs to meet the criteria
              rr1t[p[jmvpa]:endi] = 2 # remember that this was a bout in r1t
            } else {
              rr[p[jmvpa]] = 0
            }
          } else { # bout does not fall within measurement
            if (length(p) > 1 & jmvpa > 2) {
              rr[p[jmvpa]] = rr[p[jmvpa - 1]]
            }
          }
          jmvpa = jmvpa + 1
        }
        rr[which(rr1t == 2)] = 1
        # moderate as part of 10 minute bouts of mvpa
        MODPA[which(rr == 1 & ACC < threshold.vig)] = 1
        # vigorous as part of 10 minute bouts of mvpa
        VIGPA[which(ACC >= threshold.vig & rr == 1)] = 1
        # cancel LPA sections that are MVPA
        LIGPA[which(MODPA == 1 | VIGPA == 1)] <- NA
        # create inactive sections that are NA on LPA/MPA/VPA
        INACT[which(is.na(LIGPA) & is.na(MODPA) & is.na(VIGPA))] <- 1
        #get variables - sleep:
        angle = as.matrix(M$metashort[,which(colnames(M$metashort) == "anglez")])
        detection = rep(NA,length(time))
        S = sib.cla.sum
        S$sib.onset.time = iso8601chartime2POSIX(S$sib.onset.time, tz = desiredtz)
        S$sib.end.time = iso8601chartime2POSIX(S$sib.end.time, tz = desiredtz)
        def = unique(S$definition)[1]
        S = S[which(S$definition == def),] # simplify to one definition
        for (j in 1:max(unique(S$night))) {
          tmp = S[which(S$night == j),]
          for (h in 1:nrow(tmp)) { # sleep periods
            s0 = which(time == format(tmp$sib.onset.time[h]))[1]
            s1 = which(time == format(tmp$sib.end.time[h]))[1]
            if (length(s0) == 0 | is.na(s0) == TRUE) {
              s0 = which(time == paste0(format(tmp$sib.onset.time[h])," 00:00:00"))[1]
            }
            if (length(s1) == 0 | is.na(s1) == TRUE) {
              s1 = which(time == paste0(format(tmp$sib.end.time[h]), " 00:00:00"))[1]
            }
            if (is.na(s0) == FALSE & is.na(s1) == FALSE) {
              detection[s0:s1] = 1
            }
          }
        }
        night_wake_full <- rep(NA,length(detection)) # opposite of detection (sleep/inactivity detection)
        night_wake_full[is.na(detection)] <- 1 # for plotting wake during SPT window
        # prepare to search for sleeplog_onst / sleeplog_wake
        sleep_dates = as.Date(summarysleep_tmp$calendar_date,
                              format = '%d/%m/%Y',
                              origin = "1970-1-1")
        # detect midnights
        if (viewingwindow == 1) {
          nightsi = which(sec == 0 & min_vec == 0 & hour == 0)
          xaxislabels = c("midnight","2am", "4am", "6am", "8am", "10am", "noon",
                          "2pm","4pm","6pm","8pm","10pm","midnight")
        } else if (viewingwindow == 2) {
          nightsi = which(sec == 0 & min_vec == 0 & hour == 12)
          xaxislabels = c("noon","2pm", "4pm", "6pm", "8pm", "10pm", "midnight",
                          "2am", "4am", "6am", "8am", "10am", "noon")
        }
        if (length(nightsi) > 0 && nrow(summarysleep_tmp) > 0 && nrow(P2daysummary_tmp)) {
          # Do not attempt to create a plot when there is no midnight in the data,
          # because calculation of t1 will be complicated.
          nplots = length(nightsi) + 1
          if (visualreport_without_invalid == TRUE) {
            if (viewingwindow == 2) {
              nplots = min(c(nplots, max(summarysleep_tmp$night) + incrementNight))
            } else {
              nplots = min(c(nplots, max(P2daysummary_tmp$measurementday)))
            }
          }
          # plot
          npointsperday = (60/ws3)*1440
          abcis = 1:npointsperday
          abcisL = length(abcis)
          NGPP = 7 #number of graphs per page
          par(
            mfcol = c(NGPP, 1),
            mar = c(2, 0.5, 2, 0.5) + 0.1,
            omi = c(0, 0, 0.3, 0),
            mgp = c(2, 0.8, 0)
          )
          daycount = 1
          for (g in 1:nplots) {
            skip = FALSE
            if (g == 1) {
              t0 = 1
              t1 = nightsi[g] - 1
              if ((t1 - t0) < (threshold_hrs_of_data_per_day * (60 / ws3) * 60)) {
                skip = TRUE # regardless of inclusion criterion only plot windows with at least half an hour of data
              }
            } else if (g > 1 & g < nplots) {
              t0 = nightsi[g - 1]
              t1 = nightsi[g] - 1
            }  else if (g == nplots) {
              t0 = nightsi[g - 1]
              t1 = min(c(length(time), t0 + (24 * 60 * (60 / ws3)) - 1))
              if ((t1 - t0) < (threshold_hrs_of_data_per_day * (60 / ws3) * 60)) {
                skip = TRUE
              }
            }
            NhoursInThisDay = ((t1 - t0) + 1) / (60 * 60 / ws3)
            if (NhoursInThisDay == 25) { # day with 25 hours, pretend that 25th hour did not happen
              t1 = t1 - (3600/ws3)
            } else if (NhoursInThisDay == 23) { # day with 23 hours, extend timeline with 1 hour
              t1 = t1 + (3600/ws3)
            }
            # Initialize daily 'what we think you did' vectors:
            if (length(tail_expansion_log) != 0) {
              from = length(ACC) - tail_expansion_log$short + 1
              to = length(ACC)
              ACC[from:to] = NA; angle[from:to] = NA
            }
            acc = abs(ACC[t0:t1])
            ang = angle[t0:t1]
            # if only expanded data in this day, then no plot
            if (all(is.na(acc)) & all(is.na(ang))) next
            non_wear <- NONWEAR[t0:t1]
            annot_mat = matrix(NA, nrow = length(acc), ncol = 6)
            annot_mat[,1] <- detection[t0:t1]          # night sleep
            annot_mat[,2] <- night_wake_full[t0:t1]    # nocturnal wake
            annot_mat[,3] <- INACT[t0:t1]              # inactivity
            annot_mat[,4] <- LIGPA[t0:t1]              # light pa
            annot_mat[,5] <- MODPA[t0:t1]              # moderate pa
            annot_mat[,6] <- VIGPA[t0:t1]              # vigorous pa
            # check to see if there are any sleep onset or wake annotations on this day
            sleeponset_loc = 0
            wake_loc = 0
            if (viewingwindow == 1) {  # use different search coefficients for noon or midnight centered plots
              sw_coefs = c(0,24)
            } else if (viewingwindow == 2) {
              sw_coefs = c(12,36)
            }
            # check for sleeponset & wake time that is logged on this day before midnight
            curr_date = as.Date(substr(time[t0], start = 1, stop = 10),
                                format = '%Y-%m-%d',
                                origin = "1970-1-1")  # what day is it?
            if (viewingwindow == 2) {
              # check to see if it is the first day that has less than 24 and starts after midnight
              if ((t1 - t0) < ((60*60*12)/ws3)) { # if there is less than half a days worth of data
                curr_date = curr_date - 1
              }
            }
            check_date = match(curr_date, sleep_dates)
            if (is.na(check_date) == FALSE) {
              sleeponset_time = summarysleep_tmp$sleeponset[check_date]  # get the time of sleep_onset
              if (sleeponset_time >= sw_coefs[1] & sleeponset_time < sw_coefs[2]) {
                sleeponset_hour = trunc(sleeponset_time)
                if (sleeponset_hour == 24) sleeponset_hour = 0
                if (sleeponset_hour > 24) sleeponset_hour = sleeponset_hour - 24 # only with viewingwindow==2
                sleeponset_min = round((sleeponset_time - trunc(sleeponset_time)) * 60)
                if (sleeponset_min == 60) sleeponset_min = 0
                sleeponset_locations = which(hour[t0:t1] == sleeponset_hour & min_vec[t0:t1] == sleeponset_min)
                if (!is.na(sleeponset_locations[1])) {
                  sleeponset_loc = sleeponset_locations[1]
                }
              }
              wake_time = summarysleep_tmp$wakeup[check_date]
              if (wake_time >= sw_coefs[1] & wake_time < sw_coefs[2]) {
                wake_hour = trunc(wake_time)
                if (wake_hour == 24) wake_hour = 0
                if (wake_hour > 24) {
                  wake_hour = wake_hour - 24
                }
                wake_min = round((wake_time - trunc(wake_time)) * 60)
                if (wake_min == 60) wake_min = 0
                wake_locations = which(hour[t0:t1] == wake_hour & min_vec[t0:t1] == wake_min)
                if (!is.na(wake_locations[1])) {
                  wake_loc = wake_locations[1]
                }
              }
            }
            # check for sleeponset & wake time that is logged on the previous day after midnight
            prev_date = curr_date - 1
            check_date = match(prev_date,sleep_dates)
            if (is.na(check_date) == FALSE) {
              wake_time = summarysleep_tmp$wakeup[check_date]
              if (wake_time >= sw_coefs[2]) {
                wake_hour = trunc(wake_time) - 24
                wake_min = round((wake_time - trunc(wake_time)) * 60)
                if (wake_min == 60) wake_min = 0
                wake_locations = which(hour[t0:t1] == wake_hour & min_vec[t0:t1] == wake_min)
                if (wake_loc > 0) {
                  if (!is.na(wake_locations[1])) {
                    wake_loc[2] = wake_locations[1]
                  }
                } else if (wake_loc == 0) {
                  if (!is.na(wake_locations[1])) {
                    wake_loc = wake_locations[1]
                  }
                }
              }
              # new way
              sleeponset_time = summarysleep_tmp$sleeponset[check_date]
              if (sleeponset_time >= sw_coefs[2]) {
                sleeponset_hour = trunc(sleeponset_time) - 24
                sleeponset_min = round((sleeponset_time - trunc(sleeponset_time)) * 60)
                if (sleeponset_min == 60) sleeponset_min = 0
                sleeponset_locations = which(hour[t0:t1] == sleeponset_hour & min_vec[t0:t1] == sleeponset_min)
                if (sleeponset_loc > 0) {
                  if (!is.na(sleeponset_locations[1])) {
                    sleeponset_loc[2] = sleeponset_locations[1]
                  }
                } else if (sleeponset_loc == 0) {
                  if (!is.na(sleeponset_locations[1])) {
                    sleeponset_loc = sleeponset_locations[1]
                  }
                }
              }
            }
            # if midnight centered plots, then search next day as well
            if (viewingwindow == 2) {
              next_day = curr_date + 1
              check_date = match(next_day,sleep_dates)
            }
            # add extensions if <24hr of data
            first_day_adjust = 0 # hold adjustment amounts on first and last day plots
            last_day_adjust = 0
            if (((t1 - t0) + 1) != npointsperday & t0 == 1) {
              daySize = (t1 - t0) - (floor(((t1 - t0) / npointsperday)) * npointsperday)
              extension = rep(NA, (npointsperday - (daySize + 1)))
              first_day_adjust = length(extension)
              acc = c(extension,acc)
              ang = c(extension,ang)
              non_wear = c(extension, non_wear)
              t1 = length(acc) # this was missing and causing x.y coords errors in some tests
              if (length(acc) == (abcisL + 1)) {
                extension = extension[2:(length(extension))]
                acc = acc[2:(length(acc))]
                ang = ang[2:(length(ang))]
                non_wear = non_wear[2:(length(non_wear))]
              }
              extension_mat = matrix(NA, nrow = length(extension), ncol = 6)
              annot_mat = rbind(extension_mat,annot_mat)
              # adjust any sleeponset / wake annotations if they exist:
              if (sleeponset_loc[1] != 0) {
                for (i2 in 1:length(sleeponset_loc)) {
                  sleeponset_loc[i2] = sleeponset_loc[i2] + length(extension)
                }
              }
              if (wake_loc[1] != 0) {
                for (i3 in 1:length(wake_loc)) {
                  wake_loc[i3] = wake_loc[i3] + length(extension)
                }
              }
            } else if (((t1 - t0) + 1) != npointsperday & t1 == length(time)) {
              daySize = (t1 - t0) - (floor(((t1 - t0) / npointsperday)) * npointsperday)
              extension = rep(NA, (npointsperday - (daySize + 1)))
              last_day_adjust = length(acc)
              acc = c(acc,extension)
              ang = c(ang,extension)
              non_wear = c(non_wear,extension)
              if (length(acc) == (abcisL + 1)) {
                extension = extension[1:(length(extension) - 1)]
                acc = acc[1:(length(acc) - 1)]
                ang = ang[1:(length(ang) - 1)]
                non_wear = non_wear[1:(length(non_wear) - 1)]
              }
              extension_mat = matrix(NA, nrow = length(extension), ncol = 6)
              annot_mat = rbind(annot_mat,extension_mat)
            }
            acc = as.numeric(acc)
            acc[which(acc >= 900)] = 900
            acc = (acc/9) - 210
            annot_mat[non_wear == 1, ] = 0   # no annotations in non-wear periods:
            sleep_mat = annot_mat[, 1:2]  # split annotation matrix
            wake_mat = annot_mat[, 3:6]
            # how many sleeponset and wake annotations are there?
            if (sleeponset_loc[1] != 0 | wake_loc[1] != 0) {
              # combine sleep and wake annotations and sort them
              if (wake_loc[1] == 0) {
                # assume there is only one sleeponset loc
                sleeponset_mat = matrix(nrow = length(sleeponset_loc), ncol = 2)
                sleeponset_mat[,1] = sleeponset_loc
                sleeponset_mat[,2] = 0 # sleeponset = 0
                sleep_wake_mat = sleeponset_mat
              } else if (sleeponset_loc[1] == 0) {
                # assume there is only one wake loc
                wake_time_mat = matrix(nrow = length(wake_loc), ncol = 2)
                wake_time_mat[,1] = wake_loc
                wake_time_mat[,2] = 1 # wake = 1
                sleep_wake_mat = wake_time_mat
              } else {
                # there is both sleeponst and wake annotations
                sleeponset_mat = matrix(nrow = length(sleeponset_loc), ncol = 2)
                sleeponset_mat[,1] = sleeponset_loc
                sleeponset_mat[,2] = 0 # sleeponset = 0
                wake_time_mat = matrix(nrow = length(wake_loc), ncol = 2)
                wake_time_mat[,1] = wake_loc
                wake_time_mat[,2] = 1 # wake = 1
                sleep_wake_mat = rbind(sleeponset_mat,wake_time_mat)
                sleep_wake_mat = sleep_wake_mat[order(sleep_wake_mat[,1]),] # sort rows
              }
              # create an annotation on the final data-point for the day that is opposite to the previous point
              if (last_day_adjust != 0) {  # adjust end location if it is the final day of plotting and there is less than 24h
                end_value = last_day_adjust
              } else {
                end_value = abcisL
              }
              if (sleep_wake_mat[length(sleep_wake_mat)] == 1) {    # add final location at end of 24h period
                sleep_wake_mat = rbind(sleep_wake_mat,c(end_value,0))
              } else {
                sleep_wake_mat = rbind(sleep_wake_mat,c(end_value,1))
              }
              # loop through each annotation and update data frames accordingly
              if (first_day_adjust != 0) {
                prev_loc = first_day_adjust  # start at adjusted start point on day one
                if (first_day_adjust > sleep_wake_mat[1,1]) warning('ERROR: sleep/wake annotation is before the recording begins')
              } else {
                prev_loc = 1  # otherwise, start from first sample
              }
              for (i4 in 1:length(sleep_wake_mat[, 1])) {
                annot_type = sleep_wake_mat[i4, 2] # get if it is wake (1) or sleep (0)
                curr_loc = sleep_wake_mat[i4, 1]
                if (annot_type == 0) {
                  # sleeponset annotation, previous data is wake  # turn off sleep_mat (NA)
                  sleep_mat[prev_loc:curr_loc,] <- 0
                } else {
                  # wake annotation, previous data is sleep    # turn off wake_mat (NA)
                  wake_mat[prev_loc:curr_loc,] <- 0
                }
                prev_loc = curr_loc
              }
            } else {
              # there are no sleeponset or wake annotations on this day.
              # plot both sleep and wake annotations for now ?
              # ? other suggestions welcome ?
            }
            if (viewingwindow == 1) { #focus on day
              if (length(d2exclude) > 0) {
                if (length(which(d2exclude == g)) > 0) skip = TRUE
              }
            } else { #focus on night
              if (length(n2exclude) > 0) {
                if (length(which(n2exclude == g)) > 0) skip = TRUE
              }
            }
            #VvH I have moved unclass to one location, to avoid doing this computation several times:
            curr_date_unclassed = unclass(as.POSIXlt(curr_date,desiredtz))
            title = paste0("Day ",daycount,": ",
                           wdaynames[curr_date_unclassed$wday + 1],
                           " | ",
                           curr_date_unclassed$mday, " ",
                           month.abb[curr_date_unclassed$mon + 1], " ",
                           curr_date_unclassed$year + 1900)
            rm(curr_date_unclassed)
            if (skip == FALSE) {
              YXLIM = c(-230,300)
              LJ = 2
              # plot accelerometer data:
              if (abcisL == length(acc) & abcisL == length(ang)) {  # check to prevent x.y coords errors, show more useful error msg
                plot(abcis, acc, type = "l", lwd = LWDA,
                     bty = "l", axes = FALSE, ylim = YXLIM,
                     xlab = "", ylab = "", main = "",
                     cex.main = 0.9, lend = LJ) #,axes=FALSE,ylim=YXLIM,xlab="",ylab="",main="",cex=0.3
                # plot z-angle:
                lines(abcis,ang, type = "l", lwd = LWDA, bty = "l",
                      xlab = "", ylab = "", cex = 0.3, lend = LJ)
              } else {
                break()
                # Following warning turned off because we expect condition not
                # to be met when recording ends
                # with many non-wear days
                # warning('\nplot5 error: index, acc and ang vectors are different lengths')
              }
              # add sleeponset time annotation to plot:
              arrow_line_length = abcisL * 0.01736 # make arrow line length adaptable to differnt short epochs
              if (sleeponset_loc[1] != 0) {
                for (i5 in sleeponset_loc) { # allow for multiple sleeponset_loc
                  set_pos = 4 # position of text in relation to arrow
                  ar_start_idx <- i5
                  ar_end_idx <- i5 + arrow_line_length # make arrow go to the right of the annotation line
                  if (i > (0.8 * abcisL)) {  # check to see if text should be placed on the left side of the line
                    set_pos = 2
                    ar_end_idx <- i5 - arrow_line_length
                  }
                  # draw sleeponset annotation:
                  segments(i5, -230, i5, 210, col = 'black', lwd = 1.5)
                  arrows(ar_start_idx, 205, ar_end_idx, 205, length = 0.05,
                         angle = 20, code = 1, lwd = 0.5)
                  segments(ar_start_idx, 205, ar_end_idx, 205, col = "black", lwd = 0.5)
                  text(ar_end_idx, 205, labels = "Sleep-onset", pos = set_pos,
                       font = 1.8, cex = 0.8, col = "darkgrey")
                }
              }
              # add wake time annotation to plot:
              if (wake_loc[1] != 0) {
                for (i6 in wake_loc) {
                  set_pos <- 4
                  ar_start_idx <- i6
                  ar_end_idx <- i6 + arrow_line_length
                  if (i6 > (0.8 * abcisL)) {
                    # check to see if text should be placed on the left side of the line
                    set_pos = 2
                    ar_end_idx <- i6 - arrow_line_length
                  }
                  # draw wake annotation:
                  segments(i6,-230, i6, 210, col = 'black', lwd = 1.5)
                  arrows(ar_start_idx, 160, ar_end_idx, 160,
                         length = 0.05, angle = 20, code = 1, lwd = 0.5)
                  segments(ar_start_idx, 160, ar_end_idx, 160, col = "black", lwd = 0.5)
                  text(ar_end_idx, 160, labels = "Wake", pos = set_pos,
                       font = 1.8, cex = 0.8, col = "darkgrey")
                }
              }
              # rect for all annotations:
              # make everything 0 in case there are any NA's left:
              sleep_mat[is.na(sleep_mat)] <- 0
              wake_mat[is.na(wake_mat)] <- 0
              non_wear[is.na(non_wear)] <- 0
              # colours for annotations:
              nonwear_col <- rgb(0, 0.8, 0, alpha = 0.4)
              inactive_col <- rgb(0.3, 0.3, 1, alpha = 0.2)
              light_pa_col <- rgb(1, 1, 0, alpha = 0.2)
              mod_pa_col <- rgb(1, 0.5, 0, alpha = 0.2)
              vig_pa_col <- rgb(1, 0, 0, alpha = 0.2)
              night_sleep_col <- rgb(0.8, 0.8, 0.8, alpha = 0.3)
              night_wake_col <- rgb(0, 0.8, 0.6, alpha = 0.2)
              spt_heights <- c(-80, 110)
              pa_heights <- c(-220, -100)
              plot_rects <- function(vec, col_select, rect_heights) {
                # use the rect function to highlight sleep and activity annotations on the plot
                if (sum(vec) > 0) {
                  r_start_idx <- which(diff(c(0L, vec)) == 1L)
                  if (length(r_start_idx) != 0) {
                    r_end_idx <- which(diff(c(vec, 0L)) == -1L)
                    if (length(r_end_idx) + 1 == length(r_start_idx)) {
                      r_end_idx[length(r_start_idx)] <- length(vec)
                    }
                    for (idx in 1:length(r_start_idx)) {
                      rect(
                        r_start_idx[idx],
                        rect_heights[1],
                        r_end_idx[idx],
                        rect_heights[2],
                        col = col_select,
                        border = NA
                      )
                    }
                  }
                }
              }
              # daytime inactivity annotations
              plot_rects(vec = wake_mat[, 1],
                         col_select = inactive_col,
                         rect_heights = pa_heights)
              # light pa annotatoins
              plot_rects(vec = wake_mat[, 2],
                         col_select = light_pa_col,
                         rect_heights = pa_heights)
              # mod pa annotations
              plot_rects(vec = wake_mat[, 3],
                         col_select = mod_pa_col,
                         rect_heights = pa_heights)
              # vig pa annotations
              plot_rects(vec = wake_mat[, 4],
                         col_select = vig_pa_col,
                         rect_heights = pa_heights)
              # night_sleep annotations on graph
              plot_rects(vec = sleep_mat[, 1],
                         col_select = night_sleep_col,
                         rect_heights = spt_heights)
              # night_wake annotations on graph
              plot_rects(vec = sleep_mat[, 2],
                         col_select = night_wake_col,
                         rect_heights = spt_heights)
              # non_wear annotations on graph
              plot_rects(
                vec = non_wear,
                col_select = nonwear_col,
                rect_heights = c(-240, 130)
              )
              axis(side = 1, at = seq(1, (((60 / ws3) * 60 * 24) + 1),
                                      by = (2 * (60 / ws3) * 60)),
                   labels = xaxislabels, cex.axis = 0.7)
              abline(h = 0, untf = FALSE, lty = 3, lwd = 1, col = "grey")
              # x-axis coordinate for plotting text on the plot adaptable
              # to different short epoch lengths
              plot_loc = -abcisL * 0.05
              text(x = plot_loc, y = 285, labels = title, pos = 4, font = 2, cex = 1)
              text(x = plot_loc, y = -120, labels = "Arm movement:",
                   pos = 4, font = 1.8, cex = 0.9)
              text(x = plot_loc, y = 100,
                   labels = "Angle of sensor's z-axis relative to horizontal plane:",
                   pos = 4, font = 1.8, cex = 0.9)
              box("figure", col = "black")
              legend("topright", legend = c("SPT Window: Sleep",
                                            "SPT Window: Wake",
                                            "Inactivity", "Light PA",
                                            "Moderate PA", "Vigorous PA",
                                            "Non-Wear"),
                     lty = c(1, 1), col = c(
                       night_sleep_col, night_wake_col,
                       inactive_col, light_pa_col,
                       mod_pa_col, vig_pa_col,
                       nonwear_col
                     ),
                     lwd = c(LWDX, LWDX, LWDX), bg = "white", cex = 0.6, ncol = 7, box.lwd = BLX)
              if (daycount == 1 |
                  ((daycount - 1) / NGPP) == (round((daycount - 1) / NGPP))) {
                mtext(paste0("Filename: ", fnames_ms1_stripped[sel]),
                      side = 3, line = 0, outer = TRUE, font = 2, cex = 0.6)
                mtext(text = paste0("Warning: This GGIR legacy report is not intended to be used for data quality assessment,",
                                    " To quality check your data visually see file names starting with Time_report_ "),
                      side = 3, line = 1, outer = TRUE, font = 2, cex = 0.6
                )
              }
            }
            daycount = daycount + 1
          }
          dev.off()
        }
      }
    }
  }
}

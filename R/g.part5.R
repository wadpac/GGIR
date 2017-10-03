g.part5 = function (datadir = c(), metadatadir = c(), f0 = c(), f1 = c(), 
                    strategy = 1, maxdur = 7, hrs.del.start = 0, hrs.del.end = 0, 
                    loglocation = c(), excludefirstlast = FALSE, windowsizes = c(5, 
                                                                                 900, 3600), acc.metric = "ENMO", boutcriter.mvpa = 0.8, 
                    boutcriter.in = 0.9, boutcriter.lig = 0.8, storefolderstructure = FALSE, 
                    threshold.lig = c(40), threshold.mod = c(100), threshold.vig = c(400), 
                    timewindow = c("MM", "WW"), boutdur.mvpa = c(1, 5, 10), 
                    boutdur.in = c(10, 20, 30), boutdur.lig = c(1, 5, 10), winhr = 5, 
                    M5L5res = 10, overwrite = FALSE, desiredtz = "Europe/London", 
                    bout.metric = 4, dayborder = 0) 
{
        ms5.out = "/meta/ms5.out"
        if (file.exists(paste(metadatadir, ms5.out, sep = ""))) {
        }
        else {
                dir.create(file.path(metadatadir, ms5.out))
        }
        SUM = nightsummary = M = sib.cla.sum = c()
        fnames.ms1 = sort(dir(paste(metadatadir, "/meta/basic", 
                                    sep = "")))
        fnames.ms2 = sort(dir(paste(metadatadir, "/meta/ms2.out", 
                                    sep = "")))
        fnames.ms3 = sort(dir(paste(metadatadir, "/meta/ms3.out", 
                                    sep = "")))
        fnames.ms4 = sort(dir(paste(metadatadir, "/meta/ms4.out", 
                                    sep = "")))
        fnames.ms5 = sort(dir(paste(metadatadir, "/meta/ms5.out", 
                                    sep = "")))
        results = paste(metadatadir, "/results", sep = "")
        ffdone = fnames.ms5
        wdaynames = c("Sunday", "Monday", "Tuesday", "Wednesday", 
                      "Thursday", "Friday", "Saturday")
        nfeatures = 500
        ws3 = windowsizes[1]
        ds_names = rep("", nfeatures)
        di = 1
        cnt = 1
        fnames.ms3 = sort(fnames.ms3)
        if (f1 == 0) 
                length(fnames.ms4)
        if (f1 > length(fnames.ms4)) 
                f1 = length(fnames.ms4)
        if (storefolderstructure == TRUE) {
                extractfilenames = function(x) as.character(unlist(strsplit(x, 
                                                                            ".RDa"))[1])
                referencefnames = sapply(fnames.ms3, extractfilenames)
                folderstructure = getfolderstructure(datadir, referencefnames)
                fullfilenames = folderstructure$fullfilenames
                foldername = folderstructure$foldername
        }
        t0 = t1 = Sys.time()
        for (i in f0:f1) {
                if (length(ffdone) > 0) {
                        if (length(which(ffdone == fnames.ms3[i])) > 0) {
                                skip = 1
                        }
                        else {
                                skip = 0
                        }
                }
                else {
                        skip = 0
                }
                if (overwrite == TRUE) 
                        skip = 0
                selp = which(fnames.ms4 == fnames.ms3[i])
                if (file.exists(paste(metadatadir, "/meta/ms4.out/", 
                                      fnames.ms4[selp], sep = "")) == FALSE) {
                        skip = 1
                }
                if (skip == 0) {
                        t2 = Sys.time()
                        deltat = as.numeric(difftime(t2, t1, units = "hours"))
                        if (deltat > 0.5) {
                                deltat0 = round(as.numeric(difftime(t2, t0, 
                                                                    units = "hours")) * 10)/10
                                cat(paste(" ", i, " (", unlist(strsplit(as.character(t2), 
                                                                        " "))[2], ")", sep = ""))
                                t1 = Sys.time()
                        }
                        else {
                                cat(paste(" ", i, sep = ""))
                        }
                        selp = which(fnames.ms2 == fnames.ms3[i])
                        load(file = paste(metadatadir, "/meta/ms2.out/", 
                                          fnames.ms2[selp], sep = ""))
                        daysummary = SUM$daysummary
                        summary = SUM$summary
                        selp = which(fnames.ms4 == fnames.ms3[i])
                        load(file = paste(metadatadir, "/meta/ms4.out/", 
                                          fnames.ms4[selp], sep = ""))
                        summarysleep = nightsummary
                        rm(nightsummary)
                        idindex = which(summarysleep$filename == fnames.ms3[i])
                        id = summarysleep$id[idindex[1]]
                        ndays = nrow(summarysleep)
                        dsummary = matrix("", (40 * length(unique(summarysleep$acc_def)) * 
                                                       length(unique(threshold.lig)) * length(unique(threshold.mod)) * 
                                                       length(unique(threshold.vig)) * length(unique(timewindow))), 
                                          nfeatures)
                        di = 1
                        fi = 1
                        if (length(idindex) > 0 & nrow(summarysleep) > 1) {
                                summarysleep_tmp = summarysleep
                                selp = which(fnames.ms1 == paste("meta_", fnames.ms3[i], 
                                                                 sep = ""))
                                if (length(selp) != 1) {
                                        cat("Warning: File not processed with part1")
                                }
                                load(paste0(metadatadir, "/meta/basic/", fnames.ms1[selp]))
                                load(paste0(metadatadir, "/meta/ms3.out/", fnames.ms3[i]))
                                IMP = g.impute(M, I, strategy = strategy, hrs.del.start = hrs.del.start, 
                                               hrs.del.end = hrs.del.end, maxdur = maxdur)
                                time = IMP$metashort[, 1]
                                ACC = IMP$metashort[, acc.metric] * 1000
                                if (length(which(names(IMP$metashort) == "anglez")) == 
                                    0) {
                                        cat("Warning: anglez not extracted. Please check that do.anglez == TRUE")
                                }
                                angle = as.numeric(as.matrix(M$metashort[, which(names(IMP$metashort) == 
                                                                                         "anglez")]))
                                nonwear = IMP$rout[, 5]
                                nonwear = rep(nonwear, each = (IMP$windowsizes[2]/IMP$windowsizes[1]))
                                if (length(nonwear) > length(time)) {
                                        nonwear = nonwear[1:length(time)]
                                }
                                else if (length(nonwear) < length(time)) {
                                        nonwear = c(nonwear, rep(0, (length(time) - 
                                                                             length(nonwear))))
                                }
                                rm(IMP, M, I)
                                S = sib.cla.sum
                                rm(sib.cla.sum)
                                def = unique(S$definition)
                                cut = which(S$fraction.night.invalid > 0.7 | 
                                                    S$nsib.periods == 0)
                                if (length(cut) > 0) 
                                        S = S[-cut, ]
                                for (j in def) {
                                        Sbackup = S
                                        S2 = S[which(S$definition == j), ]
                                        detection = rep(0, length(time))
                                        tmp = S2
                                        s0s1 = c()
                                        pr0 = 1
                                        pr1 = pr0 + ((60/ws3) * 1440 * 6)
                                        pr2 = length(time)
                                        if (nrow(S2) > 0) {
                                                gik.ons = as.character(S2$sib.onset.time)
                                                gik.end = as.character(S2$sib.end.time)
                                                for (g in 1:nrow(S2)) {
                                                        lastpr0 = pr0
                                                        pr1 = pr0 + ((60/ws3) * 1440 * 6)
                                                        if (pr1 > pr2) 
                                                                pr1 = pr2
                                                        if (pr0 > pr1) 
                                                                pr0 = pr1
                                                        s0 = which(time[pr0:pr1] == gik.ons[g])[1]
                                                        s1 = which(time[pr0:pr1] == gik.end[g])[1]
                                                        timebb = as.character(time[pr0:pr1])
                                                        if (length(unlist(strsplit(timebb[1], 
                                                                                   "[+]"))) > 1) {
                                                                timebb = iso8601chartime2POSIX(timebb, 
                                                                                               tz = desiredtz)
                                                        }
                                                        #Change 1 - 03/10/2017
                                                        timebb = as.character(timebb)
                                                        #End of change 1
                                                        s0 = which(timebb == gik.ons[g])[1]
                                                        s1 = which(timebb == gik.end[g])[1]
                                                        if (is.na(s0) == TRUE) 
                                                                s0 = which(timebb == paste(gik.ons[g], 
                                                                                           " 00:00:00", sep = ""))[1]
                                                        if (is.na(s1) == TRUE) 
                                                                s1 = which(timebb == paste(gik.end[g], 
                                                                                           " 00:00:00", sep = ""))[1]
                                                        s0 = s0 + pr0 - 1
                                                        s1 = s1 + pr0 - 1
                                                        pr0 = s1
                                                        if (length(s1) != 0 & length(s0) != 0 & 
                                                            is.na(s0) == FALSE & is.na(s1) == FALSE) {
                                                                s0s1 = c(s0s1, s0:s1)
                                                        }
                                                        else {
                                                                pr0 = lastpr0 + ((60/ws3) * 1440 * 6)
                                                        }
                                                }
                                        }
                                        detection[s0s1] = 1
                                        tempp = unclass(as.POSIXlt(iso8601chartime2POSIX(time, 
                                                                                         tz = desiredtz), tz = desiredtz))
                                        if (is.na(tempp$sec[1]) == TRUE) {
                                                tempp = unclass(as.POSIXlt(time, tz = desiredtz))
                                        }
                                        sec = tempp$sec
                                        min = tempp$min
                                        hour = tempp$hour
                                        if (dayborder == 0) {
                                                nightsi = which(sec == 0 & min == 0 & hour == 
                                                                        0)
                                        }
                                        else {
                                                nightsi = which(sec == 0 & min == (dayborder - 
                                                                                           floor(dayborder)) * 60 & hour == floor(dayborder))
                                        }
                                        summarysleep_tmp2 = summarysleep_tmp[which(summarysleep_tmp$acc_def == 
                                                                                           j), ]
                                        if (excludefirstlast == FALSE) {
                                                summarysleep_tmp2 = summarysleep_tmp2[-nrow(summarysleep_tmp2), 
                                                                                      ]
                                        }
                                        redo1 = nightsi[1] - ((60/ws3) * 60)
                                        if (redo1 < 1) 
                                                redo1 = 1
                                        redo2 = nightsi[1] + (14 * (60/ws3) * 60)
                                        anglethreshold = as.numeric(unlist(strsplit(j, 
                                                                                    "A"))[2])
                                        tempi = unlist(strsplit(unlist(strsplit(j, 
                                                                                "A"))[1], "T"))
                                        timethreshold = as.numeric(tempi[length(tempi)])
                                        Nsleep = length(timethreshold) * length(anglethreshold)
                                        sleep = matrix(0, length(angle[redo1:redo2]), 
                                                       Nsleep)
                                        angle[which(is.na(angle[redo1:redo2]) == T)] = 0
                                        sdl1 = rep(0, length(time[redo1:redo2]))
                                        postch = which(abs(diff(angle[redo1:redo2])) > 
                                                               anglethreshold)
                                        q1 = c()
                                        if (length(postch) > 1) {
                                                q1 = which(diff(postch) > (timethreshold * 
                                                                                   (60/ws3)))
                                        }
                                        if (length(q1) > 0) {
                                                for (gi in 1:length(q1)) {
                                                        sdl1[postch[q1[gi]]:postch[q1[gi] + 1]] = 1
                                                }
                                        }
                                        else {
                                                if (length(postch) < 5) {
                                                        sdl1[1:length(sdl1)] = 1
                                                }
                                                else {
                                                        sdl1[1:length(sdl1)] = 0
                                                }
                                        }
                                        detection[redo1:redo2] = sdl1
                                        w0 = w1 = rep(0, length(summarysleep_tmp2$calendardate))
                                        diur = rep(0, length(time))
                                        pko = which(summarysleep_tmp2$acc_onset == 
                                                            0 & summarysleep_tmp2$acc_wake == 0 & summarysleep_tmp2$acc_dur_noc == 
                                                            0)
                                        if (length(pko) > 0) {
                                                summarysleep_tmp2 = summarysleep_tmp2[-pko, 
                                                                                      ]
                                        }
                                        if (nrow(summarysleep_tmp2) > 0) {
                                                for (k in 1:length(summarysleep_tmp2$calendardate)) {
                                                        tt = unlist(strsplit(as.character(summarysleep_tmp2$calendardate[k]), 
                                                                             "/"))
                                                        w0[k] = paste(tt[3], "-", tt[2], "-", 
                                                                      tt[1], " ", as.character(summarysleep_tmp2$acc_onset_ts[k]), 
                                                                      sep = "")
                                                        w1[k] = paste(tt[3], "-", tt[2], "-", 
                                                                      tt[1], " ", as.character(summarysleep_tmp2$acc_wake_ts[k]), 
                                                                      sep = "")
                                                        if (summarysleep_tmp2$acc_onset[k] >= 
                                                            24) {
                                                                w0[k] = as.character(as.POSIXlt(w0[k], 
                                                                                                tz = desiredtz) + (24 * 3600))
                                                        }
                                                        if (summarysleep_tmp2$acc_wake[k] >= 24 | 
                                                            (summarysleep_tmp2$daysleeper[k] == 
                                                             1 & summarysleep_tmp2$acc_wake[k] < 
                                                             18)) {
                                                                w1[k] = as.character(as.POSIXlt(w1[k], 
                                                                                                tz = desiredtz) + (24 * 3600))
                                                        }
                                                        w0c = as.character(as.POSIXlt(w0[k], tz = desiredtz))
                                                        w1c = as.character(as.POSIXlt(w1[k], tz = desiredtz))
                                                        s0 = which(as.character(time) == w0c)[1]
                                                        s1 = which(as.character(time) == w1c)[1]
                                                        if (length(s0) == 0) {
                                                                w0c = paste0(w0c, " 00:00:00")
                                                                s0 = which(as.character(time) == w0c)[1]
                                                        }
                                                        if (length(s1) == 0) {
                                                                w1c = paste0(w1c, " 00:00:00")
                                                                s1 = which(as.character(time) == w1c)[1]
                                                        }
                                                        timebb = as.character(time)
                                                        if (length(unlist(strsplit(timebb, "[+]"))) > 
                                                            1) {
                                                                timebb = iso8601chartime2POSIX(timebb, 
                                                                                               tz = desiredtz)
                                                        }
                                                        #Change 2 - 03/10/2017
                                                        timebb = as.character(timebb)
                                                        if (is.na(s0) == TRUE) 
                                                                s0 = which(timebb == w0c)[1]
                                                        if (is.na(s1) == TRUE) 
                                                                s1 = which(timebb == w1c)[1]
                                                        #End of change 2
                                                        if (length(s1) != 0 & length(s0) != 0 & 
                                                            is.na(s0) == FALSE & is.na(s1) == FALSE) {
                                                                diur[s0:s1] = 1
                                                        }
                                                }
                                                if (length(loglocation) > 0) {
                                                        LOG = read.csv(loglocation)
                                                        waket = as.character(LOG[which(LOG$STNO == 
                                                                                               id), which(names(LOG) == "FGAWK2")])
                                                        if (length(waket) > 0) {
                                                                if (waket != "") {
                                                                        waket2 = as.numeric(unlist(strsplit(waket, 
                                                                                                            ":")))
                                                                        waket3 = waket2[1] * (60 * (60/ws3)) + 
                                                                                waket2[2] * ((60/ws3)) + round(waket2[3]/5)
                                                                        waketi = nightsi[1] + waket3
                                                                }
                                                                else {
                                                                        waketi = nightsi[1] + ((60/ws3) * 
                                                                                                       8 * 60)
                                                                }
                                                        }
                                                        else {
                                                                waketi = nightsi[1] + ((60/ws3) * 8 * 
                                                                                               60)
                                                        }
                                                }
                                                else {
                                                        waketi = nightsi[1] + ((60/ws3) * 8 * 
                                                                                       60)
                                                }
                                                found = FALSE
                                                step = 0
                                                while (found == FALSE) {
                                                        if (detection[waketi] == 1) {
                                                                step = step + 1
                                                        }
                                                        else {
                                                                step = step - 1
                                                        }
                                                        if (detection[waketi + step] != detection[waketi]) {
                                                                found = TRUE
                                                                waketi = waketi + step
                                                        }
                                                        if (abs(step) == (60/ws3) * (4 * 60) | 
                                                            (waketi + step) < nightsi[1]) {
                                                                found = TRUE
                                                        }
                                                }
                                                ss0 = nightsi[1] - 10
                                                if (ss0 < 1) 
                                                        ss0 = 1
                                                diur[ss0:waketi] = 1
                                                for (TRLi in threshold.lig) {
                                                        for (TRMi in threshold.mod) {
                                                                for (TRVi in threshold.vig) {
                                                                        levels = identify_levels(time, diur, 
                                                                                                 detection, ACC, TRLi, TRMi, TRVi, 
                                                                                                 boutdur.mvpa, boutcriter.mvpa, boutdur.lig, 
                                                                                                 boutcriter.lig, boutdur.in, boutcriter.in, 
                                                                                                 ws3, bout.metric)
                                                                        LEVELS = levels$LEVELS
                                                                        OLEVELS = levels$OLEVELS
                                                                        Lnames = levels$Lnames
                                                                        bc.mvpa = levels$bc.mvpa
                                                                        bc.lig = levels$bc.lig
                                                                        bc.in = levels$bc.in
                                                                        NNIGHTSSLEEP = length(unique(summarysleep_tmp2$calendardate))
                                                                        NNIGHTSACC = length(nightsi)
                                                                        FM = which(diff(diur) == -1)
                                                                        if (length(FM) > 0) {
                                                                                nightsi = nightsi[which(nightsi > 
                                                                                                                FM[1] & nightsi < FM[length(FM)])]
                                                                        }
                                                                        for (timewindowi in timewindow) {
                                                                                for (wi in 1:(nrow(summarysleep_tmp2))) {
                                                                                        qqq = rep(0, 2)
                                                                                        if (timewindowi == "MM") {
                                                                                                qqq[1] = nightsi[wi] + 1
                                                                                                qqq[2] = nightsi[wi + 1]
                                                                                        }
                                                                                        else {
                                                                                                qqq[1] = which(diff(diur) == 
                                                                                                                       -1)[wi] + 1
                                                                                                qqq[2] = which(diff(diur) == 
                                                                                                                       -1)[wi + 1]
                                                                                        }
                                                                                        if (length(which(is.na(qqq) == 
                                                                                                         TRUE)) == 0) {
                                                                                                fi = 1
                                                                                                dsummary[di, fi] = id
                                                                                                ds_names[fi] = "id"
                                                                                                fi = fi + 1
                                                                                                dsummary[di, fi] = fnames.ms3[i]
                                                                                                ds_names[fi] = "filename"
                                                                                                fi = fi + 1
                                                                                                dsummary[di, fi:(fi + 1)] = c(as.character(summarysleep_tmp2$weekday[wi]), 
                                                                                                                              as.character(summarysleep_tmp2$calendardate[wi]))
                                                                                                ds_names[fi:(fi + 1)] = c("weekday", 
                                                                                                                          "calendardate")
                                                                                                fi = fi + 2
                                                                                                dsummary[di, fi] = j
                                                                                                ds_names[fi] = "acc_def"
                                                                                                fi = fi + 1
                                                                                                dsummary[di, fi] = summarysleep_tmp2$night[wi]
                                                                                                ds_names[fi] = "night number"
                                                                                                fi = fi + 1
                                                                                                dsummary[di, fi] = summarysleep_tmp2$acc_onset[wi]
                                                                                                ds_names[fi] = "acc_onset"
                                                                                                fi = fi + 1
                                                                                                dsummary[di, fi] = summarysleep_tmp2$acc_wake[wi]
                                                                                                ds_names[fi] = "acc_wake"
                                                                                                fi = fi + 1
                                                                                                dsummary[di, fi] = summarysleep_tmp2$sleeplog_onset[wi]
                                                                                                ds_names[fi] = "sleeplog_onset"
                                                                                                fi = fi + 1
                                                                                                dsummary[di, fi] = summarysleep_tmp2$sleeplog_wake[wi]
                                                                                                ds_names[fi] = "sleeplog_wake"
                                                                                                fi = fi + 1
                                                                                                dsummary[di, fi] = summarysleep_tmp2$acc_onset_ts[wi]
                                                                                                ds_names[fi] = "acc_onset_ts"
                                                                                                fi = fi + 1
                                                                                                dsummary[di, fi] = summarysleep_tmp2$acc_wake_ts[wi]
                                                                                                ds_names[fi] = "acc_wake_ts"
                                                                                                fi = fi + 1
                                                                                                dsummary[di, fi] = summarysleep_tmp2$sleeplog_onset_ts[wi]
                                                                                                ds_names[fi] = "sleeplog_onset_ts"
                                                                                                fi = fi + 1
                                                                                                dsummary[di, fi] = summarysleep_tmp2$sleeplog_wake_ts[wi]
                                                                                                ds_names[fi] = "sleeplog_wake_ts"
                                                                                                fi = fi + 1
                                                                                                dsummary[di, fi] = summarysleep_tmp2$daysleeper[wi]
                                                                                                ds_names[fi] = "daysleeper"
                                                                                                fi = fi + 1
                                                                                                dsummary[di, fi] = summarysleep_tmp2$cleaningcode[wi]
                                                                                                ds_names[fi] = "cleaningcode"
                                                                                                fi = fi + 1
                                                                                                dsummary[di, fi] = summarysleep_tmp2$sleeplog_used[wi]
                                                                                                ds_names[fi] = "sleeplog_used"
                                                                                                fi = fi + 1
                                                                                                dsummary[di, fi] = summarysleep_tmp2$acc_available[wi]
                                                                                                ds_names[fi] = "acc_available"
                                                                                                fi = fi + 1
                                                                                                if (timewindowi == "MM") {
                                                                                                        qqq1 = nightsi[wi] + 1
                                                                                                        qqq2 = nightsi[wi + 1]
                                                                                                        dsummary[di, fi] = "MM"
                                                                                                }
                                                                                                else {
                                                                                                        qqq1 = which(diff(diur) == 
                                                                                                                             -1)[wi] + 1
                                                                                                        qqq2 = which(diff(diur) == 
                                                                                                                             -1)[wi + 1]
                                                                                                        dsummary[di, fi] = "WW"
                                                                                                }
                                                                                                ds_names[fi] = "window"
                                                                                                fi = fi + 1
                                                                                                dsummary[di, fi] = TRLi
                                                                                                ds_names[fi] = "TRLi"
                                                                                                fi = fi + 1
                                                                                                dsummary[di, fi] = TRMi
                                                                                                ds_names[fi] = "TRMi"
                                                                                                fi = fi + 1
                                                                                                dsummary[di, fi] = TRVi
                                                                                                ds_names[fi] = "TRVi"
                                                                                                fi = fi + 1
                                                                                                wlih = ((qqq2 - qqq1) + 1)/((60/ws3) * 
                                                                                                                                    60)
                                                                                                if (wlih > 30 & length(summarysleep_tmp2$night) > 
                                                                                                    1) {
                                                                                                        qqq1 = (qqq2 - (24 * ((60/ws3) * 
                                                                                                                                      60))) + 1
                                                                                                        if (qqq1 < 1) 
                                                                                                                qqq1 = 1
                                                                                                        wlih = ((qqq2 - qqq1) + 1)/((60/ws3) * 
                                                                                                                                            60)
                                                                                                }
                                                                                                dsummary[di, fi] = wlih
                                                                                                ds_names[fi] = "window_length_in_hours"
                                                                                                fi = fi + 1
                                                                                                dsummary[di, fi] = (length(which(nonwear[qqq1:qqq2] == 
                                                                                                                                         1)) * ws3)/3600
                                                                                                ds_names[fi] = "nonwear_hours"
                                                                                                fi = fi + 1
                                                                                                for (levelsc in 0:(length(Lnames) - 
                                                                                                                   1)) {
                                                                                                        dsummary[di, fi] = (length(which(LEVELS[qqq1:qqq2] == 
                                                                                                                                                 levelsc)) * ws3)/60
                                                                                                        ds_names[fi] = paste("dur_", 
                                                                                                                             Lnames[levelsc + 1], "_min", 
                                                                                                                             sep = "")
                                                                                                        fi = fi + 1
                                                                                                }
                                                                                                for (g in 1:5) {
                                                                                                        dsummary[di, (fi + (g - 1))] = (length(which(OLEVELS[qqq1:qqq2] == 
                                                                                                                                                             g)) * ws3)/60
                                                                                                }
                                                                                                ds_names[fi:(fi + 4)] = c("dur_TSIBday_min", 
                                                                                                                          "dur_TOINday_min", "dur_TLIGday_min", 
                                                                                                                          "dur_TMODday_min", "dur_TVIGday_min")
                                                                                                fi = fi + 5
                                                                                                dsummary[di, fi] = (length(which(OLEVELS[qqq1:qqq2] == 
                                                                                                                                         1 | OLEVELS[qqq1:qqq2] == 
                                                                                                                                         2)) * ws3)/60
                                                                                                ds_names[fi] = "dur_TINday_min"
                                                                                                fi = fi + 1
                                                                                                dsummary[di, fi] = (length(which(diur[qqq1:qqq2] == 
                                                                                                                                         0)) * ws3)/60
                                                                                                ds_names[fi] = "dur_day_min"
                                                                                                fi = fi + 1
                                                                                                dsummary[di, fi] = (length(which(diur[qqq1:qqq2] == 
                                                                                                                                         1)) * ws3)/60
                                                                                                ds_names[fi] = "dur_night_min"
                                                                                                fi = fi + 1
                                                                                                dsummary[di, fi] = (length(c(qqq1:qqq2)) * 
                                                                                                                            ws3)/60
                                                                                                ds_names[fi] = "dur_nightandday_min"
                                                                                                fi = fi + 1
                                                                                                Nawake = length(which(abs(diff(which(LEVELS[qqq1:qqq2] == 
                                                                                                                                             0))) > (300/ws3))) - 2
                                                                                                if (Nawake < 0) 
                                                                                                        Nawake = 0
                                                                                                dsummary[di, fi] = Nawake
                                                                                                ds_names[fi] = "N_atleast5minwakenight"
                                                                                                fi = fi + 1
                                                                                                zt_hrs_nonwear = (length(which(diur[qqq1:qqq2] == 
                                                                                                                                       0 & nonwear[qqq1:qqq2] == 
                                                                                                                                       1)) * ws3)/3600
                                                                                                zt_hrs_total = (length(which(diur[qqq1:qqq2] == 
                                                                                                                                     0)) * ws3)/3600
                                                                                                dsummary[di, fi] = round((zt_hrs_nonwear/zt_hrs_total) * 
                                                                                                                                 10000)/100
                                                                                                ds_names[fi] = "nonwear_perc_day"
                                                                                                fi = fi + 1
                                                                                                zt_hrs_nonwear = (length(which(diur[qqq1:qqq2] == 
                                                                                                                                       1 & nonwear[qqq1:qqq2] == 
                                                                                                                                       1)) * ws3)/3600
                                                                                                zt_hrs_total = (length(which(diur[qqq1:qqq2] == 
                                                                                                                                     1)) * ws3)/3600
                                                                                                dsummary[di, fi] = round((zt_hrs_nonwear/zt_hrs_total) * 
                                                                                                                                 10000)/100
                                                                                                ds_names[fi] = "nonwear_perc_night"
                                                                                                fi = fi + 1
                                                                                                zt_hrs_nonwear = (length(which(nonwear[qqq1:qqq2] == 
                                                                                                                                       1)) * ws3)/3600
                                                                                                zt_hrs_total = (length(diur[qqq1:qqq2]) * 
                                                                                                                        ws3)/3600
                                                                                                dsummary[di, fi] = round((zt_hrs_nonwear/zt_hrs_total) * 
                                                                                                                                 10000)/100
                                                                                                ds_names[fi] = "nonwear_perc_nightandday"
                                                                                                fi = fi + 1
                                                                                                dsummary[di, fi] = length(which(detection[qqq1:qqq2] == 
                                                                                                                                        1 & diur[qqq1:qqq2] == 1))/length(which(diur[qqq1:qqq2] == 
                                                                                                                                                                                        1))
                                                                                                ds_names[fi] = "sleep_efficiency"
                                                                                                fi = fi + 1
                                                                                                sse = qqq1:qqq2
                                                                                                for (levelsc in 0:(length(Lnames) - 
                                                                                                                   1)) {
                                                                                                        dsummary[di, fi] = mean(ACC[sse[which(LEVELS[sse] == 
                                                                                                                                                      levelsc)]])
                                                                                                        ds_names[fi] = paste("ACC_", 
                                                                                                                             Lnames[levelsc + 1], "_mg", 
                                                                                                                             sep = "")
                                                                                                        fi = fi + 1
                                                                                                }
                                                                                                for (g in 1:5) {
                                                                                                        dsummary[di, (fi + (g - 1))] = mean(ACC[sse[which(OLEVELS[sse] == 
                                                                                                                                                                  g)]])
                                                                                                }
                                                                                                ds_names[fi:(fi + 4)] = c("ACC_TSIBday_mg", 
                                                                                                                          "ACC_TOINday_mg", "ACC_TLIGday_mg", 
                                                                                                                          "ACC_TMODday_mg", "ACC_TVIGday_mg")
                                                                                                fi = fi + 5
                                                                                                dsummary[di, fi] = mean(ACC[sse[which(OLEVELS[sse] == 
                                                                                                                                              1 | OLEVELS[sse] == 2)]])
                                                                                                ds_names[fi] = "ACC_TINday_min"
                                                                                                fi = fi + 1
                                                                                                dsummary[di, fi] = mean(ACC[sse[which(diur[sse] == 
                                                                                                                                              0)]])
                                                                                                ds_names[fi] = "ACC_day_mg"
                                                                                                fi = fi + 1
                                                                                                dsummary[di, fi] = mean(ACC[sse[which(diur[sse] == 
                                                                                                                                              1)]])
                                                                                                ds_names[fi] = "ACC_night_mg"
                                                                                                fi = fi + 1
                                                                                                dsummary[di, fi] = mean(ACC[sse])
                                                                                                ds_names[fi] = "ACC_nightandday_mg"
                                                                                                fi = fi + 1
                                                                                                WLH = ((qqq2 - qqq1) + 1)/((60/ws3) * 
                                                                                                                                   60)
                                                                                                if (WLH <= 1) 
                                                                                                        WLH = 1.001
                                                                                                dsummary[di, fi] = quantile(ACC[sse], 
                                                                                                                            probs = ((WLH - 1)/WLH))
                                                                                                ds_names[fi] = paste("quantile_mostactive60min_mg", 
                                                                                                                     sep = "")
                                                                                                fi = fi + 1
                                                                                                dsummary[di, fi] = quantile(ACC[sse], 
                                                                                                                            probs = ((WLH - 0.5)/WLH))
                                                                                                ds_names[fi] = paste("quantile_mostactive30min_mg", 
                                                                                                                     sep = "")
                                                                                                fi = fi + 1
                                                                                                for (wini in winhr) {
                                                                                                        reso = M5L5res
                                                                                                        endd = floor(WLH * 10)/10
                                                                                                        nwindow_f = (endd - wini)
                                                                                                        ignore = FALSE
                                                                                                        if (endd <= wini) 
                                                                                                                ignore = TRUE
                                                                                                        nwindow_f = nwindow_f * (60/reso)
                                                                                                        if (ignore == FALSE) {
                                                                                                                ACCrunwin = matrix(0, nwindow_f, 
                                                                                                                                   1)
                                                                                                                TIMErunwin = matrix("", 
                                                                                                                                    nwindow_f, 1)
                                                                                                                for (hri in 0:floor((((endd - 
                                                                                                                                       wini) * (60/reso)) - 1))) {
                                                                                                                        e1 = (hri * reso * (60/ws3)) + 
                                                                                                                                1
                                                                                                                        e2 = (hri + (wini * (60/reso))) * 
                                                                                                                                reso * (60/ws3)
                                                                                                                        if (e2 > length(sse)) 
                                                                                                                                e2 = length(sse)
                                                                                                                        ACCrunwin[(hri + 1), 1] = mean(ACC[sse[e1:e2]])
                                                                                                                        TIMErunwin[(hri + 1), 
                                                                                                                                   1] = as.character(time[sse[e1]])
                                                                                                                }
                                                                                                                ACCrunwin = ACCrunwin[is.na(ACCrunwin) == 
                                                                                                                                              F]
                                                                                                                TIMErunwin = TIMErunwin[is.na(ACCrunwin) == 
                                                                                                                                                F]
                                                                                                                if (length(ACCrunwin) > 
                                                                                                                    0 & length(TIMErunwin) > 
                                                                                                                    0) {
                                                                                                                        L5HOUR = TIMErunwin[which(ACCrunwin == 
                                                                                                                                                          min(ACCrunwin))[1]]
                                                                                                                        L5VALUE = min(ACCrunwin)
                                                                                                                        M5HOUR = TIMErunwin[which(ACCrunwin == 
                                                                                                                                                          max(ACCrunwin))[1]]
                                                                                                                        M5VALUE = max(ACCrunwin)
                                                                                                                }
                                                                                                                else {
                                                                                                                        L5HOUR = M5HOUR = "not detected"
                                                                                                                        L5VALUE = M5VALUE = ""
                                                                                                                }
                                                                                                        }
                                                                                                        if (ignore == FALSE) 
                                                                                                                dsummary[di, fi] = L5HOUR
                                                                                                        ds_names[fi] = paste("L", 
                                                                                                                             wini, "TIME", sep = "")
                                                                                                        fi = fi + 1
                                                                                                        if (ignore == FALSE) 
                                                                                                                dsummary[di, fi] = L5VALUE
                                                                                                        ds_names[fi] = paste("L", 
                                                                                                                             wini, "VALUE", sep = "")
                                                                                                        fi = fi + 1
                                                                                                        if (ignore == FALSE) 
                                                                                                                dsummary[di, fi] = M5HOUR
                                                                                                        ds_names[fi] = paste("M", 
                                                                                                                             wini, "TIME", sep = "")
                                                                                                        fi = fi + 1
                                                                                                        if (ignore == FALSE) 
                                                                                                                dsummary[di, fi] = M5VALUE
                                                                                                        ds_names[fi] = paste("M", 
                                                                                                                             wini, "VALUE", sep = "")
                                                                                                        fi = fi + 1
                                                                                                        if (ignore == FALSE) {
                                                                                                                if (length(unlist(strsplit(L5HOUR, 
                                                                                                                                           "[+]"))) > 1) {
                                                                                                                        L5HOUR = as.character(iso8601chartime2POSIX(L5HOUR, 
                                                                                                                                                                    tz = desiredtz))
                                                                                                                        M5HOUR = as.character(iso8601chartime2POSIX(M5HOUR, 
                                                                                                                                                                    tz = desiredtz))
                                                                                                                        if (length(unlist(strsplit(L5HOUR, 
                                                                                                                                                   " "))) == 1) 
                                                                                                                                L5HOUR = paste0(L5HOUR, 
                                                                                                                                                " 00:00:00")
                                                                                                                        if (length(unlist(strsplit(M5HOUR, 
                                                                                                                                                   " "))) == 1) 
                                                                                                                                M5HOUR = paste0(M5HOUR, 
                                                                                                                                                " 00:00:00")
                                                                                                                }
                                                                                                                time_num = sum(as.numeric(unlist(strsplit(unlist(strsplit(L5HOUR, 
                                                                                                                                                                          " "))[2], ":"))) * c(3600, 
                                                                                                                                                                                               60, 1))/3600
                                                                                                                if (time_num < 12) 
                                                                                                                        time_num = time_num + 
                                                                                                                        24
                                                                                                                dsummary[di, fi] = time_num
                                                                                                        }
                                                                                                        ds_names[fi] = paste("L", 
                                                                                                                             wini, "TIME_num", sep = "")
                                                                                                        fi = fi + 1
                                                                                                        if (ignore == FALSE) {
                                                                                                                time_num = sum(as.numeric(unlist(strsplit(unlist(strsplit(M5HOUR, 
                                                                                                                                                                          " "))[2], ":"))) * c(3600, 
                                                                                                                                                                                               60, 1))/3600
                                                                                                                if (time_num < 12) 
                                                                                                                        time_num = time_num + 
                                                                                                                                24
                                                                                                                dsummary[di, fi] = time_num
                                                                                                        }
                                                                                                        ds_names[fi] = paste("M", 
                                                                                                                             wini, "TIME_num", sep = "")
                                                                                                        fi = fi + 1
                                                                                                }
                                                                                                NANS = which(is.nan(dsummary[di, 
                                                                                                                             ]) == TRUE)
                                                                                                if (length(NANS) > 0) 
                                                                                                        dsummary[di, NANS] = ""
                                                                                                checkshape = function(boutcount) {
                                                                                                        if (is.matrix(boutcount) == 
                                                                                                            FALSE) {
                                                                                                                boutcount = as.matrix(boutcount)
                                                                                                                if (nrow(boutcount) > ncol(boutcount)) 
                                                                                                                        boutcount = t(boutcount)
                                                                                                        }
                                                                                                        return(boutcount)
                                                                                                }
                                                                                                bc.mvpa = checkshape(bc.mvpa)
                                                                                                for (bci in 1:nrow(bc.mvpa)) {
                                                                                                        dsummary[di, fi + (bci - 1)] = length(which(diff(bc.mvpa[bci, 
                                                                                                                                                                 ])[sse] == 1))
                                                                                                        ds_names[fi + (bci - 1)] = paste0("Nbouts_MVPA_D", 
                                                                                                                                          boutdur.mvpa[bci], "T", 
                                                                                                                                          TRMi)
                                                                                                }
                                                                                                fi = fi + bci
                                                                                                bc.in = checkshape(bc.in)
                                                                                                for (bci in 1:nrow(bc.in)) {
                                                                                                        dsummary[di, fi + (bci - 1)] = length(which(diff(bc.in[bci, 
                                                                                                                                                               ])[sse] == 1))
                                                                                                        ds_names[fi + (bci - 1)] = paste0("Nbouts_INB_D", 
                                                                                                                                          boutdur.in[bci], "T", TRLi)
                                                                                                }
                                                                                                fi = fi + bci
                                                                                                bc.lig = checkshape(bc.lig)
                                                                                                for (bci in 1:nrow(bc.lig)) {
                                                                                                        dsummary[di, fi + (bci - 1)] = length(which(diff(bc.lig[bci, 
                                                                                                                                                                ])[sse] == 1))
                                                                                                        ds_names[fi + (bci - 1)] = paste0("Nbouts_LIGB_D", 
                                                                                                                                          boutdur.lig[bci], "T", TRLi, 
                                                                                                                                          "_", TRMi)
                                                                                                }
                                                                                                fi = fi + bci
                                                                                                for (levelsc in 0:(length(Lnames) - 
                                                                                                                   1)) {
                                                                                                        dsummary[di, fi] = length(which(diff(which(LEVELS[sse] != 
                                                                                                                                                           levelsc)) > 1))
                                                                                                        if (dsummary[di, fi] == 0 & 
                                                                                                            LEVELS[qqq1] == levelsc) 
                                                                                                                dsummary[di, fi] = 1
                                                                                                        ds_names[fi] = paste("Nblocks_", 
                                                                                                                             Lnames[levelsc + 1], sep = "")
                                                                                                        fi = fi + 1
                                                                                                }
                                                                                                for (g in 1:5) {
                                                                                                        dsummary[di, (fi + (g - 1))] = length(which(diff(which(OLEVELS[qqq1:qqq2] != 
                                                                                                                                                                       g)) > 1))
                                                                                                }
                                                                                                ds_names[fi:(fi + 4)] = c("Nblocks_TSIBday", 
                                                                                                                          "Nblocks_TOINday", "Nblocks_TLIGday", 
                                                                                                                          "Nblocks_TMODday", "Nblocks_TVIGday")
                                                                                                fi = fi + 5
                                                                                                dsummary[di, fi] = length(which(diff(which(OLEVELS[qqq1:qqq2] != 
                                                                                                                                                   1 & OLEVELS[qqq1:qqq2] != 
                                                                                                                                                   2)) > 1))
                                                                                                ds_names[fi] = "Nblocks_TINday"
                                                                                                fi = fi + 1
                                                                                                dsummary[di, fi] = boutcriter.in
                                                                                                ds_names[fi] = "boutcriter.in"
                                                                                                fi = fi + 1
                                                                                                dsummary[di, fi] = boutcriter.lig
                                                                                                ds_names[fi] = "boutcriter.lig"
                                                                                                fi = fi + 1
                                                                                                dsummary[di, fi] = boutcriter.mvpa
                                                                                                ds_names[fi] = "boutcriter.mvpa"
                                                                                                fi = fi + 1
                                                                                                dsummary[di, fi] = paste(boutdur.in, 
                                                                                                                         collapse = "_")
                                                                                                ds_names[fi] = "boutdur.in"
                                                                                                fi = fi + 1
                                                                                                dsummary[di, fi] = paste(boutdur.lig, 
                                                                                                                         collapse = "_")
                                                                                                ds_names[fi] = "boutdur.lig"
                                                                                                fi = fi + 1
                                                                                                dsummary[di, fi] = paste(boutdur.mvpa, 
                                                                                                                         collapse = "_")
                                                                                                ds_names[fi] = "boutdur.mvpa"
                                                                                                fi = fi + 1
                                                                                                dsummary[di, fi] = bout.metric
                                                                                                ds_names[fi] = "bout.metric"
                                                                                                fi = fi + 1
                                                                                                if (storefolderstructure == 
                                                                                                    TRUE) {
                                                                                                        dsummary[di, fi] = fullfilenames[i]
                                                                                                        ds_names[fi] = "filename_dir"
                                                                                                        fi = fi + 1
                                                                                                        dsummary[di, fi] = foldername[i]
                                                                                                        ds_names[fi] = "foldername"
                                                                                                        fi = fi + 1
                                                                                                }
                                                                                                di = di + 1
                                                                                        }
                                                                                }
                                                                        }
                                                                }
                                                        }
                                                }
                                        }
                                }
                                output = data.frame(dsummary, stringsAsFactors = FALSE)
                                names(output) = ds_names
                                whoareWW = which(output$window == "WW")
                                if (length(loglocation) > 0) {
                                        if (length(whoareWW) > 0) {
                                                whoareNOSL = which(output$sleeplog_used[whoareWW] == 
                                                                           FALSE)
                                                if (length(whoareNOSL) > 0) {
                                                        for (k23 in 1:length(whoareNOSL)) {
                                                                k24 = whoareWW[(whoareNOSL[k23] - 1)]
                                                                if (length(k24) > 0) {
                                                                        if (k24 > 0) {
                                                                                output$sleeplog_used[k24] = FALSE
                                                                        }
                                                                }
                                                        }
                                                }
                                        }
                                }
                                emptyrows = which(output[, 1] == "" & output[, 
                                                                             2] == "")
                                if (length(emptyrows) > 0) 
                                        output = output[-emptyrows, ]
                                lastcolumn = which(colnames(output) == "bout.metric")
                                if (ncol(output) > lastcolumn) {
                                        emptycols = which(output[1, ] == "" & output[2, 
                                                                                     ] == "")
                                        if (length(emptycols) > 0) 
                                                emptycols = emptycols[which(emptycols > 
                                                                                    lastcolumn)]
                                        if (length(emptycols) > 0) 
                                                output = output[-emptycols]
                                }
                                save(output, file = paste(metadatadir, ms5.out, 
                                                          "/", fnames.ms3[i], sep = ""))
                                rm(output, dsummary)
                        }
                }
        }
}

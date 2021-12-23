g.part5.addsib = function(ts,ws3, Nts, S2, desiredtz, j, nightsi) {
  #========================================================
  # SUSTAINED INACTIVITY BOUTS
  # These are stored in part 3 milestone data as start- and end-times
  # in the following code we convert the them into indices of the recording sequence
  
  ts$sibdetection = 0# initialize output vector that will hold the sibs
  s0s1 = c()
  # pr0 and pr1 define the indices relative the start of the recording
  # and specifying a 6 day time window 
  # always relative to the most recently processed sustained inactivity bout
  # s0 and s1 are the indices within that time window 
  # that match the start and end of the next sustained inactivity bout 
  pr0 = 1
  pr1 = pr0 + ((60/ws3)*1440*6)
  pr2 = Nts
  if (nrow(S2) > 0) {
    gik.ons = as.character(S2$sib.onset.time)
    gik.end = as.character(S2$sib.end.time)
    for (g in 1:nrow(S2)) { # sustained inactivity bouts
      lastpr0 = pr0
      pr1 = pr0 + ((60/ws3)*1440*6)
      if (pr1 > pr2) pr1 = pr2
      if (pr0 > pr1) pr0 = pr1
      #Coerce time into iso8601 format, so it is sensitive to daylight saving times when hours can be potentially repeated
      timebb = as.character(ts$time[pr0:pr1])
      if (is.ISO8601(timebb[1]) == FALSE) { # only do this for POSIX format
        timebb = POSIXtime2iso8601(timebb,tz=desiredtz)
      }
      s0 = which(timebb == gik.ons[g])[1]
      s1 = which(timebb == gik.end[g])[1]
      if ( timebb[1] != as.character(timebb[1])){ #not s0 because s0 does not exist yet if classes differ
        timebb = as.character(timebb)
        s0 = which(timebb == gik.ons[g])[1]
        s1 = which(timebb == gik.end[g])[1]
      }
      if (is.na(s0) == TRUE) s0 = which(timebb == paste(gik.ons[g]," 00:00:00",sep=""))[1]
      if (is.na(s1) == TRUE) s1 = which(timebb == paste(gik.end[g]," 00:00:00",sep=""))[1]
      # add pr0 to make s0 and s1 be relative to the start of the recording
      s0 = s0 + pr0 - 1
      s1 = s1 + pr0 - 1
      pr0 = s1
      if (length(s1) != 0 & length(s0) != 0 & is.na(s0) == FALSE & is.na(s1) == FALSE) {
        s0s1 = c(s0s1,s0:s1)
      } else {
        pr0 = lastpr0 + ((60/ws3)*1440*6)
      }
    }
  }
  ts$sibdetection[s0s1] = 1
  
  if (length(grep(pattern = "A", j)) > 0 & length(grep(pattern = "T", j)) > 0) {
    #=========
    # If excludefirstlast was set to TRUE in part 4 then 
    # SUSTAINED INACTIVITY BOUTS are not assessed for the time between
    # the first midnight and the first noon
    # because it is not used for sleep reports (first night is ignored in this case).
    # Therefore, expand the SIB sibdetection to include this time segment, for part 5 only.
    # This part of the code is only performed if using sib estimates based on vanHees2015
    #----------------------------------------------------------------------
    # Step 1: Identify time window that needs to be processed
    redo1 = nightsi[1] - ((60/ws3)*60) # 1 hour before first midnight
    if (redo1 < 1) redo1 = 1
    redo2 = nightsi[1] + (14*(60/ws3)*60) # 14 hours after first midngiht
    # Specify defintion of sustained inactivity bout
    anglethreshold = as.numeric(unlist(strsplit(j,"A"))[2])
    tempi = unlist(strsplit(unlist(strsplit(j,"A"))[1],"T"))
    timethreshold = as.numeric(tempi[length(tempi)])
    Nsleep = length(timethreshold) * length(anglethreshold)
    sleep = matrix(0,length(ts$angle[redo1:redo2]),Nsleep)
    ts$angle[which(is.na(ts$angle[redo1:redo2]) == T)] = 0
    sdl1 = rep(0,length(ts$time[redo1:redo2]))
    postch = which(abs(diff(ts$angle[redo1:redo2])) > anglethreshold) #posture change of at least j degrees
    # count posture changes that happen less than once per ten minutes
    q1 = c()
    if (length(postch) > 1) {
      q1 = which(diff(postch) > (timethreshold*(60/ws3))) #less than once per i minutes
    }
    if (length(q1) > 0) {
      for (gi in 1:length(q1)) {
        sdl1[postch[q1[gi]]:postch[q1[gi]+1]] = 1 #periods with no posture change
      }
    } else { #possibly a day without wearing
      if (length(postch) < 5) {  #possibly a day without wearing
        sdl1[1:length(sdl1)] = 1 #periods with no posture change
      } else {  #possibly a day with constantly posture changes
        sdl1[1:length(sdl1)] = 0 #periodsposture change
      }
    }
    # update variable sibdetection
    if (redo2 > Nts) {
      delta = redo2 - Nts
      redo2 = Nts
      sdl1 = sdl1[1:(length(sdl1)-delta)]
    }
    if (redo1 > Nts) {
      redo1 = Nts
      sdl1 = sdl1[1]
    }
    ts$sibdetection[redo1:redo2] = sdl1
  }
  return(ts)
}

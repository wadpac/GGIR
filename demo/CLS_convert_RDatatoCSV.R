rm(list=ls())
graphics.off()
#==================================================================
# INPUT NEEDED:
# setwd("D:/sharedfolder/first5") #<= the folder in which there is a folder named output_...
# setwd("D:/dropbox/Dropbox/Accelerometry/GGIR/development") #<= the folder in which there is a folder named output_...
path = "/media/windows-share/London/data_spring2017"
setwd(path)
studyname = "RDAfiles"
# studyname = "Millenium dress rehearsal"
# studyname = "first5"

exportrawdata = FALSE

source("/home/vincent/GGIR/mcs-acc/R/g.getbout.R")

#===================================================
# Define output directories:
outdir = "accelerometer_5second" # epoch data

newdir = paste0(path,"/output_",studyname,"/",outdir)
if (dir.exists(newdir) == FALSE) dir.create(newdir)

# Define which files need to beprocessed:
ufn = unique(dir(paste0(path,"/output_",studyname,"/raw")))

# Alternatively, only use data from files for which there is a corresponding diary:
# ann = read.csv("D:/dropbox/Dropbox/Accelerometry/GGIR/development/input_cls/data_annotations.csv")
# ufn = as.character(unique(ann$filename))
#---------------------------------------------------
# Extract identifiers of indiduals from filenames:
rm.dayfromname = function(x) {
  return(unlist(strsplit(x,"_day"))[1])
}
ufn2 = unique(unlist(lapply(ufn,rm.dayfromname)))
rm.RDatafromname = function(x) {
  return(unlist(strsplit(x,"[.]RDa"))[1])
}
path = paste0("output_",studyname,"/meta/ms2.out")
fnames = dir(path)
fnames2 = unique(unlist(lapply(fnames,rm.RDatafromname)))
rpath = paste0("output_",studyname,"/meta/raw")
rfiles = dir(rpath)
rfiles2 = unlist(lapply(rfiles,rm.dayfromname))

print("Load and export epoch data")
for (i in 1:length(fnames2)) {
  if (length(unlist(strsplit(fnames2[1],"_day"))) > 1) { # milestone data is already split by day because they were derived from raw RData files 
    fnames2_withday = fnames2
    fnames2_withoutday = unlist(lapply(fnames2,rm.dayfromname))
    file2read = fnames2_withday[which(fnames2_withoutday == ufn2[i])] # maximum 2 files
      if (length(file2read) > 0) {
      for (j in 1:length(file2read)) {
        load(paste0(path,"/",file2read[j],".RData"))
        invalid = IMP$rout[,5]
        invalid = rep(invalid,each=(IMP$windowsizes[2]/IMP$windowsizes[1]))
        NR = nrow(IMP$metashort)
        if (length(invalid) > NR) {
          invalid = invalid[1:NR]
        } else if (length(invalid) < NR) {
          invalid = c(invalid,rep(0,(NR-length(invalid))))
        }
        output = cbind(IMP$metashort,invalid)
        names(output)[2] = "acceleration"
        output$acceleration = as.numeric(as.character(output$acceleration))
        #=================================
        # also add conventional approach
        output$heuristic = 0
        abs_delta_angle = abs(diff(as.numeric(as.character(output$anglez))))
        ch = which(abs_delta_angle > 5) # index
        sl = which(diff(ch) > 12*5) # ch[index]
        if (length(sl) > 1) {
          for (g in 1:(length(sl)-1)) {
            output$heuristic[ch[sl[g]]:ch[sl[g]+1]] = 1 #sleep or SIB
            
          }
          output$acceleration[which(output$heuristic == 1)] = 0
        }
        OIN = which(output$acceleration < 0.040 & output$heuristic != 1)
        if (length(OIN) > 0) output$heuristic[OIN] = 2
        LIG = which(output$acceleration >= 0.040 & output$acceleration < 0.100 & output$heuristic != 1)
        if (length(LIG) > 0) output$heuristic[LIG] = 3
        MVPA = which(output$acceleration >= 0.100 & output$heuristic != 1)
        if (length(MVPA) > 0) output$heuristic[MVPA] = 4
        
        # 10 minute bouts of MVPA
        LN = nrow(output)
        rr1 = rep(0,LN)
        rr1[MVPA] = 1
        out1 = g.getbout(x=rr1,boutduration=10*12,boutcriter=0.8,
                         closedbout=FALSE,bout.metric=4,ws3=5)
        output$heuristic[which(out1$x == 1)] = 5
        
        # 1 minute bouts of MVPA
        rr1 = rep(0,LN)
        p = which(output$heuristic == 4)
        rr1[p] = 1
        out1 = g.getbout(x=rr1,boutduration=1*12,boutcriter=0.8,
                         closedbout=FALSE,bout.metric=4,ws3=5)
        output$heuristic[which(out1$x == 1)] = 6
        
        # 30 minute bouts of Inactivity
        rr1 = rep(0,LN)
        p = which(output$heuristic == 2)
        rr1[p] = 1
        out1 = g.getbout(x=rr1,boutduration=30*12,boutcriter=0.9,
                         closedbout=FALSE,bout.metric=4,ws3=5)
        output$heuristic[which(out1$x == 1)] = 7
        # # Bouts of Light
        # x11()
        # plot(output$heuristic,type="l")
        # KK
        #=================================
        
        
        day1 = output[1:(1440*12),]
        write.csv(day1,file=paste0(newdir,"/",file2read[j],".csv"),row.names = FALSE)
        
      }
    }
  } else { # scenario in which the milestone data is not split by day yet
    file2read = fnames2[i]
    if (length(file2read) > 0) {
      load(paste0(path,"/",file2read,".RData"))
      invalid = IMP$rout[,5]
      invalid = rep(invalid,each=(IMP$windowsizes[2]/IMP$windowsizes[1]))
      NR = nrow(IMP$metashort)
      if (length(invalid) > NR) {
        invalid = invalid[1:NR]
      } else if (length(invalid) < NR) {
        invalid = c(invalid,rep(0,(NR-length(invalid))))
      }
      output = cbind(IMP$metashort,invalid)
      names(output)[2] = "acceleration"
      if (nrow(output) > (1440 * 12)) {
        day1 = output[1:(1440*12),]
        day2 = output[(1440*12+1):nrow(output),]
        write.csv(day1,file=paste0(outdir,"/",file2read,"_day1.csv"),row.names = FALSE)
        write.csv(day2,file=paste0(outdir,"/",file2read,"_day2.csv"),row.names = FALSE)
      } else {
        day1 = output[1:(1440*12),]
        write.csv(day1,file=paste0(newdir,"/",file2read,"_day1.csv"),row.names = FALSE)
      }
    }
  }
}
if (exportrawdata == TRUE) {
  outdir2 = "accelerometer_40Hz" # raw data
  newdir = paste0(path,"/output_",studyname,"/",outdir2)
  if (dir.exists(newdir) == FALSE) dir.create(newdir)
  print("Load and export raw data")
  for (i in 1:length(ufn2)) {
    print(i)
    file2read = rfiles[which(rfiles2 == ufn2[i])]
    if (length(file2read) > 0) {
      for (j in 1:length(file2read)) {
        print(paste0(i,".",j))
        load(paste0(rpath,"/",file2read[j]))
        S = cbind(Gx,Gy,Gz) #,temperature,light
        names(S) = c("x","y","z")
        write.table(S,file=paste0(outdir2,"/",file2read[j],".csv"),row.names = FALSE,sep=",")
      }
    }
  }
}
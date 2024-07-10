# Developed by: Vincent van Hees
# Objective: Facilitate research on individualized cut-points

library(GGIR)

# specifiy locations of files
setwd("D:/myprojectfolder")

individual_cutpoints_file = "individual_cutpoints.csv" # the file with the individual cutpoints
datadir = "testfiles" # folder with accelerometer data
outputdir = "."
GGIRresultsfolder = "output_testfiles/results" # path to GGIR results

#=======================================
# 1) Extract individual cut-points
#=======================================

ISWTcp = read.csv(individual_cutpoints_file) # ISWT cut-points
ilevels = sort(unique(c(0, as.vector(as.matrix(ISWTcp[, which(tolower(colnames(ISWTcp)) != "id")])), 8000)))

#=======================================
# 2) Run GGIR part 1 and 2
#=======================================

# Replace by your own GGIR call, but make sure ilevels = ilevels is part of it
GGIR(datadir = datadir,
     outputdir = outputdir,
     mode = NULL,
     do.enmo = TRUE,
     do.mad = FALSE,
     idloc = 2,
     overwrite = TRUE,
     qwindow = c(0, 24),
     ilevels = ilevels,
     do.report = c(),
     visualreport = FALSE)

#=======================================
# 3) Combine columns based on individual cut-points
#=======================================

# declare functions
getMetrics = function(x) {
  tmp = unlist(strsplit(x, "_"))
  return(tmp[2])
}
getDaysegments = function(x) {
  tmp = unlist(strsplit(x, "_"))
  return(tmp[4])
}
getRange = function(x) {
  tmp = unlist(strsplit(x, "[.]"))
  if (tmp[3] == "8e") tmp[3] = "8000"
  tmp = as.numeric(tmp[2:3])
  return(tmp)
}
filterValues = function(x, metric, daySegment) {
  return(x[intersect(grep(pattern = metric, x = x), grep(pattern = daySegment, x = x))])
}

#---------------------------------------
# Day report
#---------------------------------------
part2_dayreport_file = paste0(GGIRresultsfolder, "/part2_daysummary.csv")
part2_dayreport = read.csv(file = part2_dayreport_file)
colindex = grep(pattern = "X[.]", x = colnames(part2_dayreport))
values1 = colnames(part2_dayreport)[colindex]

# Account for possible multiple metrics and/or multiple day segments
metrics = unique(mapply(values1, FUN = getMetrics))
daySegments = unique(mapply(values1, FUN = getDaysegments))
ilevel_names = NULL
for (metric in metrics) {
  for (daySegment in daySegments) {
    # Extract bin range per ilevel
    values = filterValues(values1, metric, daySegment)
    ilevel_range = mapply(values, FUN = getRange)
    # Loop over rows (days)
    D = matrix(NA, nrow(part2_dayreport), ncol(ISWTcp))
    for (j in 1:nrow(part2_dayreport)) {
      ID = part2_dayreport$ID[j]
      h = which(ISWTcp$ID == ID)
      
      TotalTime = sum(part2_dayreport[j, colindex], na.rm = TRUE)
      
      if (length(h) == 0) next # skip because ID not found in individual cut-point file
      # The break statements below are to break the loop when individual cut-point file has empty cell
      # Loop over rows in individual cutpoints overview
      for (i in 1:(ncol(ISWTcp) - 1)) {
        # Identify which columns in part2_daysummary need to be summed to get individualised level
        if (i == 1) {
          if (is.na(ISWTcp[h, i + 1])) {
            break
          }
          columns_of_interest = which(ilevel_range[1,] >= 0 & ilevel_range[2,] <= ISWTcp[h, i + 1])
        } else {
          if (is.na(ISWTcp[h, i])) {
            break
          }
          if (i + 1 <= ncol(ISWTcp)) {
            if (is.na(ISWTcp[h, i + 1])) {
              break
            }
            columns_of_interest = which(ilevel_range[1,] >= ISWTcp[h, i] & ilevel_range[2,] <= ISWTcp[h, i + 1])
          } else {
            columns_of_interest = which(ilevel_range[1,] >= ISWTcp[h, i])
          }
        }
        time_in_levels = part2_dayreport[j, columns_of_interest + (min(colindex) - 1)]
        if (!all(is.na(time_in_levels))) {
          D[j, i] = sum(time_in_levels, na.rm = TRUE)
        }
      }
      if (TotalTime != 0) {
        D[j, ncol(ISWTcp)] = round(TotalTime - sum(D[j, ], na.rm = TRUE), digits = 3)
      }
    }
    ilevel_names = c(ilevel_names, sub(pattern = "X[.]", replacement = "", x = colnames(ilevel_range)))
    D = as.data.frame(D) 
    colnames(D)[1:(ncol(ISWTcp) - 1)] = paste0(metric, "_Level_", 0:(ncol(ISWTcp) - 2), "_", 1:(ncol(ISWTcp) - 1), "_", daySegment)
    colnames(D)[ncol(ISWTcp)] = paste0(metric, "_above_highest_level_", daySegment)
    part2_dayreport = cbind(part2_dayreport, D)
  }
}
part2_dayreport_file_modified = paste0(unlist(strsplit(part2_dayreport_file, "[.]csv")), "_modified.csv")
data.table::fwrite(x = part2_dayreport, file = part2_dayreport_file_modified,
                   row.names = F, na = "")

#---------------------------------------
# Person report
#---------------------------------------
part2_personreport_file = paste0(GGIRresultsfolder, "/part2_summary.csv")
part2_personreport = read.csv(file = part2_personreport_file)

for (weeksegment in c("AD", "WE", "WD", "WWE", "WWD")) {
  P2 = part2_personreport[grep(pattern = paste0(weeksegment, "|ID"), x = colnames(part2_personreport))]
  colindex = grep(pattern = paste0(ilevel_names, collapse = "|"), x =  colnames(P2))
  values1 = colnames(P2)[colindex]
  for (metric in metrics) {
    for (daySegment in daySegments) {
      values = filterValues(values1, metric, daySegment)
      ilevel_range = mapply(values, FUN = getRange)
      D = matrix(NA, nrow(P2), ncol(ISWTcp))
      for (j in 1:nrow(P2)) {
        ID = P2$ID[j]
        h = which(ISWTcp$ID == ID)
        TotalTime = sum(P2[j, colindex], na.rm = TRUE)
        if (length(h) == 0) next # skip because ID not found in individual cut-point file
        # The break statements below are to break the loop when individual cut-point file has empty cell
        for (i in 1:(ncol(ISWTcp) - 1)) {
          if (i == 1) {
            if (is.na(ISWTcp[h, i + 1])) {
              break
            }
            columns_of_interest = which(ilevel_range[1,] >= 0 & ilevel_range[2,] <= ISWTcp[h, i + 1])
          } else {
            if (is.na(ISWTcp[h, i])) {
              break
            }
            if (i + 1 <= ncol(ISWTcp)) {
              if (is.na(ISWTcp[h, i + 1])) {
                break
              }
              columns_of_interest = which(ilevel_range[1,] >= ISWTcp[h, i] & ilevel_range[2,] <= ISWTcp[h, i + 1])
            } else {
              columns_of_interest = which(ilevel_range[1,] >= ISWTcp[h, i])
            }
          }
          time_in_levels = P2[j, columns_of_interest + (min(colindex) - 1)]
          if (!all(is.na(time_in_levels))) {
            D[j, i] = sum(time_in_levels, na.rm = TRUE)
          }
        }
        if (TotalTime != 0) {
          D[j, ncol(ISWTcp)] = round(TotalTime - sum(D[j, ], na.rm = TRUE), digits = 3)
        }
      }
      D = as.data.frame(D) 
      colnames(D)[1:(ncol(ISWTcp) - 1)] = paste0(weeksegment, "_", metric, "_Level_", 0:(ncol(ISWTcp) - 2), "_", 1:(ncol(ISWTcp) - 1), "_", daySegment)
      colnames(D)[ncol(ISWTcp)] = paste0(weeksegment, "_", metric, "_above_highest_level_", daySegment)
      part2_personreport = cbind(part2_personreport, D)
    }
  }
}

part2_personreport_file_modified = paste0(unlist(strsplit(part2_personreport_file, "[.]csv")), "_modified.csv")
data.table::fwrite(x = part2_personreport, file = part2_personreport_file_modified,
                   row.names = F, na = "")


# Review the data dictionaries of all GGIR reports (for GGIR developers)
#
# Runs GGIR parts 1 to 6 on simulated data with as many output variables as
# possible, generates the data dictionaries in results/variableDictionary and
# prints which variables have no definition. Use it to review the definitions
# after adding or renaming report variables. The automatic check that every
# variable has a definition is part of tests/testthat/test_integration.R.
#
# Usage: run from the root of the GGIR repository, e.g.
#   source("dev-functions/reviewDictionaries.R")
# It loads GGIR with devtools::load_all(), so uncommitted changes are included.
# Runtime: about 1-2 minutes.

ggir_dir = "."                                       # root of the GGIR repository
outdir = file.path(tempdir(), "ggir_dictionaries")   # change to keep the output

devtools::load_all(ggir_dir, quiet = TRUE)
if (dir.exists(outdir)) unlink(outdir, recursive = TRUE)
dir.create(outdir, recursive = TRUE)
oldwd = setwd(outdir)

# 1. Simulated data: 4 days of accelerometer data (Thursday to Sunday, so
#    that weekdays and weekend days are included)
#    and an advanced sleep diary (with naps and non-wear) -----------------------
create_test_acc_csv(Nmin = 4 * 1440)
create_test_sleeplog_csv(advanced = TRUE)

# 2. External function with one output of each report type ----------------------
#    step_count (event), activity_type (type) and intensity (scalar).
#    These are dummy values, only meant to generate the report columns.

dummyExtFun = function(data, parameters) {
  sf = parameters
  N = floor(nrow(data) / sf)                # one value per second
  mag = sqrt(rowSums(data[1:(N * sf), 1:3]^2))
  sec = rep(1:N, each = sf)
  sdmag = tapply(mag, sec, sd)
  steps = ifelse(sdmag > 0.02, 2, 0)        # 2 steps per second when moving
  type = ifelse(sdmag > 0.1, "walking", ifelse(sdmag > 0.02, "standing", "sitting"))
  data.frame(step_count = steps, activity_type = type,
             intensity = round(sdmag * 1000), stringsAsFactors = FALSE)
}

myfun = list(FUN = dummyExtFun,
             parameters = 30,
             expected_sample_rate = 30,
             expected_unit = "g",
             colnames = c("step_count", "activity_type", "intensity"),
             outputres = 1,
             minlength = 1,
             outputtype = c("numeric", "character", "numeric"),
             aggfunction = list(sum, function(x) names(which.max(table(x))), mean),
             timestamp = FALSE,
             reporttype = c("event", "type", "scalar"),
             name = "step",
             # event (step and cadence) settings for the part 2 event report
             ilevels = c(0, 50, 100),
             clevels = c(0, 30, 100),
             qlevels = c(0.25, 0.5, 0.75),
             ebout.dur = c(1, 5, 10),
             ebout.th.cad = 30,
             ebout.th.acc = 50,
             ebout.criter = 0.8,
             ebout.condition = "AND")

# 3. Run GGIR parts 1 to 6 with most optional outputs switched on ---------------
GGIR(datadir = "123A_testaccfile.csv", 
     outputdir = outdir, 
     studyname = "dict",
     mode = 1:6, 
     do.report = c(2, 4, 5, 6), 
     overwrite = TRUE,
     desiredtz = "Europe/London", 
     idloc = 2, 
     minimumFileSizeMB = 0,
     do.parallel = FALSE, 
     verbose = FALSE, 
     myfun = myfun,
     # part 1: enmo, mad and anglez
     do.enmo = TRUE, 
     do.mad = TRUE, 
     do.anglez = TRUE,
     # part 2: distribution, intensity gradient, LX/MX, MVPA, day segments, cosinor
     qlevels = c(0.5, 0.9), 
     ilevels = c(0, 50, 100), 
     iglevels = TRUE,
     winhr = c(5, 10), 
     qM5L5 = 0.5, 
     mvpathreshold = 100,
     boutcriter = 0.8, 
     mvpadur = c(1, 5, 10), 
     qwindow = c(0, 12, 24),
     cosinor = TRUE, 
     epochvalues2csv = FALSE, 
     includedaycrit = 0,
     # part 3 and 4: sleep diary with naps and non-wear
     loglocation = "testsleeplogfile.csv", 
     colid = 1, 
     coln1 = 2, 
     nnights = 7,
     sleepwindowType = "SPT", 
     includenightcrit = 0,
     # part 5: bouts, fragmentation, research variables, naps
     threshold.lig = 40, threshold.mod = 100, threshold.vig = 400,
     boutdur.mvpa = c(1, 5, 10), boutdur.in = c(10, 20, 30), boutdur.lig = c(1, 10),
     frag.metrics = "all", 
     part5_agg2_60seconds = TRUE, 
     includedaycrit.part5 = 0,
     timewindow = c("MM", "WW"), 
     do.sibreport = TRUE,
     possible_nap_window = c(9, 18), 
     possible_nap_dur = c(15, 240),
     method_research_vars = "nap", 
     save_ms5rawlevels = TRUE,
     # part 6: circadian rhythm and fragmentation over the full recording
     part6CR = TRUE, 
     part6Window = c("start", "end"),
     visualreport = FALSE, 
     do.visual = FALSE)

# 4. Overview of the dictionaries -------------------------------------------------
dictdir = file.path(outdir, "output_dict", "results", "variableDictionary")
files = dir(dictdir, full.names = TRUE)

# One data.frame per dictionary, e.g. View(dictionaries$part2_dictionary_summary)
dictionaries = lapply(files, data.table::fread, data.table = FALSE)
names(dictionaries) = sub("\\.csv$", "", basename(files))

# Variables without a definition, per dictionary
undefined = lapply(dictionaries, function(D) {
  D$Variable[is.na(D$Definition) | D$Definition == ""]
})

overview = data.frame(dictionary = names(dictionaries),
                      N_variables = sapply(dictionaries, nrow),
                      N_undefined = lengths(undefined), row.names = NULL)
print(overview)

undefined = undefined[lengths(undefined) > 0]
if (length(undefined) > 0) {
  cat("\nVariables without a definition:\n")
  for (d in names(undefined)) cat("\n", d, ":\n  ", paste(undefined[[d]], collapse = "\n  "), "\n", sep = "")
}

# All dictionaries in one table, e.g. to browse with View(all_dictionaries)
all_dictionaries = do.call(rbind, lapply(names(dictionaries), function(d) {
  cbind(dictionary = d, dictionaries[[d]])
}))

data.table::fwrite(all_dictionaries, file.path(outdir, "all_dictionaries.csv"))

cat("\nDictionaries in:", dictdir,
    "\nAll in one file:", file.path(outdir, "all_dictionaries.csv"), "\n")

# reports without a dictionary (e.g. new report types)
reports = c(dir(file.path(outdir, "output_dict", "results"), pattern = "csv$"),
            dir(file.path(outdir, "output_dict", "results", "QC"), pattern = "csv$"))

cat("\nReports in results and results/QC:\n"); print(reports)

setwd(oldwd)

g.report.part5_dictionary = function(metadatadir, params_output) {
  # internal function to get class ------------------------------------------
  getclass = function(x) {
    if (length(x) == 0) return(NULL)
    class = NULL
    if (grepl("2", x)) x = unlist(strsplit(x, "2"))
    for (i in 1:length(x)) {
      if (x[i] == "IN") class[i] = "inactivity"
      if (x[i] == "LIG" | x[i] == "LIPA") class[i] = "light physical activity"
      if (x[i] == "MOD") class[i] = "moderate physical activity"
      if (x[i] == "VIG") class[i] = "vigorous physical activity"
      if (x[i] == "MVPA") class[i] = "moderate-to-vigorous physical activity"
      if (x[i] == "PA") class[i] = "physical activity"
      if (x[i] == "sleep") class[i] = "sleep"
      if (x[i] == "wake") class[i] = "awake"
      if (x[i] == "nonwear") class[i] = "non-wear time"
      if (x[i] == "nap") class[i] = "naps (detected with the possible_nap_* criteria)"
      if (x[i] == "mostactive60min") class = "the most active 60 minutes of the day are accumulated"
      if (x[i] == "mostactive30min") class = "the most active 30 minutes of the day are accumulated"
    }
    if (length(class) == 2) class = paste(class[1], "to", class[2])
    return(class)
  }
  # definitions for variables not covered by baseDictionary -----------------
  # naps and rest periods (g.part5.analyseRest), denap = accelerometer-detected
  # nap (sustained inactivity bout meeting the possible_nap_* criteria),
  # srnap = self-reported nap, srnonw = self-reported non-wear
  extraDictionary = list(
    sibreport_n_items = "Number of items in the rest report (detected naps meeting the possible_nap_* criteria and self-reported naps and non-wear periods) in the full recording",
    sibreport_n_items_day = "Number of items in the rest report (detected naps meeting the possible_nap_* criteria and self-reported naps and non-wear periods) in the window",
    nbouts_day_denap = "Number of accelerometer-detected naps (sustained inactivity bouts meeting the possible_nap_* criteria) in the window",
    nbouts_day_srnap = "Number of self-reported naps in the window",
    nbouts_day_srnonw = "Number of self-reported non-wear periods in the window",
    noverl_denap_srnap = "Number of accelerometer-detected naps that overlap with a self-reported nap",
    noverl_denap_srnonw = "Number of accelerometer-detected naps that overlap with a self-reported non-wear period",
    noverl_srnap_denap = "Number of self-reported naps that overlap with an accelerometer-detected nap",
    noverl_srnonw_denap = "Number of self-reported non-wear periods that overlap with an accelerometer-detected nap",
    frag_mean_dur_denap_day = "Mean duration of the accelerometer-detected naps in the window (minutes)",
    dur_day_denap_min = "Total duration of the accelerometer-detected naps in the window (minutes)",
    frag_mean_dur_srnap_day = "Mean duration of the self-reported naps in the window (minutes)",
    dur_day_srnap_min = "Total duration of the self-reported naps in the window (minutes)",
    frag_mean_dur_srnonw_day = "Mean duration of the self-reported non-wear periods in the window (minutes)",
    dur_day_srnonw_min = "Total duration of the self-reported non-wear periods in the window (minutes)",
    mdur_denap_overl_srnap = "Mean duration of the accelerometer-detected naps that overlap with a self-reported nap (minutes)",
    tdur_denap_overl_srnap = "Total duration of the accelerometer-detected naps that overlap with a self-reported nap (minutes)",
    perc_denap_overl_srnap = "Percentage of the duration of the accelerometer-detected naps that overlaps with self-reported naps (%)",
    mdur_srnap_overl_denap = "Mean duration of the self-reported naps that overlap with an accelerometer-detected nap (minutes)",
    tdur_srnap_overl_denap = "Total duration of the self-reported naps that overlap with an accelerometer-detected nap (minutes)",
    perc_srnap_overl_denap = "Percentage of the duration of the self-reported naps that overlaps with accelerometer-detected naps (%)",
    mdur_denap_overl_srnonw = "Mean duration of the accelerometer-detected naps that overlap with a self-reported non-wear period (minutes)",
    tdur_denap_overl_srnonw = "Total duration of the accelerometer-detected naps that overlap with a self-reported non-wear period (minutes)",
    perc_denap_overl_srnonw = "Percentage of the duration of the accelerometer-detected naps that overlaps with self-reported non-wear periods (%)",
    mdur_srnonw_overl_denap = "Mean duration of the self-reported non-wear periods that overlap with an accelerometer-detected nap (minutes)",
    tdur_srnonw_overl_denap = "Total duration of the self-reported non-wear periods that overlap with an accelerometer-detected nap (minutes)",
    perc_srnonw_overl_denap = "Percentage of the duration of the self-reported non-wear periods that overlaps with accelerometer-detected naps (%)",
    nap_count = "Number of naps classified with nap_model during the waking time",
    nap_totalduration = "Total duration of the naps classified with nap_model during the waking time (minutes)",
    # other variables
    sleepparam = "Definition of sustained inactivity bouts used (T = time threshold in minutes, A = angle threshold in degrees)",
    filename_dir = "Full path of the file (only if storefolderstructure = TRUE)",
    foldername = "Name of the folder containing the file (only if storefolderstructure = TRUE)",
    GGIRversion = "GGIR version used to process this recording in part 5 (stored in the milestone data), which may differ from the version used to generate the report",
    Nmarkerbutton_used = "Number of valid windows in which the marker button was used to estimate the sleep period time",
    Nvaliddays_AL10F = "Number of valid days with at least 10 fragments (fragmentation analysis)",
    Nvalidsegments_AL10F = "Number of valid segments with at least 10 fragments (fragmentation analysis)",
    Nvalidsegments_AL10F_WD = "Number of valid weekday segments with at least 10 fragments (fragmentation analysis)",
    Nvalidsegments_AL10F_WE = "Number of valid weekend day segments with at least 10 fragments (fragmentation analysis)"
  )
  # -------------------------------------------------------------------------
  # main script -------------------------------------------------------------
  # -------------------------------------------------------------------------
  # identify individual reports
  reports = dir(file.path(metadatadir, "results"), full.names = TRUE, 
                pattern = "^part5.*\\.csv$")
  # Select one daysummary, personsummary, and Segment report as variables are the same across configurations
  ds = grep("^part5_daysummary", basename(reports))[1]
  ps = grep("^part5_personsummary", basename(reports))[1]
  reports = reports[c(ds, ps)]
  if (length(reports) == 0 || all(is.na(reports))) {
    # No cleaned part 5 report probably because no valid windows, so try full report instead
    reports = dir(file.path(metadatadir, "results", "QC"), full.names = TRUE, 
                    pattern = "^part5.*\\.csv$")
    if (length(reports) == 0 || all(is.na( reports))) return()
  }
  reports = reports[!is.na(reports)]
  if (!exists("baseDictionary")) return()
  generic_defs = c() # definitions of the main reports, reused for the type reports
  # read col names of each report and derive definitions
  for (ri in 1:length(reports)) {
    # read report
    R = data.table::fread(reports[ri], verbose = FALSE, nrows = 2)
    cnames = colnames(R)
    # initialise dictionary
    dictionary = data.frame(Variable = cnames,
                            Definition = NA)
    # get definitions
    for (coli in 1:length(cnames)) {
      # definitions will be a combination of what, window, when (only for LUX segments), class, unit
      what = window = class = unit = NULL
      # split up variable name
      elements = unlist(strsplit(cnames[coli], "[.]|_"))
      # get variable names from baseDictionary
      nam = gsub("_pla$|_wei$|_WD$|_WE$", "", cnames[coli])
      if (grepl("Nvalid", cnames[coli])) nam = cnames[coli]
      if (nam %in% names(baseDictionary)) {
        what = baseDictionary[[nam]]
      } else if (nam %in% names(extraDictionary)) {
        what = extraDictionary[[nam]]
      }
      # if the variable is not in baseDictionary, extract definition:
      if (is.null(what)) {
        # split up variable name 
        elements = unlist(strsplit(cnames[coli], "[.]|_"))
        # WHAT -------------------------------------------------------------
        if ("dur" %in% elements | "nonwear" %in% elements) {
          what = "Time accumulated"
        } else if ("ACC" %in% elements) {
          what = "Mean acceleration"
          if ("median" %in% elements) {
            what = "Median acceleration"
          } else if ("stdev" %in% elements) {
            what = "Standard deviation of acceleration"
          }
        } else if ("Nbouts" %in% elements) {
          what = "Number of bouts"
        } else if ("Nblocks" %in% elements) {
          what = "Number of blocks (defined as consecutive series of epochs with the same behavioural class)"
        } else if ("quantile" %in% elements) {
          what = "Acceleration above which (percentile)"
        } else if ("LUX" %in% elements) {
          if ("min" %in% elements | "above1000" %in% elements) {
            # time in lux ranges
            what = "Time accumulated with LUX"
            if ("above1000" %in% elements) {
              numbers = 1000
            } else {
              numbers = suppressWarnings(as.numeric(elements))
              numbers = numbers[!is.na(numbers)]
            }
            if (any(numbers == Inf) | length(numbers) == 1) {
              thresholds = paste("above", numbers[1])
            } else if (length(numbers) == 2) {
              thresholds = paste("between", numbers[1], "and", numbers[2])
            } else {
              thresholds = ""
            }
            what = paste(what, thresholds, "luxes")
            unit = "(minutes)"
          } else if ("mean" %in% elements) {
            what = "Mean LUX value"
            if ("mvpa" %in% elements) what = paste(what, "in moderate-to-vigorous physical activity time")
            unit = "(luxes)"
          } else if ("max" %in% elements) {
            what = "Maximum LUX value"
            unit = "(luxes)"
          } else if ("timeawake" %in% elements) {
            what = "Time classified as awake"
            unit = "(minutes)"
          } else if ("imputed" %in% elements) {
            what = "Time in which the LUX has been imputed"
            unit = "(minutes)"
          } else if ("ignored" %in% elements) {
            what = "Time in which the LUX has been ignored"
            unit = "(minutes)"
          }
          # lux segments
          if (any(grepl("hr", elements))) {
            t1 = elements[grep("hr", elements)]; t1 = gsub("hr", ":00", t1)
            t0 = elements[grep("hr", elements) - 1]
            if (nchar(t0) <= 2) t0 = paste0(t0, ":00")
            what = paste(what, "in the segment from", t0, "to", t1)
          }
        }
        # fragmentation metrics
        if ("FRAG" %in% elements) {
          what_bu = tolower(what)
          what = "Fragmentation analysis:"
          if ("TP" %in% elements) {
            what = paste(what, "transition probability (%)")
          } else if ("Nfrag" %in% elements) {
            what = paste(what, "number of fragments")
          } else if ("NFragPM" %in% elements) {
            what = paste(what, "number of fragments per minute")
          } else if ("mean" %in% elements) {
            what = paste(what, "mean duration in the")
          } else if ("Gini" %in% elements) {
            what = paste(what, "Gini inequality index as calculated in the ineq R package")
          } else if ("CoV" %in% elements) {
            what = paste(what, "Coefficient of variance as proposed in https://shorturl.at/nsDU9")
          } else if ("alpha" %in% elements) {
            what = paste(what, "Alpha power law exponent metric as proposed in https://shorturl.at/gwzB8")
          } else if ("x0" %in% elements) {
            what = paste(what, "x0.5 power law exponent metric as proposed in https://shorturl.at/gwzB8")
          } else if ("W0" %in% elements) {
            what = paste(what, "W0.5 power law exponent metric as proposed in https://shorturl.at/gwzB8")
          } else if ("SD" %in% elements) {
            what = paste(what, "standard deviation in the")
          }
          what = paste(what, what_bu)
        }
        # WINDOW -----------------------------------------------------------
        if ("day" %in% elements & "spt" %in% elements) {
          window = "during the waking time and sleep period time (i.e., full window)"
        } else if ("day" %in% elements) {
          window = "during the waking time"
        } else if ("spt" %in% elements) {
          window = "during the sleep period time"
        }
        # CLASS ------------------------------------------------------------
        classes = c("IN", "LIG", "LIPA", "MOD", "VIG", "MVPA", "PA", 
                    "sleep", "nonwear", "nap", "mostactive60min", "mostactive30min",
                    "IN2PA", "PA2IN", "IN2LIPA", "IN2MVPA", "sleep2wake", "wake2sleep")
        x = elements[which(elements %in% classes)]
        class = getclass(x)
        # sleep efficiency (overwrite prev classification as sleep)
        if ("sleep" %in% elements & "efficiency" %in% elements) {
          class = "Sleep efficiency after onset"
          unit = "(%)"
        }
        # wakefulness after sleep onset (overwrite prev classification of intensity)
        if ("wake" %in% elements) class = paste("awake", class)
        # bouts
        if ("bts" %in% elements) {
          numbers = suppressWarnings(as.numeric(elements))
          boutdur = numbers[!is.na(numbers)]
          # the longest bout category has no upper limit, e.g. bts_10 = 10 min or longer
          if (length(boutdur) == 1) {
            boutdur_text = paste("at least", boutdur, "min")
          } else {
            boutdur_text = paste(paste(boutdur, collapse = "-"), "min")
          }
          if ("Nbouts" %in% elements) {
            what = paste("Number of bouts of", boutdur_text, class)
            class = NULL
          } else {
            class = paste("bouts of", boutdur_text, class)
          }
        }
        # unbouted time
        if ("unbt" %in% elements & !("Nbouts" %in% elements)) {
          intensity = elements[which(elements %in% c("IN", "LIG", "MOD", "VIG"))]
          if (intensity %in% c("MOD", "VIG")) intensity = "MVPA"
          look4 = paste("dur_day", intensity, "bts", sep = "_")
          boutVars = grep(look4, cnames, value = TRUE)
          boutdurs = c()
          for (i in 1:length(boutVars)) {
            x = unlist(strsplit(boutVars[i], split = "_"))
            numbers = suppressWarnings(as.numeric(x))
            boutdurs = c(boutdurs, numbers[!is.na(numbers)])
          }
          minboutdur = min(boutdurs)
          class = paste("unbouted", class, paste0("(",0,"-",minboutdur, " min)"))
        }
        # intensity gradient
        if ("ig" %in% elements) {
          if ("gradient" %in% elements) class = "Intensity gradient"
          if ("rsquared" %in% elements) class = paste("R-squared from the log-log time to intensity regression to calculate the intensity gradient")
          if ("intercept" %in% elements) class = paste("Intercept from the log-log time to intensity regression to calculate the intensity gradient")
        }
        # connector (in)
        if (!is.null(class)) {
          if (class != "Sleep efficiency after onset" 
              & substr(class, 1, 8) != "the most"
              & !("ig" %in% elements)) class = paste("in", class)
        }
        # UNITS -------------------------------------------------------------------
        if ("min" %in% elements) {
          unit = "(minutes)"
        } else if ("mg" %in% elements) {
          unit = "(mili-gravity units)"
        } else if ("perc" %in% elements) {
          unit = "(%)"
        }
        # ------------------------------------------------------------------
        # Rest of column names...
        if (is.null(what)) {
          if ("sleeponset" %in% elements | "wakeup" %in% elements) {
            if ("sleeponset" %in% elements) what = "Sleep onset time"
            if ("wakeup" %in% elements) what = "Wake up time"
            if ("ts" %in% elements) {
              unit = "(hh:mm:ss)"
            } else {
              unit = "(hours from previous midnight)"
            }
          } else if ((substr(elements[1], 1, 1) == "L" | substr(elements[1], 1, 1) == "M")
                     & elements[1] != "LUX") {
            # LX and MX metrics
            # what
            if (grepl("TIME", elements[1])) {
              what = "Starting time"
              unit = "(timestamp)"
              if ("num" %in% elements) {
                unit = "(timestamp)"
              }
            } else if (grepl("VALUE", elements[1])) {
              what = "Mean acceleration"
              unit = "(mili-gravity units)"
            } else if ("peakLUX" %in% elements) {
              if ("mean" %in% elements) {
                what = "Mean peak Lux"
              } else if ("max" %in% elements) {
                what = "Max peak Lux"
              }
            }
            # class
            X = as.numeric(gsub("\\D", "", elements))[1]
            if (substr(elements[1], 1, 1) == "L") {
              class = paste("during the", X, "consecutive hours with the lowest acceleration")
            } else if (substr(elements[1], 1, 1) == "M") {
              class = paste("during the", X, "consecutive hours with the highest acceleration")
            }
          } else if ("daytype" %in% elements) {
            what = "WD = weekday; WE = weekend day"
          } else {
            what = ""
          } 
        } 
      }
      # build up definition
      def = paste(what, class, window, unit)
      # only for personsummary - aggregation method
      if (grepl("personsummary", reports[ri])) {
        agg = NULL
        if (!grepl("Nvalid", cnames[coli])) {
          if ("pla" %in% elements) agg = "- plain average"
          if ("wei" %in% elements) agg = "- weighted average"
          if ("WD" %in% elements) agg = "- weekdays average"
          if ("WE" %in% elements) agg = "- weekend days average"
        } else {
          # adapt definition of valid days to part 5 reports
          def = gsub("part 2, 4, and 5", "part 5", def)
        }
        def = paste(def, agg)
      }
      def = gsub("\\s+", " ", def) # this removes extra spaces
      def = gsub("^ ", "", def) # this removes leading spaces
      
      # if window and class have not been identified, then return blank definition
      if (is.null(what) & is.null(class)) def = NA
      
      # store definition
      dictionary[coli, "Definition"] = def
    }
    # write csv reports with data dictionaries
    directory = file.path(metadatadir, "results", "variableDictionary/")
    if (!dir.exists(directory)) dir.create(directory)
    fn = gsub("part5_", "part5_dictionary_", basename(reports[ri]))
    fn = unlist(strsplit(fn, "_MM|_WW|_OO"))[1]
    fn = paste0(fn, ".csv")
    data.table::fwrite(dictionary, file = file.path(directory, fn), 
                       row.names = FALSE, na = "", sep = params_output[["sep_reports"]],
                       dec = params_output[["dec_reports"]])
    generic_defs = c(generic_defs, structure(dictionary$Definition, names = dictionary$Variable))
  }
  # -------------------------------------------------------------------------
  # reports of external function outputs with reporttype "type":
  # part5_extfuntype_<output>_daysummary_* and part5_extfuntype_<output>_personsummary_*
  typereports = dir(file.path(metadatadir, "results"), full.names = TRUE,
                    pattern = "^part5_extfuntype_.*_(daysummary|personsummary)_.*\\.csv$")
  if (length(typereports) == 0) return()
  typeoutputs = unique(sub("^part5_extfuntype_(.+)_(daysummary|personsummary)_.*$", "\\1",
                           basename(typereports)))
  for (tm in typeoutputs) {
    for (rtype in c("daysummary", "personsummary")) {
      # one dictionary per output and report type, as variables are the same across configurations
      report = grep(paste0("^part5_extfuntype_", tm, "_", rtype, "_"), basename(typereports))[1]
      if (is.na(report)) next
      R = data.table::fread(typereports[report], verbose = FALSE, nrows = 2)
      cnames = colnames(R)
      dictionary = data.frame(Variable = cnames, Definition = NA)
      for (coli in 1:length(cnames)) {
        x = cnames[coli]
        agg = ""
        if (rtype == "personsummary") {
          suffix = regmatches(x, regexpr("_(pla|wei|WD|WE)$", x))
          if (length(suffix) == 1) {
            agg = switch(suffix, "_pla" = " - plain average", "_wei" = " - weighted average",
                         "_WD" = " - weekdays average", "_WE" = " - weekend days average")
            x = sub("_(pla|wei|WD|WE)$", "", x)
          }
        }
        def = NA
        typepattern = paste0("^", tm, "_(dur|ACC|Nblocks|Nbouts)_(day|spt)_(.+)$")
        if (grepl(typepattern, x)) {
          what = sub(typepattern, "\\1", x)
          window = sub(typepattern, "\\2", x)
          r = sub("_(min|mg)$", "", sub(typepattern, "\\3", x))
          if (grepl("^total_", r)) {
            category = sub("^total_", "", r)
            boutText = "in all time"
          } else if (grepl("_unbt$", r)) {
            category = sub("_unbt$", "", r)
            boutText = "outside bouts (unbouted time)"
          } else if (grepl("_bts_[0-9.]+(_[0-9.]+)?$", r)) {
            category = sub("_bts_[0-9.]+(_[0-9.]+)?$", "", r)
            bts = unlist(strsplit(sub("^.*_bts_", "", r), "_"))
            boutText = ifelse(length(bts) == 1,
                              paste0("in bouts of at least ", bts[1], " min"),
                              paste0("in bouts of ", bts[1], " to ", bts[2], " min"))
          } else {
            category = r
            boutText = ""
          }
          if (what == "Nbouts") {
            boutText = sub("^in bouts of ", "", boutText)
            start = paste0("Number of bouts of ", boutText, " of time")
            boutText = ""
          } else {
            start = switch(what, dur = "Time (minutes)",
                           ACC = "Mean acceleration (mg) during the time",
                           Nblocks = "Number of uninterrupted blocks of time")
          }
          def = paste0(start, " classified as '", category, "' by external function output ", tm, " ",
                       boutText, ", ",
                       ifelse(window == "day", "during the waking hours (outside the sleep period time)",
                              "during the sleep period time"), agg)
          def = gsub("\\s+", " ", gsub(" ,", ",", def))
        } else if (cnames[coli] %in% names(generic_defs)) {
          def = generic_defs[[cnames[coli]]]
        } else if (x %in% names(generic_defs)) {
          def = paste0(generic_defs[[x]], agg)
        }
        dictionary[coli, "Definition"] = def
      }
      fn = paste0("part5_dictionary_extfuntype_", tm, "_", rtype, ".csv")
      data.table::fwrite(dictionary, file = file.path(directory, fn),
                         row.names = FALSE, na = "", sep = params_output[["sep_reports"]],
                         dec = params_output[["dec_reports"]])
    }
  }
}

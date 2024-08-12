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
      if (x[i] == "mostactive60min") class = "the most active 60 minutes of the day are accumulated"
      if (x[i] == "mostactive30min") class = "the most active 30 minutes of the day are accumulated"
    }
    if (length(class) == 2) class = paste(class[1], "to", class[2])
    return(class)
  }
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
  if (length(reports) == 0 || all(is.na( reports))) {
    # No cleaned part 5 report probably because no valid windows, so try full report instead
    reports = dir(file.path(metadatadir, "results", "QC"), full.names = TRUE, 
                    pattern = "^part5.*\\.csv$")
    if (length(reports) == 0 || all(is.na( reports))) return()
  }
  reports = reports[!is.na(reports)]
  if (!exists("baseDictionary")) return()
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
      # get variable names from baseDictionary
      nam = gsub("_pla|_wei|_WD|_WE", "", cnames[coli])
      if (grepl("Nvalid", cnames[coli])) nam = cnames[coli]
      if (nam %in% names(baseDictionary)) {
        what = baseDictionary[[nam]]
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
            if (nchar(t0) == 1) t0 = paste0(t0, ":00")
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
                    "sleep", "nonwear", "mostactive60min", "mostactive30min",
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
        if ("bts" %in% elements & !("Nbouts" %in% elements)) {
          numbers = suppressWarnings(as.numeric(elements))
          boutdur = numbers[!is.na(numbers)]
          class = paste("bouts of", paste(boutdur, collapse = "-"), "min", class)
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
            if ("wakeup" %in% elements) what = "Sleep onset time"
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
  }
}

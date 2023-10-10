g.report.part5_dictionary = function(metadatadir, sep_reports = ",") {
  # identify individual reports
  reports = dir(file.path(metadatadir, "results"), full.names = TRUE, 
                pattern = "^part5.*\\.csv$")
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
      # definitions will be a combination of what, window, class, unit
      what = window = class = unit = NULL
      # get variable names from baseDictionary
      nam = gsub("_pla|_wei|_WD|_WE", "", cnames[coli])
      if (cnames[coli] %in% c("Nvaliddays_WD", "Nvaliddays_WE")) nam = cnames[coli]
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
          what = "Number of blocks"
        } else if ("quantile" %in% elements) {
          what = "Acceleration above which (percentile)"
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
        if ("IN" %in% elements) {
          class = "inactivity"
        } else if ("LIG" %in% elements) {
          class = "light physical activity"
        } else if ("MOD" %in% elements) {
          class = "moderate physical activity"
        } else if ("VIG" %in% elements) {
          class = "vigorous physical activity"
        } else if ("MVPA" %in% elements) {
          class = "moderate-to-vigorous physical activity"
        } else if ("sleep" %in% elements) {
          class = "sleep"
          if ("efficiency" %in% elements) {
            class = "Sleep efficiency"
            unit = "(%)"
          }
        } else if ("nonwear" %in% elements) {
          class = "non-wear time"
        } else if ("mostactive60min" %in% elements) {
          class = "the most active 60 minutes of the day are accumulated"
        } else if ("mostactive30min" %in% elements) {
          class = "the most active 30 minutes of the day are accumulated"
        }
        # wakefulness after sleep onset
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
        # mostactive30/60min
        
        # connector (in)
        if (!is.null(class)) {
          if (class != "Sleep efficiency" & substr(class, 1, 8) != "the most") class = paste("in", class)
        }
        # UNITS -------------------------------------------------------------------
        if ("min" %in% elements) {
          unit = "(minutes)"
        } else if ("mg" %in% elements) {
          unit = "(mili-g units)"
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
          } else if (substr(elements[1], 1, 1) == "L" | substr(elements[1], 1, 1) == "M") {
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
              unit = "(mili-g units)"
            }
            # class
            X = as.numeric(gsub("\\D", "", elements))[1]
            if (substr(elements[1], 1, 1) == "L") {
              class = paste("the", X, "consecutive hours with the lowest activity")
            } else if (substr(elements[1], 1, 1) == "M") {
              class = paste("the", X, "consecutive hours with the highest activity")
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
        if (!(cnames[coli] %in% c("Nvaliddays", "Nvaliddays_WD", "Nvaliddays_WE"))) {
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
      
      # store definition
      dictionary[coli, "Definition"] = def
    }
    # write csv reports with data dictionaries
    directory = file.path(metadatadir, "results", "data dictionary/")
    if (!dir.exists(directory)) dir.create(directory)
    fn = gsub("part5_", "part5_dictionary_", basename(reports[ri]))
    data.table::fwrite(dictionary, file = file.path(directory, fn), 
                       row.names = FALSE, na = "", sep = sep_reports)
  }
}

g.report.part5_dictionary = function(metadatadir) {
  # identify individual reports
  reports = dir(metadatadir, recursive = TRUE, full.names = TRUE, 
                pattern = "^part5.*\\.csv$")
  # read col names of each report and derive definitions
  for (ri in 1:length(reports)) {
    # read report
    R = data.table::fread(reports[ri], verbose = FALSE, nrows = 2)
    cnames = colnames(R)
    # build dictionary structure
    dictionary = data.frame(Variable = cnames,
                            Definition = NA)
    # get definitions
    for (coli in 1:length(cnames)) {
      # split up variable name 
      elements = unlist(strsplit(cnames[coli], "[.]|_"))
      # definitions will be a combination of what, window, class, unit
      what = window = class = unit = NULL
      # WHAT -------------------------------------------------------------
      if ("dur" %in% elements | "nonwear" %in% elements) {
        what = "Time accumulated"
      } else if ("ACC" %in% elements) {
        what = "Mean acceleration"
      } else if ("Nbouts" %in% elements) {
        what = "Number of bouts"
      } else if ("Nblocks" %in% elements) {
        what = "Number of blocks"
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
      } else if ("nonwear" %in% elements) {
        class = "non-wear time"
      } 
      if ("wake" %in% elements) class = paste("awake", class)
      # bouts
      if ("bts" %in% elements) class = paste("bouts of", class)
      if (!is.null(class)) class = paste("in", class)
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
        if ("ID" %in% elements) {
          what = "File identifier"
        } else if ("filename" %in% elements) {
          what = "Filename"
        } else if ("weekday" %in% elements) {
          what = "Day of the week (full name)"
        } else if ("sleeponset|wakeup" %in% elements) {
          if ("sleeponset" %in% elements) what = "Sleep onset time"
          if ("wakeup" %in% elements) what = "Sleep onset time"
          if ("ts" %in% elements) {
            unit = "(hh:mm:ss)"
          } else {
            unit = "(hours from previous midnight)"
          }
        } else if ("daysleeper" %in% elements) {
          what = "Night classified as daysleeper (i.e., wake-up time after noon)"
        } else if ("cleaningcode" %in% elements) {
          what = "Cleaning code for the sleep period time classification (0=no problem; 1=no sleeplog; 2=insufficient valid data; 3=no acc data available; 4=no nights; 5=guider-defined SPT; 6=SPT not found)"
        } else if ("guider" %in% elements) {
          what = "Guider used for the sleep period time identification"
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
          X = as.numeric(gsub("\\D", "", elements))
          if (grepl("^L", elements)) {
            class = paste("the", X, "consecutive hours with the lowest activity")
          } else if (grepl("^M", elements)) {
            class = paste("the", X, "consecutive hours with the highest activity")
          }
        } else if ("daytype" %in% elements) {
          what = "WD = weekday; WE = weekend day"
        } else {
          what = ""
        } 
      } 
      # store definition
      dictionary[coli, "Definition"] = paste(what, class, window, unit)
    }
  }
}

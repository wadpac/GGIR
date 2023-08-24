MONITOR = setNames(0:6, c("AD_HOC", "GENEA", "GENEACTIV", "ACTIGRAPH", "AXIVITY",  "MOVISENS", "VERISENSE")) # GENEA is deprecated
MONITOR = as.environment(as.list(MONITOR))
lockEnvironment(MONITOR, bindings = TRUE)

FORMAT = setNames(1:6, c("BIN", "CSV", "WAV", "CWA", "AD_HOC_CSV", "GT3X")) # WAV is deprecated
FORMAT = as.environment(as.list(FORMAT))
lockEnvironment(FORMAT, bindings = TRUE)

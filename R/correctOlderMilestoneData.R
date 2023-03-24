correctOlderMilestoneData = function(x) {
  # Function to convert older format milestone data
  # for which temperature and light data may have been
  # stored in factor format
  
  # timestamp is assumed to be always present and in character format
  if (is.factor(x$timestamp[1])) {
    x$timestamp = format(x$timestamp)
  }
  # temperature and light may not always be present
  # if present they are expected to be in numeric format
  metrics = c("ENMO","LFENMO", "BFEN", "EN", "HFEN", "HFENplus", "MAD", "ENMOa",
              "ZCX", "ZCY", "ZCZ", "BrondCount_x", "BrondCount_y",
              "BrondCount_z", "NeishabouriCount_x", "NeishabouriCount_y", 
              "NeishabouriCount_z", "NeishabouriCount_vm", 
              "anglex", "angley", "anglez", "temperature", "light")
  pattern = paste(metrics, collapse = "|")
  columns2check = grep(pattern = pattern, x = names(x), value = FALSE)
  if (length(columns2check) > 0) {
    for (column in columns2check) {
      if (is.factor(x[1, column])) {
        x[, column] = as.numeric(as.character(x[, column]))
      }
    }
  }
  return(x)
}
create_test_sleeplog_csv = function(Nnights = 7,
                                    storagelocation = c(),
                                    advanced = FALSE,
                                    sep = ",",
                                    begin_date = "2016/06/25",
                                    type = "sleeplog") {
  
  # function to create a test sleeplog file needed for testing GGIR
  # Nnights is the number of nights the sleeplog has
  if (length(storagelocation) == 0) storagelocation = getwd()
  if (Nnights == 0) Nnights = 1
  if (advanced == FALSE) {
    sleeplog = as.data.frame(t(c("123",rep(c("23:00:00","07:00:00"),Nnights))), stringsAsFactors = TRUE)
    if (ncol(sleeplog) >= 5) {
      # let's add one night where the person slept untill the afternoon
      sleeplog[, 4:5] = c("03:00:00", "17:00:00") 
    }
    colnames(sleeplog) = c("ID",rep(c("onset","wakeup"),Nnights))
    write.table(
      sleeplog,
      file = paste0(storagelocation, "/testsleeplogfile.csv"),
      row.names = FALSE,
      col.names = TRUE,
      sep = ",",
      fileEncoding = "UTF-8"
    )
  } else {
    times = c("07:00:00", "23:00:00", "13:00:00", "13:30:00", "15:00:00", "15:40:00",  "17:00:00", "17:15:00")
    dates = seq(as.Date(begin_date), as.Date(begin_date) + Nnights, by = 1)
    Cnames = c(
      "_wakeup",
      "_onset",
      "_nap1_start",
      "_nap1_end",
      "_nap2_start",
      "_nap2_end",
      "_nonwear1_off",
      "_nonwear1_on"
    )
    if (type == "bedlog") {
      Cnames[1:2] = c("_bedstart", "_bedend")
    }
    if (type == "both") {
      Cnames = c(Cnames[1:2], c("_bedend", "_bedstart"), Cnames[3:8])
      bedtimes = c("08:00:00", "22:00:00")
      times = c(times[1:2], bedtimes, times[3:8])
    }
    names = "ID"
    log = c()
    for (di in 1:length(dates)) {
      # include day number in the time, to ease testing sleeplog processing
      if (di < 10) {
        times2 = gsub(pattern = ":00:00", replacement = paste0(":00:0",di), x = times)
      } else {
        times2 = gsub(pattern = ":00:00", replacement = paste0(":00:", ifelse(di < 59, yes = di, no = "00")),
                                                               x = times)
      }
      log = c(log, as.character(dates[di]), times2)
      names = c(names, c(paste0("D",di,"_date"),paste0("D",di, Cnames)))
    }
    sleeplog = as.data.frame(t(c("123A",log)), stringsAsFactors = TRUE)
    colnames(sleeplog) = names
    
    data.table::fwrite(sleeplog, file = paste0(storagelocation,"/testsleeplogfile.csv"),
                row.names = FALSE, col.names = TRUE, sep = sep)
  }
}

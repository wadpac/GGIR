create_test_sleeplog_csv = function(Nnights=7,storagelocation=c()) {
  # function to create a test sleeplog file needed for testing GGIR
  # Nnights is the number of nights the sleeplog has
  if (length(storagelocation) == 0) storagelocation = getwd()
  if (Nnights == 0) Nnights = 1
  Nnights= 7
  sleeplog = as.data.frame(t(c("123",rep(c("23:00:00","07:00:00"),Nnights))), stringsAsFactors = TRUE)
  sleeplog[,4:5]=c("03:00:00","17:00:00") # let's add one night where the person slept untill the afternoon
  colnames(sleeplog) = c("ID",rep(c("onset","wakeup"),Nnights))
  write.table(sleeplog,file=paste0(storagelocation,"/testsleeplogfile.csv"),
              row.names = FALSE,col.names = TRUE,sep=",",fileEncoding="UTF-8")
}

library("testthat")
library("GGIR")
LC_TIME_backup = Sys.getlocale("LC_TIME")
Sys.setlocale("LC_TIME", "C") # standardises all language in GGIR to English
test_check("GGIR")
Sys.setlocale("LC_TIME", LC_TIME_backup) # reverts language setting
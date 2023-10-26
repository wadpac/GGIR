prepareNewRelease = function(version = c()) {
  # In preparation of a new package release this function helps
  # to check that version number and release date are correct
  # in the DESCRIPTION file
  # The function does not fix any errors, it only warns the user about
  # mistakes via messages in the console.
  # The function is not part of the package on CRAN, because it's name is
  # listed in the .RBuildignore file.
  # Argument version: a character specifying the expected version number, e.g. "1.8-1"
  
  dirman = dir(paste0(getwd(), "/man"), full.names = TRUE)
  for (di in dirman) {
    tools::checkRd(Rd = di, listOK = FALSE)
  }
  
  
  date = unlist(strsplit(as.character(Sys.time())," "))[1]
  dateReversed = unlist(strsplit(date,"-"))
  dateReversed = paste0(dateReversed[3],"-",dateReversed[2],"-",dateReversed[1])
  # Check DESCRIPTION file
  D = read.csv(file = "./DESCRIPTION", sep = "\n")
  errorfound = FALSE
  i = 1
  while (i <= nrow(D)) {
    tmp = as.character(unlist(strsplit(as.character(D[i,]),": ")))
    if (tmp[1] == "Version") {
      if (tmp[2] != version) {
        cat("\nError: Version number is not correct in DESCRIPTION file")
        errorfound = TRUE
      } 
    }
    if (tmp[1] == "Date") {
      if (tmp[2] != date) {
        cat("\nError: Date is not correct in DESCRIPTION file")
        errorfound = TRUE
      } 
    }
    i = i + 1
  }
  if (errorfound == FALSE) cat(paste0("\nNo problem found. Package uses version ",version," and release date ", dateReversed))
  
  Q1 = menu(c("Yes", "No"), title = paste0("\nDo you want to check the package with manual, remote and incoming?"))
  if (Q1 == 1) {
    devtools::check(
      manual = TRUE,
      remote = TRUE,
      incoming = TRUE
    )
  }
  Q2 = menu(c("Yes", "No"), title = paste0("\nDo you want to build the package for CRAN including manual?"))
  if (Q2 == 1) {
    devtools::build(manual = TRUE)
  }
}

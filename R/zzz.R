.onAttach <- function(...) {
  if (!interactive()) return()
  pkgs <- available.packages()
  cran_version <- package_version(pkgs[which(pkgs[,1] == "GGIR"),"Version"])
  local_version <- packageVersion("GGIR")
  behind_cran <- cran_version > local_version
  if (interactive()) {
    if (behind_cran) {
      msg <- paste0("A new version of GGIR (", cran_version,") is available with bug fixes and new features. Do not forget to install it.")
      packageStartupMessage(msg)
    }   
  }
}

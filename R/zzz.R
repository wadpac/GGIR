# GGIR used to depend on CRAN package GENEAread as developed by Joss Langford and Zhou Fang.
# Now GENEAread is depricated the essential parts of the code has been copied to GGIR to ensure
# ongoing functionality.
.onAttach <-function(lib,pkg)
{
ver <- read.dcf(file.path(lib, pkg, "DESCRIPTION"), "Version")
     ver <- as.character(ver)	

options(digits=12)
text = paste("GENEAread", ver, "loaded\n")
packageStartupMessage(text)

}

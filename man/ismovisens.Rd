\name{ismovisens}
\alias{ismovisens}
\title{
Checks whether the files to process are collected with
movisens accelerometers
}
\description{
Checks whether the files indicated by fnames are files collected
with movisens accelerometer}
\usage{
ismovisens(fnames)	
}
\arguments{
  \item{fnames}{
Full path to the file/s or directory where they are stored.
}
}
\value{
Boolean whether it is a movisens file (TRUE) or not (FALSE)
}
  
\examples{
\dontrun{
is.mv = ismovisens(fnames)
}
}

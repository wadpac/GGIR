\name{ismovisens}
\alias{ismovisens}
\title{
Checks whether fnames refer to files collected with
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
Argument fnames as derived in some GGIR functions
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

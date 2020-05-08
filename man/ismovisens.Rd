\name{ismovisens}
\alias{ismovisens}
\title{
Checks whether the files to process are collected with
movisens accelerometers
}
\description{
Checks whether the files in the datadir folder are files collected
with movisens accelerometers}
\usage{
ismovisens(datadir)	
}
\arguments{
  \item{datadir}{
Full path to the directory where they raw files are stored.
}
}
\value{
Boolean whether it is a movisens file (TRUE) or not (FALSE)
}
  
\examples{
\dontrun{
is.mv = ismovisens(datadir)
}
}

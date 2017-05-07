\name{isfilelist}
\alias{isfilelist}
\title{
Checks whether datadir is a directory or a vector with
filenames
}
\description{
Checks whether argument datadir used in various other functions in 
GGIR is the name of a directory that includes data files or whether
it is a vector with the full paths to one or more data files}
\usage{
isfilelist(datadir)	
}
\arguments{
  \item{datadir}{
Argument datadir as used in various other functions in GGIR
}
}
\value{
Boolean whether it is a list of files (TRUE) or not (FALSE)
}
  
\examples{
\dontrun{
isitafilelist = isfilelist(datadir)
}
}

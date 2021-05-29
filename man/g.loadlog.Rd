\name{g.loadlog}
\alias{g.loadlog}
\title{
Load and clean sleeplog information
}
\description{
Loads sleeplog from a csv input file and applies sanity checks
before storing the output in a dataframe
}
\usage{
g.loadlog(loglocation=c(),coln1=c(),colid=c(),nnights=c(),
sleeplogidnum=TRUE, startdates, sleeplogsep=",")
}
\arguments{
  \item{loglocation}{
  See \link{g.part4}
}
  \item{coln1}{
  See \link{g.part4}
}
  \item{colid}{
  See \link{g.part4}
}
  \item{nnights}{
  See \link{g.part4}
}
  \item{sleeplogidnum}{
  See \link{g.part4}
}
\item{startdates}{
data.frame with column ID and startdate with recording identifiers and 
respective start dates
}
\item{sleeplogsep}{
  Value used as sep argument for reading sleeplog csv file.
}
}
\value{
  Data frame with sleeplog
}
\examples{
\dontrun{
sleeplog = g.loadlog(loglocation="C:/mysleeplog.csv",coln1=2,
colid=1,nnights=5,sleeplogidnum=TRUE)
}
}
\author{
Vincent T van Hees <v.vanhees@accelting.com>
}
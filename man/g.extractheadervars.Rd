\name{g.extractheadervars}
\alias{g.extractheadervars}
\title{
Extracts header variables from header object
}
\description{
Function is not intended for direct interaction by package end user
}
\usage{
g.extractheadervars(I)
}
\arguments{
  \item{I}{
  Object produced by \link{g.inspectfile}
  }
}
\value{
\itemize{
\item ID = participant identifier
\item iid = investigator identifier
\item HN = handedness
\item BodyLocation = Attachement location of the sensor
\item SX = sex
\item deviceSerialNumber = serial number
}
}
\examples{
data(data.inspectfile)
headervars = g.extractheadervars(I=data.inspectfile)
}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
}

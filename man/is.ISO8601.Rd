\name{is.ISO8601}
\alias{is.ISO8601}
\title{
Check whether character timestamp is in iso8601 format.
}
\description{
Checks whether timestamp stored in character format is in ISO8601 format or not}
\usage{
is.ISO8601(x)	
}
\arguments{
  \item{x}{
Timestamps in character format either in ISO8601 or as "yyyy-mm-dd hh:mm:ss".
}
}
\examples{
x ="1980-1-1 18:00:00"
is.ISO8601(x)
}

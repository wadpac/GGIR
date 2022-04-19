\name{g.wavread}
\alias{g.wavread}
\title{
function to read .wav files as produced by the accelerometer named 'Axivity'

}
\description{
For reading the wav accelerometer data as collected with an Axivity accelerometer 
}
\usage{
g.wavread(wavfile, start = 1, end = 100,units="minutes")
}
\arguments{
  \item{wavfile}{
filename (required)
}
  \item{start}{
 start point for reading data, see also units
}
  \item{end}{
end point for reading data, see also units
}
\item{units}{
units used for defining start and end
}
}
\details{
If only \code{start} is defined then \code{g.binread} will read all data beyond
\code{start} until the end of the file is reached

}
\value{
 \item{\code{rawxyz}}{matrix with raw x, y, and, z acceleration values}
  \item{\code{header}}{file header}
  \item{\code{timestamps}}{local timestamps for \code{rawxyz}}
 }
\author{
Vincent T van Hees <v.vanhees@accelting.com>
}
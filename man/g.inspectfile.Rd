\name{g.inspectfile}
\alias{g.inspectfile}
\title{
function to inspect accelerometer file for brand, sample frequency and header
}
\description{
Inspects accelerometer file for key information, including: monitor brand, sample frequency and file header
}
\usage{
g.inspectfile(datafile, desiredtz = "", ...)
}
\arguments{
  \item{datafile}{
  	name of data file
  }
  \item{desiredtz}{
  	Desired timezone, see documentation \link{g.getmeta}
  }
  \item{...}{
   Any input arguments needed for function \link{read.myacc.csv} if you
   are working with a non-standard csv formatted files.
  }
}
  
\value{
 \item{header}{fileheader}
  \item{monn}{monitor name (genea, geneactive)}
  \item{monc}{monitor brand code (1 = genea; 2 = geneactive, 3 = actigraph)}
  \item{dformn}{data format (bin, csv)}
  \item{dformc}{data format code (1 = bin, 2 = csv)}
  \item{sf}{samplefrequency in Hertz}
  \item{filename}{filename}
}

\author{
Vincent T van Hees <v.vanhees@accelting.com>
}
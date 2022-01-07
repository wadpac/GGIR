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
  \item{monc}{monitor brand code (0 - ad-hoc file format, 1 = genea (non-commercial),
      2 = GENEActive, 3 = actigraph, 4 = Axivity (AX3, AX6), 5 = Movisense, 6 = Verisense)}
   \item{dformn}{data format name, e.g bin, csv, cwa, gt3x}
  \item{dformc}{data format code (1 = .bin, 2 = .csv, 3 = .wav, 4 = .cwa, 5 = ad-hoc .csv, 6 = .gt3x)}
  \item{sf}{samplefrequency in Hertz}
  \item{filename}{filename}
}

\author{
  Vincent T van Hees <v.vanhees@accelting.com>
}
\name{applyExtFunction}
\alias{applyExtFunction}
\title{
  Apply external function to acceleration data.
}
\description{
  Applies external function to the raw acceleration data within GGIR.
  This makes it easier for new algorithms developed to be pilotted
  on accelerometer data while taking advantage of the existing comprehensive GGIR
  data management and analysis infrastructure.
  This function is not for direct interaction by user, please supply object 
  \code{myfun} to \link{g.shell.GGIR} or \link{g.part1}. Object \code{myfun}
  is a list as detailed below.
}
\usage{
  applyExtFunction(data, myfun, sf, ws3, interpolationType=1)
  
}
\arguments{
  \item{data}{
    Data data.frame as present internally in \link{g.getmeta}. It has at least
    four columns of which the first is the timestamp followed by the x, y,
    and z acceleration.
  }
  \item{myfun}{
    See details, in short: myfun is a list object that holds the external function
    to be applied to the data and various parameters to aid in the process.
  }
  \item{sf}{
    Sample frequency (Hertz) of the data object
  }
  \item{ws3}{
    Short epoch size (first value of windowsizes in \link{g.getmeta}).
  }
  \item{interpolationType}{
    Integer to indicate type of interpolation to be used when resampling time series (mainly relevant for Axivity sensors), 1=linear, 2=nearest neighbour.
  }
}
\value{
  The output of the external algorithm aggregated or repeated to fit the
  short epoch length of GGIR. Therefore, the short epoch length of GGIR
  should be a multitude of the resolution of the external function output,
  or visa versa.
}
\details{
    See package vignette for detailed tutorial with examples
    on how to use the function embedding:
    https://cran.r-project.org/web/package=GGIR/vignettes/applyExtFunction.pdf
    Function applyExtFunction
    is typically not used by the GGIR user directly.
}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
}
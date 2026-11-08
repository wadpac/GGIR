\name{detectTypeBouts}
\alias{detectTypeBouts}
\title{
  Detect bouts for each type as produced by external function
}
\description{
  Detect bouts for each type produced by an external function.
  Bout detection is done with \code{g.getbout} using the \code{tbout.dur}
  and \code{tbout.criter} parameters that may be specified in \code{myfun}.
}
\usage{
   detectTypeBouts(myfun, varnum_type, 
                   daysummary, ds_names,
                   di, fi, ws3, boutnameEnding)
}
\arguments{
  \item{myfun}{
    See \link{GGIR}
  }
  \item{varnum_type}{
    Vector (usually of class factor) containing the type metric as produced by 
    the external function.
  }
  \item{daysummary}{
    Matrix with preliminary day summary.
  }
  \item{ds_names}{
    Daysummary column names
  }
  \item{di}{
    Row index of daysummary representing the recording day
  }
  \item{fi}{
    Column index of daysummary
  }
  \item{ws3}{
    Numeric epochsize in seconds of the timeseries
  }
  \item{boutnameEnding}{
    Character with ending suffix to be pasted to the bout variable names, 
    indicating the segment of the day over which the bouts have been calculated.
  }
}
\value{
  A list containing the updated \code{daysummary}, \code{ds_names}, \code{fi},
  and \code{di} objects.
}
\keyword{internal}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
  Jairo H Migueles <jairo@jhmigueles.com>
}

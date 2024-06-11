\name{detectTypeBouts}
\alias{detectTypeBouts}
\title{
  Detect bouts for each type as produced by external function
}
\description{
  Detect bouts for each tye as produced by external function.
  Bout detection is done with the g.getbout function, and using the 
  parameters tbout.dur, tbout.th.acc, tbout.criter, and tbout.condition
  that may be specified by the user in the myfun list (see Details).
}
\usage{
   detectTypeBouts(myfun, varnum_type, varnum, 
                           UnitReScale, daysummary, ds_names,
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
  \item{varnum}{
    Numeric vector containing the acceleration metric.
  }
  \item{UnitReScale}{
    Numeric indicating the scaling factor to apply to the acceleration 
    metric (varnum).
  }
  \item{daysummary}{
    Matrix with prelimenary day summary.
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
\keyword{internal}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
  Jairo H Migueles <jairo@jhmigueles.com>
}

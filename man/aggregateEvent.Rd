\name{aggregateEvent}
\alias{aggregateEvent}
\title{
  Aggregates event data as produced by external function
}
\description{
  Aggregates event data as produced by external function
}
\usage{
   aggregateEvent(metric_name, epochsize, daysummary, ds_names,
                          fi, di, vari, segmentInfo, myfun = NULL,
                          params_247)
}
\arguments{
  \item{metric_name}{
    Character with name of metric
  }
  \item{epochsize}{
    Numeric epochsize in seconds of the timeseries
  }
  \item{daysummary}{
    Matrix with prelimenary day summary
  }
  \item{ds_names}{
    Daysummary column names
  }
  \item{fi}{
    Column index of daysummary
  }
  \item{di}{
    Row index of daysummary representing the recording day
  }
  \item{vari}{
    Time series derived from metashort
  }
  \item{segmentInfo}{
    List passed on from \link{g.analyse.perday} with time series indices of 
    segment to analyse segment name and segment number.
  }
  \item{myfun}{
    See \link{GGIR}
  }
  \item{params_247}{
    See \link{GGIR}
  }
}
\keyword{internal}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
}
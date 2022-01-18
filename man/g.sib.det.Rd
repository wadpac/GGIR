\name{g.sib.det}
\alias{g.sib.det}
\title{
  sustiained inactivty bouts detection
}
\description{
  Detects sustiained inactivty bouts. Function not intended
  for direct use by package user
}
\usage{
  g.sib.det(M, IMP, I, twd = c(-12, 12),
             acc.metric = "ENMO", desiredtz = "",
             myfun=c(), sensor.location = "wrist", params_sleep = c(), ...)
}
\arguments{
  \item{M}{
    Object produced by \link{g.getmeta}
  }
  \item{IMP}{
    Object produced by \link{g.impute}
  }
  \item{I}{
    Object produced by \link{g.inspectfile}
  }
  \item{twd}{
    Vector of length 2, indicating the time window to consider
    as hours relative to midnight.
  }
    \item{acc.metric}{
    Which one of the metrics do you want to consider to analyze L5. 
    The metric of interest need to be calculated in
    M (see \link{g.part1})
  }
  \item{desiredtz}{
    See \link{g.part3}
  }
  \item{myfun}{
    External function object to be applied to raw data.
    See details \link{applyExtFunction}.
  }
  \item{sensor.location}{
    Character to indicate sensor location, default is wrist.
    If it is hip HDCZA algorithm also requires longitudinal axis of sensor to be
    between -45 and +45 degrees.
  }
   \item{params_sleep}{
    See \link{g.part3}
  }
  \item{...}{
     Any argument used in the previous version of g.sib.det, which will now
     be used to overrule the arguments specified with the parameter objects.
  }
}
\value{
  \itemize{
    \item output = Dataframe for every epoch a classification
    \item detection.failed = Boolean whether detection failed
    \item L5list = L5 for every day (defined from noon to noon)
  }
}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
}

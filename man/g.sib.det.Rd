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
  g.sib.det(M, IMP, I, twd=c(-12,12), anglethreshold = 5, 
              timethreshold = c(5,10), acc.metric = "ENMO",
              desiredtz="", constrain2range=TRUE, 
              myfun=c(), sensor.location = "wrist", HASPT.algo="HDCZA",
              HASIB.algo = "vanHees2015",
              HASIB_axis = "Z", longitudinal_axis=c())
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
  \item{anglethreshold}{
    See \link{g.part3}
  }
  \item{timethreshold}{
    See \link{g.part3}
  }
  \item{acc.metric}{
    Which one of the metrics do you want to consider to analyze L5. 
    The metric of interest need to be calculated in
    M (see \link{g.part1})
  }
  \item{desiredtz}{
    See \link{g.part3}
  }
  \item{constrain2range}{
    See \link{g.part3}
  }
  \item{myfun}{
    External function object to be applied to raw data.
    See details \link{applyExtFunction}.
  }
  \item{sensor.location}{
    See \link{g.part3}
  }
  \item{HASPT.algo}{
    Character to indicate what heuristic algorithm to use for detecting the SPT
    window, see \link{HASPT}
  }
  \item{HASIB.algo}{
    Character to indicate what heuristic algorithm to use for detecting the SIB
    window, see \link{HASIB}
  }
  \item{HASIB_axis}{
    Character to indicate which axis to use for the Sadeh algorithm.
  }
  \item{longitudinal_axis}{
    Integer, to indicate which axis is the longitudinal axis. 
    If not provided function will estimate longitudinal axis. Only used when
    sensor.location="hip" or HASPT.algo="HorAngle".
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

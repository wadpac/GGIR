\name{g.part5_analyseSegment}
\alias{g.part5_analyseSegment}
\title{
  Analyses the time series per time segment for part 5
}
\description{
  Not intended for direct use by GGIR users, part of \link{g.part5}.
}
\usage{
  g.part5_analyseSegment(indexlog, timeList, levelList,
                                  segments,
                                  segments_names,
                                  dsummary, ds_names,
                                  params_general, params_output,
                                  params_sleep, params_247,
                                  params_phyact,
                                  sumSleep, sibDef,
                                  fullFilename,
                                  add_one_day_to_next_date,
                                  lightpeak_available,
                                  tail_expansion_log,
                                  foldernamei, sibreport = NULL,
                                  long_nap_boutsi = NULL)
}
\arguments{
  \item{indexlog}{
    List of objects related to indices of window, file, and segment that
    are passed on from \link{g.part5} to aid selecting time segments or keeping track
    of where in file the code is.
  }
  \item{timeList}{
    List of objects related to time series passed on from \link{g.part5}.
  }
  \item{levelList}{
    List of objects related to intensity levels passed on from \link{g.part5}. 
  }
  \item{segments}{
    List produced by \link{g.part5}
  }
  \item{segments_names}{
    Vector produced by \link{g.part5}
  }
  \item{dsummary}{
    Matrix to hold all daysummary (and segment summary)
  }
  \item{ds_names}{
    Character vector with column names of the dsummary matrix. The code collects
    these separately in this vector and assigns them at the end.
  }
  \item{params_general}{
    See \link{GGIR}
  }
  \item{params_output}{
    See \link{GGIR}
  }
  \item{params_sleep}{
    See \link{GGIR}
  }
  \item{params_247}{
    See \link{GGIR}
  }
  \item{params_phyact}{
    See \link{GGIR}
  }
  \item{sumSleep}{
    Section of data.frame produced by \link{g.part4} passed on from \link{g.part5}.
  }
  \item{sibDef}{
    Character to identify sib definition. 
  }
  \item{fullFilename}{
    Character with full filename being processed
  }
  \item{add_one_day_to_next_date}{
    Boolean to indicate whether one day should be added to next date
  }
  \item{lightpeak_available}{
    Boolean to indicate whether light peak is available
  }
  \item{tail_expansion_log}{
    Object generated in \link{g.part1} and passed on to \link{g.part5} when 
    argument \code{recordingEndSleepHour} is used.
  }
  \item{foldernamei}{
    Character with folder name in which the data file is stored.
  }
  \item{sibreport}{
    Sibreport object as passed on from \link{g.part5}
  }
  \item{long_nap_boutsi}{
    Vector with row indiceses of sibreport that meet the nap criteria.
  }
}
\value{
  List with objects: indexlog, timeList, and the matrix with the prelimenary results
  and column names (dsummary and ds_names, see input arguments above)
}
\keyword{internal}

\name{g.part5.analyseRest}
\alias{g.part5.analyseRest}
\title{
  Analyse rest (internal function)
}
\description{
  Analyses overlap self-reported napping, non-wear and sib.
  Internal function not intended for direct use by GGIR end-user.
}
\usage{
  g.part5.analyseRest(sibreport = NULL, dsummary = NULL,
                                ds_names = NULL, fi = NULL,
                                di = NULL, ts = NULL, tz = NULL,
                                params_sleep = NULL)
}
\arguments{
  \item{sibreport}{
    sibreport data.frame produced by \link{g.sibreport}
  }
  \item{dsummary}{
    matrix created internally by \link{g.part5}
  }
  \item{ds_names}{
    character vector with variable names corresponding to dsummary
    created internally by \link{g.part5}
  }
  \item{fi}{
    Numeric scalar to indicate variable index, created internally by \link{g.part5}
  }
  \item{di}{
    Numeric scalar to indicate recording index, created internally by \link{g.part5}
  }
  \item{ts}{
    Data.frame with time series object passed on from \link{g.part5_analyseSegment} and \link{g.part5},
  }
  \item{tz}{
    Timezone database name
  }
  \item{params_sleep}{
    See \link{GGIR}
  }
}
\value{
  List with updated objects dsummary, ds_names, fi, and di
}
\keyword{internal}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
}
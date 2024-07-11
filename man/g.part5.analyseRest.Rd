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
                                possible_nap_dur = 0,
                                possible_nap_edge_acc = Inf)
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
  \item{possible_nap_dur}{
    Minimum sib duration to be considered. For self-reported naps/nonwear
    there is a minimum duration of 1 epoch.
  }
  \item{possible_nap_edge_acc}{
    Maximum acceleration before or after the SIB for it to be considered.
  }
}
\value{
  List with updated objects dsummary, ds_names, fi, and di
}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
}
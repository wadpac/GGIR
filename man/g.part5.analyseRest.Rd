\name{g.part5.analyseRest}
\alias{g.part5.analyseRest}
\title{
  Analyse rest (internal function)
}
\description{
  Analyses overlap self-reported napping, non-wear and sib.
  Internal function not intended for direct use by user.
}
\usage{
  g.part5.analyseRest(sibreport = NULL, dsummary = NULL,
                                ds_names = NULL, fi = NULL,
                                di = NULL, time = NULL, tz = NULL)
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
  \item{time}{
    Daytime section of time column from the ts object, created internally by \link{g.part5},
  }
  \item{tz}{
    Timezone database name
  }
}
\value{
  List with updated objects dsummary, ds_names, fi, and di
}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
}
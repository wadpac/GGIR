\name{CalcSleepRegularityIndex}
\alias{CalcSleepRegularityIndex}
\title{
  Calculates Sleep Regularity Index
}
\description{
  Calculates Sleep Regularity Index proposed by Phillips and colleagues in 2017
}
\usage{
  CalcSleepRegularityIndex(data = c(), epochsize = c(), desiredtz= c())
}
\arguments{
  \item{data}{
    Data.frame produced by function \link{g.sib.det}.
  }
  \item{epochsize}{
    Numeric value of epoch size in seconds.
  }
  \item{desiredtz}{
    Character with timezone database name, see also \link{g.getmeta}
  }
}
\value{
  Numeric value with Sleep Regularity Index, which by definition must lie in the 
  range -100 (complete irregularity) to 100 (perfect regularity).
}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
}
\references{
\itemize{
  \item Andrew J. K. Phillips, William M. Clerx, et al. Irregular sleep/wake patterns 
  are associated with poorer academic performance and delayed circadian and 
  sleep/wake timing. Scientific Reports. 2017 June 12
}
}
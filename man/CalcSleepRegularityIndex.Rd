\name{CalcSleepRegularityIndex}
\alias{CalcSleepRegularityIndex}
\title{
  Calculates Sleep Regularity Index
}
\description{
  Calculates Sleep Regularity Index per day pair. Absense of missing data is not used 
  as a criteria for calculation. However, missing data criteria are used in g.part4 
  where the output of this function is used.
  Therefore, this function is inspired by the algorithm proposed by Phillips and 
  colleagues in 2017 but not an exact copy.
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
  Data.frame with columns: day (day number); Sleep Regularity Index, which by definition must lie in the 
  range -100 (reversed regularity), to 0 (random pattern), to 100 (perfect regularity);
  weekday (e.g. Wednesday); frac_valid, number between 0 and 1 indicating the fraction of the 24 hour
  period for which valid data was available in both the current and the next day, and; date.
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
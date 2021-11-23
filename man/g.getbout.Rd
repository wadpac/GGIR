\name{g.getbout}
\alias{g.getbout}
\title{
  function to calculate bouts from vector of binary classes
}
\description{
  To detect bouts of behaviour in time series. The function is used by \link{g.analyse}
}
\usage{
g.getbout(x, boutduration, boutcriter = 0.8, closedbout = FALSE, 
bout.metric=6, ws3=5)
}
\arguments{
  \item{x}{vector of zeros and/or ones to be screened for bouts of ones
}
  \item{boutduration}{duration of bout in epochs
}
  \item{boutcriter}{ Minimum percentage of boutduration for which the epoch values
  are expected to meet the threshold criterium
}
  \item{closedbout}{TRUE if you want breaks in bouts to be counted towards
  time spent in bouts (argument only active for bout.metric 1 and 2) 
}
  \item{bout.metric}{If value=1 the code uses the MVPA bout definition as has
  been available since 2014 (see papers by Sabia AJE 2014 and da Silva IJE 2014).
  Here, the algorithm looks for 10 minute windows in which more than XX percent
  of the epochs are above mvpathreshold, and then counts the entire window as mvpa.
  If value=2 the code looks for groups of epochs with a value above
  mvpathreshold that span a time window of at least mvpadur minutes in  which
  more than boutcriter percent of the epochs are above the threshold. The motivation 
  for the defition 1 was: A person who spends 10 minutes in MVPA with a 2 minute
  break in the middle is equally active as a person who spends 8 minutes in MVPA
  without taking a break. Therefore, both should be counted equal and counted as
  10 minute MVPA bout. The motivation for the definition 2 is: not counting breaks
  towards MVPA may simplify interpretation and still counts the two persons in
  the example as each others equal. If value=3, using sliding window across the
  data to test bout criteria per window and do not allow for breaks larger than 1 minute
  and with fraction of time larger than the boutcriter threshold.
  If value=4, same as 3 but also requires the first and last epoch to 
  meet the threshold criteria. If value=5, same as 4, but now looks for breaks larger
  than a minute such that 1 minute breaks are allowe, and the fraction of time that meets
  the threshold should be equal than or greater than the bout.criter threshold.
  If value=6, algorithm improved (2021) to check for first and last epoch.
}
  \item{ws3}{epoch length in seconds, only needed for bout.metric =3, because
  it needs to measure how many epochs equal 1 minute breaks
}

}
\value{
  \item{x}{Vector with binary numbers indicator where bouts where detected}
  \item{boutcount}{Vector with binary numbers indicator where bouts where detected
  and counted towards time spent in bouts, see  argument closedbout for clarification}
  
}
\examples{
y = g.getbout(x=round(runif(1000,0.4,1)),boutduration = 120,boutcriter=0.9,
    closedbout=FALSE,bout.metric=3,ws3=5)
}
\author{
Vincent T van Hees <v.vanhees@accelting.com>
}
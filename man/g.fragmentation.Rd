\name{g.fragmentation}
\alias{g.fragmentation}
\title{
Fragmentation metrics from time series.
}
\description{
The function is used by \link{g.part5} to derive time series 
fragmentation metrics. The code is based on code from the ActFrag 
package as developed by Junrui Di and colleagues. This function could
be seen as a simplified version of their fragmentation function, by assuming
NA values and wear time are accounted before the data enters the function
and assuming that acceleration values have already been
converted into intergers of zeros and ones. 
}
\usage{
  g.fragmentation(x=c(), frag.metrics = c("mean_bout","TP","Gini","power","hazard","all"))
}
\arguments{
  \item{x}{
  Time series of integers 0 or 1 to indicate bouts, where 1 reflects
  the behaviour of interest as defined by argument frag.classes.day/frag.classes.spt in g.part5
  and 0 reflects all the other time, which you may refer to as gaps or breaks in the 
  behaviour of interest.
  }
   \item{frag.metrics}{
   Character with fragmentation metric to exract. Can be
  "mean_bout", "TP", "Gini", "power", "hazard", or all the above metrics "all".
  }
   
  
  
}
\value{
List with Character object showing how decimals are separated
  \item{mean_1}{mean duration of behaviour of interest}
  \item{mean_0}{mean duration of breaks in behaviour of interest}
 \item{awayTP}{transition probability away from behaviour of interest}
 \item{towardsTP}{transition probability towards behaviour of interest}
 \item{Gini_1}{Gini index for behaviour of interest}
  \item{Gini_0}{Gini index for breaks in behaviour of interest}
 %\item{h_1}{hazard function for behaviour of interest}
  %\item{h_0}{hazard function for breaks in behaviour of interest}
  \item{alpha_1}{power law parameter for behaviour of interest}
  \item{alpha_0}{power law parameter for breaks in behaviour of interest}


}
\details{
Metrics include
mean_bout (mean bout duration),
TP (between states transition probability),
Gini (gini index),
power (alapha parameter for power law distribution)
hazard (average hazard function)
}
\examples{
\dontrun{
  x = as.integer(c(rep(0,3),rep(1,20),rep(0,4),
    rep(1,12),rep(0,1),rep(1,15),rep(0,3),rep(1,9))
  out = g.fragmentation(x=x, frag.metrics = c("all"))
}
}
\author{
  Vincent T van Hees <vincentvanhees@gmail.com>
  Junrui Di, <dijunrui@gmail.com>
}
\references{
  Junrui Di, Andrew Leroux, Jacek Urbanek, Ravi Varadhan, Adam P. Spira,
  Jennifer Schrack, Vadim Zipunnikov.
  Patterns of sedentary and active time accumulation are associated with
  mortality in US adults: The NHANES study. bioRxiv 182337;
  doi: https://doi.org/10.1101/182337
}
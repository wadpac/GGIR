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
  g.fragmentation(frag.metrics = c("mean_bout", "TP", "Gini", "power",
                                          "hazard", "h",
                                          "CoV", "dfa", "InfEn",
                                          "SampEn", "ApEn", "RQA", "all"),
                  ACC = c(), intensity.thresholds = c(),
                  do.multiclass =c(),
                  LEVELS = c(),
                           Lnames=c())
}
\arguments{
   \item{frag.metrics}{
   Character with fragmentation metric to exract. Can be
  "mean_bout", "TP", "Gini", "power", "hazard", "h", "CoV",
  "dfa", "InfEn", "SampEn", "ApEn", "RQA"
  or all the above metrics "all".
  }
  \item{ACC}{ Acceleration time series (optional) with which complexity metrics are derived
  that depend on continuous data or multi-class data.
  }
  \item{intensity.thresholds}{
    Intensity thresholds to define classes in the multi-class approach where bouts are not considered.
  }
  \item{do.multiclass}{
  Boolean to indicate whether multiclass metrics should be extracted
  }
  \item{LEVELS}{
    vector of behavioural level identifiers derived with identify_levels function
  }
  \item{Lnames}{
    vector with characters to specify the names of the level identifiers.
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
  \item{h_1}{hazard function for behaviour of interest}
  \item{h_0}{hazard function for breaks in behaviour of interest}
  \item{alpha_1}{power law parameter for behaviour of interest}
  \item{alpha_0}{power law parameter for breaks in behaviour of interest}
  \item{x0.5_0}{ }
  \item{x0.5_1}{ }
  \item{W0.5_0}{ }
  \item{W0.5_1}{ }
  \item{dfa}{ }
  \item{InfEn_binary}{ }
  \item{SampEn_multiclass}{ }
  \item{InfEn_multiclass}{ }
  \item{FastApEn_contin}{ }
  %\item{REC_rqa_contin}{ }
  %\item{DET_rqa_contin}{ }
  %\item{RATIO_rqa_contin}{ }
  %\item{DIV_rqa_contin}{ }
  %\item{Lmax_rqa_contin}{ }
  %\item{Lmean_rqa_contin}{ }
  %\item{ENTR_rqa_contin}{ }
  %\item{LAM_rqa_contin}{ }
  %\item{Vmax_rqa_contin}{ }
 %\item{Vmean_rqa_contin}{ }
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
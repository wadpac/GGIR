\name{SSP}
\alias{SSP}
\title{Estimated self-similarity parameter}
\description{
  This function estimates the overall self-similarity parameter (SSP), 
  also known as the scaling exponent or alpha, as well as the piecewise 
  short-term (alpha_1) and long-term (alpha_2) scaling exponents.
}
\usage{
  SSP(data, scale = 2^(1/8), box_size = 4, m = 1, epochSize)
}
\arguments{
  \item{data}{
    Univariate time series (must be a vector or data frame)
  }
  \item{scale}{
    Specifies the ratio between successive box sizes (by default scale = 2^(1/8))
  }
  \item{box_size}{
    Vector of box sizes (must be used in conjunction with scale = "F")
  }
  \item{m}{
    An integer of the polynomial order for the detrending (by default m = 1)
  }
  \item{epochSize}{
    The epoch size of the data in seconds. Used to convert box sizes to minutes 
    to apply time boundaries for alpha_1 and alpha_2.
  }
}
\value{
  A list containing three real numbers (all of them between 0 and 2):
    \item{alpha_overall}{The overall estimated scaling exponent across all box sizes.}
    \item{alpha_1}{The short-term scaling exponent, calculated for time scales smaller than or equal to 90 minutes.}
    \item{alpha_2}{The long-term scaling exponent, calculated for time scales between 120 and 600 minutes.}
}
\details{
  The DFA fluctuation can be computed in a geometric scale or for different 
  choices of boxes sizes. In human motor activity, the scaling behavior exhibits 
  a crossover point around 1.5 to 2 hours. Therefore, alpha_1 captures the 
  short-term temporal correlations regulated by multiple physiological controls, 
  while alpha_2 captures long-term fluctuations that are heavily reliant on 
  the central circadian pacemaker.
}
\note{
  It is not possible estimating alpha for multiple time series at once.
}
\examples{
  # Estimate self-similarity of a very known time series available on R base: the sunspot.year.
  # Then the spend time with each method is compared.
  \dontrun{
    ssp_results = SSP(sunspot.year)
    ssp_results$alpha_overall
    ssp_results$alpha_1
    ssp_results$alpha_2
  }
}
\references{
  C.-K. Peng, S.V. Buldyrev, S. Havlin, M. Simons, H.E. Stanley, A.L. Goldberger Phys. Rev. E, 49 (1994), p. 1685
  Mesquita, Victor & Filho, Florencio & Rodrigues, Paulo. (2020). Detection of crossover points in detrended fluctuation analysis: An application to EEG signals of patients with epilepsy. Bioinformatics. 10.1093/bioinformatics/btaa955.
}
\author{
  Ian Meneghel Danilevicz <ian.meneghel-danilevicz@inserm.fr>
  Victor Barreto Mesquita <victormesquita40@hotmail.com>
  Jairo H Migueles <jairo@jhmigueles.com>
}
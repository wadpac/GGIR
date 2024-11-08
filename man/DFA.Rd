\name{DFA}
\alias{DFA}
\title{Detrended Fluctuation Analysis}
\description{
  Detrended Fluctuation Analysis (DFA)
}
\usage{
  DFA(data, scale = 2^(1/8), box_size = 4, m = 1)
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
    An integer of the polynomial order for the detrending (by default m=1)
  }
}
\value{
  Estimated alpha is a real number between zero and two.
}
\details{
  The DFA fluctuation can be computed in a geometric scale or for different choices of boxes sizes.
}
\note{
  It is not possible estimating alpha for multiple time series at once.
}
\examples{
  # Estimate self-similarity of a very known time series available
  # on R base: the sunspot.year.
  # Then the spend time with each method is compared.
  \dontrun{
    dfa = DFA(sunspot.year)
  }
}
\references{
  C.-K. Peng, S.V. Buldyrev, S. Havlin, M. Simons, H.E. Stanley, A.L. Goldberger Phys. Rev. E, 49 (1994), p. 1685
  Mesquita, Victor & Filho, Florencio & Rodrigues, Paulo. (2020). Detection of crossover points in detrended fluctuation analysis: An application to EEG signals of patients with epilepsy. Bioinformatics. 10.1093/bioinformatics/btaa955.
}
\author{
  Ian Meneghel Danilevicz <ian.meneghel-danilevicz@inserm.fr>
  Victor Barreto Mesquita <victormesquita40@hotmail.com>
}
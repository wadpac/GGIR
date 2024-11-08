\name{ABI}
\alias{ABI}
\title{Activity balance index (ABI)}
\usage{
  ABI(x)
}
\arguments{
  \item{x}{the estimated self-similarity parameter (SSP)}
}
\value{
  The estimated Activity balance index (ABI) is a real number between zero and one.
}
\description{
  This function estimates the Activity balance index (ABI), which is a transformation of the self-similarity parameter (SSP), also known as scaling exponent or alpha.
}
\details{
  ABI = exp(-abs(SSP-1)/exp(-2))
}
\examples{
  # Estimate Activity balance index of a very known time series
  # available on R base: the sunspot.year.
  \dontrun{  
    ssp = SSP(sunspot.year)
    abi = ABI(ssp)
  }
}
\references{
  C.-K. Peng, S.V. Buldyrev, S. Havlin, M. Simons, H.E. Stanley, A.L. Goldberger Phys. Rev. E, 49 (1994), p. 1685
  Mesquita, Victor & Filho, Florencio & Rodrigues, Paulo. (2020). Detection of crossover points in detrended fluctuation analysis: An application to EEG signals of patients with epilepsy. Bioinformatics. 10.1093/bioinformatics/btaa955.
}
\author{
  Ian Meneghel Danilevicz <ian.meneghel-danilevicz@inserm.fr>
}

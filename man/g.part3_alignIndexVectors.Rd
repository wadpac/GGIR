\name{g.part3_alignIndexVectors}
\alias{g.part3_alignIndexVectors}
\title{
  Aligns index vectors used inside g.part3_correct_guider
}
\description{
  Aligns index vectors used inside g.part3_correct_guider
}
\usage{
   g.part3_alignIndexVectors(x, y, a, b, N) 
}
\arguments{
  \item{x}{
    Index vector for minimum
  }
  \item{y}{
    Index vector for maximum
  }
  \item{a}{
    Index vector for lower medium
  }
  \item{b}{
    Index vector for upper medium
  }
  \item{N}{
    The maximum index, which equals the length of the time series
  }
}
\keyword{internal}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
}
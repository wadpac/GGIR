\name{filterNonwearNight}
\alias{filterNonwearNight}
\title{
  Filters short lasting nonwear during the night
}
\description{
  Filters short lasting nonwear during the night. Not intended for
  direct use by GGIR user.
}
\usage{
  filterNonwearNight(r1, metalong, qwindowImp, desiredtz, params_cleaning, ws2)
}
\arguments{
  \item{r1}{
    Object r1 as created inside function \link{g.weardec} which is a vector
    of zeros and ones derived from metalong where 1 indicates nonwear and 0 wear.
  }
  \item{metalong}{
    Object metalong created in \link{g.part1}
  }
  \item{qwindowImp}{
    See \link{g.impute}
  }
  \item{desiredtz}{
    See \link{GGIR}
  }
  \item{params_cleaning}{
    Parameters object with cleaning paramete, see \link{GGIR}.
  }
  \item{ws2}{
    Object ws2 which equals the second value of vector parameter \code{windowsizes}
  }
}
\value{
  r1 object with short lasting nonwear during the night removed
}
\keyword{internal}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
}
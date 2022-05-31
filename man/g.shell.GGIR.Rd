\name{g.shell.GGIR}
\alias{g.shell.GGIR}
\title{
  Wrapper function around function GGIR
}
\description{
  This function used to be the central function in the package,
  but has been renamed GGIR. You can still use function call g.shell.GGIR
  but all arguments will be passed on to function GGIR. We have done this to preserve
  consistency with older use cases of the GGIR package. All documentation can
  now be found in \link{GGIR}.

}
\usage{
  g.shell.GGIR(...)
}
\arguments{
  \item{...}{
    Any of the parameters used by \link{GGIR}.
  }
}
\value{
  The function provides no values, it only ensures that other functions are called
  and that their output is stored. See \link{GGIR}.
}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
}

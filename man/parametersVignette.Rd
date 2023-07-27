\name{parametersVignette}
\alias{parametersVignette}
\title{
  Builds Section for Parameters Vignette
}
\description{
  Function extracts the documentation for a given GGIR argument as provided in the \link{GGIR} documentation
  and builds the structure for the Parameters Vignette.
  Function not designed for direct use by package user.
}
\usage{
  parametersVignette(params = "sleep")
}
\arguments{
  \item{params}{
    Character (default = "sleep"). 
    Name of the parameters object to build its corresponding section in the Parameters vignette.
  }
}
\value{
  Structure for the vignette subsection.
}
\keyword{internal}
\author{
  Jairo Hidalgo Migueles <jairo.hidalgo.migueles@gmail.com>
}

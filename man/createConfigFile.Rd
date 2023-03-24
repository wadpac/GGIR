\name{createConfigFile}
\alias{createConfigFile}
\title{
  Creates Config File based on variables in function GGIR environment
}
\description{
  Only used inside \link{GGIR}. Not intended for direct use by user.
}
\usage{
  createConfigFile(config.parameters = c(), GGIRversion = "")	
}
\arguments{
  \item{config.parameters}{
   List with all arguments used in \link{GGIR}.
  }
  \item{GGIRversion}{
   GGIR version mumber to be incorported in the ConfigFile.
  }
}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
}
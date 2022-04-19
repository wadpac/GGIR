\name{g.getidfromheaderobject}
\alias{g.getidfromheaderobject}
\title{
  Extracts participant identifier from header object
}
\description{
  Extracts participant identifier from header object, if
  it can not be found then the filename is used as identifier.
  Function is not intended for direct interaction by package end user
  
}
\usage{
  g.getidfromheaderobject(filename,header,dformat,mon)	
}
\arguments{
  \item{filename}{
    File name
  }
  \item{header}{
    header object as extracted with \link{g.inspectfile}
  }
  \item{dformat}{
    Data format code, same as for \link{g.dotorcomma}
  }
  \item{mon}{
    Monitor code, same as for \link{g.dotorcomma}
  }
}
\value{
Participant identifier as character 
}
\examples{
\dontrun{
  g.getidfromheaderobject(filename="C:/myfile.bin",header,dformat=2,mon=2)	
}
}
\author{
Vincent T van Hees <v.vanhees@accelting.com>
}

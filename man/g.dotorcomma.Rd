\name{g.dotorcomma}
\alias{g.dotorcomma}
\title{
Assesses whether decimals in fileheader are stored with comma
or dot separated decimals
}
\description{
The function is used by \link{g.readaccfile} to assess how numeric data
should be interpretted
}
\usage{
g.dotorcomma(inputfile,dformat,mon, desiredtz = "", ...)
}
\arguments{
  \item{inputfile}{
  full path to inputfile
  }
   \item{dformat}{
  Data format code: 1=.bin, 2=.csv, 3=.wav, 4=.cwa
  }
  \item{mon}{
  Monitor code (accelorometer brand): 1=GENEA,2=GENEActiv,3=Actigraph,
  4=Axivity.
  }
  \item{desiredtz}{
  Desired timezone, see documentation \link{g.getmeta}
  }
  \item{...}{
   Any input arguments needed for function \link{read.myacc.csv} if you
   are working with a non-standard csv formatted files.
  }
  
}
\value{
Character object showing how decimals are separated
}

\examples{
\dontrun{
decn = g.dotorcomma(inputfile="C:/myfile.bin",dformat=1,mon=2)
}
}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
}
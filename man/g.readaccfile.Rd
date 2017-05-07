\name{g.readaccfile}
\alias{g.readaccfile}
\title{
 Generic functiont to read large blocks of accelerometer data
}
\description{
The function is used by \link{g.getmeta} to read large blocks of the
accelerometer file, which are processed and then deleted. This is needed
for memory management.
}
\usage{
g.readaccfile(filename,blocksize,blocknumber,selectdaysfile=c(),filequality,
                         decn,dayborder)
}
\arguments{
  \item{filename}{
  filename
  }
   \item{blocksize}{
  Size of blocks (in file pages) to be read
  }
  \item{blocknumber}{
  Block number relative to start of file
  }
   \item{selectdaysfile}{
  See documentation \link{g.getmeta}
  }
   \item{filequality}{
  Single row dataframe with columns: filetooshort, filecorrupt,
and  filedoesnotholdday. All with the value TRUE or FALSE
  }
  \item{decn}{
  Character with a dot or a comma, used for interpretting
  samplefrequency in the file header. decn is derived with
  \link{g.dotorcomma}
  }
  \item{dayborder}{
  See documentation \link{g.part1}
  }
}
\value{
P=P,filequality=filequality, switchoffLD = switchoffLD
\itemize{
\item \code{P} Block object extracted from file with format specific to
accelerometer brand
\item \code{filequality} Same as in function arguments
\item \code{switchoffLD} Boolean to indicate whether it is worth
continueing to read the next block of data or not
}

}
\examples{
\dontrun{
filequality = data.frame(filetooshort=FALSE,filecorrupt=FALSE,
filedoesnotholdday = FALSE)
output = g.readaccfile(filename="C:/myfile.bin",blocksize=20000,blocknumber=1,
selectdaysfile=c(),filequality=filequality,
                         decn=".",dayborder=0) 
}
}
\author{
  Vincent T van Hees <vincentvanhees@gmail.com>
}
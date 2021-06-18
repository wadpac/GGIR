\name{g.readaccfile}
\alias{g.readaccfile}
\title{
 Generic functiont to read large blocks of accelerometer data
}
\description{
The function is used by \link{g.getmeta} and \link{g.calibrate} to
read large blocks of the accelerometer file, which are processed and
then deleted. This is needed for memory management.
}
\usage{
g.readaccfile(filename,blocksize,blocknumber,
  selectdaysfile=c(),filequality, decn,dayborder,ws,desiredtz="",
  PreviousEndPage=1,inspectfileobject=c(),configtz=c(), 
  interpolationType=1,...)
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
    \item{ws}{
  Larger windowsize for non-detection, see documentation \link{g.part2}
  }
   \item{desiredtz}{
  Desired timezone, see documentation \link{g.getmeta}
  }
  \item{PreviousEndPage}{
   Page number on which previous block ended (automatically assigned within
   g.getmeta and g.calibrate).
  }
  \item{inspectfileobject}{
  Output from the function \link{g.inspectfile}.
  }
  \item{configtz}{
  Only functional for AX3 cwa data at the moment. Timezone in which the accelerometer
  was configured. Only use this argument if the timezone of configuration and
  timezone in which recording took place are different.
  }
  \item{interpolationType}{
    See \link{g.getmeta}
  }
  \item{...}{
   Any input arguments needed for function \link{read.myacc.csv} if you
   are working with a non-standard csv formatted files.
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
\item \code{endpage} Page number on which blocked ends, this will be
used as input for argument PreviousEndPage when reading the next block.
}

}
\examples{
\dontrun{
filequality = data.frame(filetooshort=FALSE, filecorrupt=FALSE,
filedoesnotholdday = FALSE)
output = g.readaccfile(filename="C:/myfile.bin", 
locksize=20000, blocknumber=1,
selectdaysfile=c(), filequality=filequality,
                         decn=".", dayborder=0,PreviousEndPage=c()) 
}
}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
}
\name{g.readaccfile}
\alias{g.readaccfile}
\title{
  Generic functiont to read large blocks of accelerometer data
}
\description{
  The function is used by \link{g.getmeta} and \link{g.calibrate} to
  read large blocks of the accelerometer file, which are processed and
  then deleted from memory. This is needed for memory management.
}
\usage{
  g.readaccfile(filename, blocksize, blocknumber, filequality,
                           ws, PreviousEndPage = 1, inspectfileobject = c(),
                           PreviousLastValue = c(0,0,1), PreviousLastTime = NULL,
                           params_rawdata = c(), params_general = c(), header = NULL, ...)
}
\arguments{
  \item{filename}{
    filename
  }
  \item{blocksize}{
    Size of blocks (in file pages) to be read
  }
  \item{blocknumber}{
    Block number relative to start of file, starting with 1.
  }
  \item{filequality}{
    Single row dataframe with columns: filetooshort, filecorrupt,
    and filedoesnotholdday. All with the value TRUE or FALSE
  }
  \item{ws}{
    Larger windowsize for non-detection, see documentation \link{g.part2}
  }
  \item{PreviousEndPage}{
    Page number on which previous block ended (automatically assigned within
    g.getmeta and g.calibrate).
  }
  \item{inspectfileobject}{
    Output from the function \link{g.inspectfile}.
  }
  \item{PreviousLastValue}{
    Automatically identified last value in previous chunk of data read.
  }
    \item{PreviousLastTime}{
    Automatically identified last timestamp in previous chunk of data read.
  }
  \item{params_rawdata}{
    See \link{g.part1}
  }
  \item{params_general}{
    See \link{g.part1}
  }
  \item{header}{
    Header information that was extracted the previous time this file was read,
    to be re-used instead of being extracted again.
  }
  \item{...}{
    Any input arguments needed for function \link{read.myacc.csv} if you
    are working with a non-standard csv formatted files. Furter,
    any argument used in the previous version of g.readaccfile, which will now
    be used to overrule the arguments specified with the parameter objects.
  }
}
\value{
  \itemize{
    \item \code{P} Block object extracted from file with format specific to
    accelerometer brand
    \item \code{filequality} Same as in function arguments
    \item \code{isLastBlock} Boolean indicating whether this was the last block to read
    \item \code{endpage} Page number on which blocked ends, this will be
    used as input for argument PreviousEndPage when reading the next block.
  }
}
\examples{
  \dontrun{
    filequality = data.frame(filetooshort = FALSE, filecorrupt = FALSE,
    filedoesnotholdday = FALSE)
    output = g.readaccfile(filename = "C:/myfile.bin", 
    blocksize = 20000, blocknumber = 1,
    selectdaysfile = c(), filequality = filequality,
    dayborder = 0, PreviousEndPage = c()) 
  }
}
\keyword{internal}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
}
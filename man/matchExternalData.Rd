\name{matchExternalData}
\alias{matchExternalData}
\title{
  Reads and matches data from sensors other than accelerometer.
}
\description{
   Reads data from sensors other than accelerometer and matches it to the GGIR
  milestone data based on the timestamp.
}
\usage{
   matchExternalData(externalDatadir = "C:/mystudy/mydata", 
                     metadatadir = "C:/mystudy/output_mydata/", 
                     colname = NULL, idloc = NULL, 
                     overwrite = FALSE, f0 = 1, f1 = NULL,
                     desiredtz = "", verbose = TRUE)
}
\arguments{
  \item{externalDatadir}{
    See \link{GGIR}.
  }
  \item{metadatadir}{
    Directory that holds a folder 'meta' and inside this a folder 'basic'
    which contains the milestone data produced by \link{g.part1}. The folderstructure
    is normally created by \link{g.part1} and \link{GGIR} will recognise what
    the value of metadatadir is.
  }
  \item{f0}{
    See \link{GGIR}.
  }
  \item{f1}{
    See \link{GGIR}.
  }
  \item{colname}{
    Character (default = NULL). Name of the column that contains the information from
    the external sensor. This will also be used in the GGIR milestone data and reports.
  }
  \item{idloc}{
    See \link{GGIR}.
  }
  \item{desiredtz}{
    See \link{GGIR}.
  }
  \item{overwrite}{
    See \link{GGIR}.
  }
  \item{verbose}{
    See \link{GGIR}.
  }
}
\details{
  This function reads and matches external data with M$metashort based on timestamp. 
  The function requires some preprocessing of the external data by the user at 
  the time. 
  
  GGIR expects RData files containing one data frame with two columns: a "timestamp" column
  and another column with the sensor data. The user should define the column name of 
  the second column with \code{externalDataColname} (this same name will be used in 
  M$metashort and reports).
  
  The user is responsible for providing timestamps in ISO8601 format and compatible
  with the epoch length used in GGIR.
}
\author{
  Jairo H Migueles <jairo@jhmigueles.com>
}

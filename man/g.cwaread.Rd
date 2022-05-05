\name{g.cwaread}
\alias{g.cwaread}
\title{
Function to read .cwa-format files as produced by the accelerometer named 'Axivity'
}
\description{
For reading .cwa-format data, if you have .wav format data then see function \link{g.wavread}
}
\usage{
g.cwaread(fileName, start = 0, end = 0, progressBar = FALSE, 
  desiredtz = "", configtz = c(), interpolationType=1)
}
\arguments{
  \item{fileName}{
    filename (required)
  }
  \item{start}{
    start point for reading data, this can either be a timestamp
    "year-month-day hr:min:sec" or a page number (optional)
  }
  \item{end}{
    end point for reading data, this can either be a timestamp 
    "year-month-day hr:min:sec" or a page number (optional)
  }
  \item{progressBar}{
    Is trigger to switch on/off the text progress bar. If progressBar
    is TRUE then the function displays the progress bar but it works
    slightly slower
  }
  \item{desiredtz}{
    Desired timezone, see documentation \link{g.getmeta}
  }
  \item{configtz}{
    Only functional for AX3 cwa data at the moment. Timezone in which the accelerometer
    was configured. Only use this argument if the timezone of configuration and
    timezone in which recording took place are different.
  }
  \item{interpolationType}{
    Integer to indicate type of interpolation to be used when resampling time series (mainly relevant for Axivity sensors), 1=linear, 2=nearest neighbour.
  }
}
\value{
 \item{\code{data}}{dataframe with timestamp, raw x, -y, and, -z acceleration values,
  temperature, battery and light}
  \item{\code{header}}{file header}
}
\author{
  Evgeny Mirkes <em322@leicester.ac.uk>
  Vincent van Hees <v.vanhees@accelting.com>
}
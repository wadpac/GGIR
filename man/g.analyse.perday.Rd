\name{g.analyse.perday}
\alias{g.analyse.perday}
\title{
  Function supports \link{g.analyse}. Not intended for direct use by user.
}
\description{
  Generates day specific analyses and fills corresponding
  output matrix, \link{g.analyse}.
}
\usage{
g.analyse.perday(ndays, firstmidnighti, time, nfeatures, 
                midnightsi, metashort, averageday,
                doiglevels, nfulldays,lastmidnight, ws3, ws2, qcheck,
                fname, sensor.location, wdayname, tooshort, includedaycrit,
                doquan, quantiletype, doilevels, domvpa,
                mvpanames, wdaycode, ID,
                deviceSerialNumber, ExtFunColsi, myfun,
                params_247 = c(), params_phyact = c(),
                params_general = c(),
                ...)
}
\arguments{
  \item{ndays}{Number of days in file} 
  \item{firstmidnighti}{see \link{g.detecmidnight}}
  \item{time}{timestamp column from metalong converted to character}
  \item{nfeatures}{estimate of number of variables that need to be stored in
  the output matrix}
  \item{midnightsi}{see \link{g.detecmidnight}}
  \item{metashort}{see \link{g.impute}}
  \item{averageday}{As produced by \link{g.impute}}
  \item{doiglevels}{Boolean to indicate whether iglevels should be calculated}
  \item{nfulldays}{Number of days between the first and last midnight in the recording}
  \item{lastmidnight}{see \link{g.detecmidnight}}
  \item{ws3}{Epoch size in seconds}
  \item{ws2}{see \link{g.weardec}}
  \item{qcheck}{vector with zeros and ones for each epoch, respenting the
    quality check derived with g.impute}
  \item{fname}{RData filename produced by g.part1}
  \item{sensor.location}{as produced by \link{g.extractheadervars}} 
  \item{wdayname}{character with weekdayname}
  \item{tooshort}{0 (file not too short) or 1 (file too short)} 
  \item{includedaycrit}{see \link{g.analyse}} 
  \item{doquan}{Boolean whether quantile analysis should be done}
  \item{quantiletype}{see \link{g.analyse}} 
  \item{doilevels}{Boolean whether to generate ilevels, see \link{g.analyse}} 
  \item{domvpa}{Boolean whether to do mvpa analysis}
  \item{mvpanames}{Matrix with 6 columns and 1 row holding the names for the six 
    mvpa variables}
  \item{wdaycode}{Equal to M$wday as produced by \link{g.getmeta}}
  \item{ID}{Person Identification number, this can be numeric or character}
  \item{deviceSerialNumber}{As produced by \link{g.extractheadervars}}
  \item{ExtFunColsi}{column index of metashort where metric is stored}
  \item{myfun}{External function object to be applied to raw data, see \link{g.getmeta}.}
  \item{params_247}{
    See \link{GGIR}
  }
  \item{params_phyact}{
    See \link{GGIR}
  }
  \item{params_general}{
    See \link{GGIR}
  }
  \item{...}{
   Any argument used in the previous version of g.analyse.perday, which will now
   be used to overrule the arguments specified with the parameter objects.
  }
}
\value{
  \item{\code{daysummary}}{Summary per day for the file that was analysed}
  \item{\code{ds_names}}{Variable names in daysummary}
  \item{\code{windowsummary}}{Window summary, only used when
  selectdayfile is specified}
  \item{\code{ws_names}}{Variable names in windowsummary}
}
\keyword{internal}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
}
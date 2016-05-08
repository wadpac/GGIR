\name{g.part2}
\alias{g.part2}
\title{
function to analyse and summarize pre-processed output from \link{g.part1}
}
\description{
Loads the output from \link{g.part1} and then applies \link{g.impute} and 
\link{g.analyse}, after which the output is converted to .RData-format
which will be used by \link{g.shell.GGIR} to generate reports. 
The variables in these reports are the same variables as described in
\link{g.analyse}.
}
\usage{
g.part2(datadir=c(),metadatadir=c(),f0=c(),f1=c(),strategy = 1, hrs.del.start = 0.5,
hrs.del.end = 0.5, maxdur = 7, includedaycrit = 16,
L5M5window = c(0,24), M5L5res = 10, winhr = 5, qwindow=c(0,24), qlevels = c(0.1),
ilevels = c(0,10), mvpathreshold = c(100),boutcriter = 0.8,ndayswindow=7,idloc=1,
do.imp=TRUE,storefolderstructure=FALSE,overwrite=FALSE,epochvalues2csv=FALSE,
mvpadur=c(1,5,10),selectdaysfile=c(),window.summary.size=10,dayborder=0,
mvpa.2014 = FALSE,closedbout=FALSE)
}
\arguments{
\item{datadir}{
  Directory where the accelerometer files are stored or list, e.g. "C:/mydata"
  of accelerometer filenames and directories,
  e.g. c("C:/mydata/myfile1.bin","C:/mydata/myfile2.bin").
  }
  \item{metadatadir}{
  Directory where the output from \link{g.part1} was stored
  }
  \item{f0}{
   File index to start with (default = 1). Index refers to the filenames sorted
   in increasing order
  }
  \item{f1}{
   File index to finish with (defaults to number of files available)
  }
  \item{strategy}{
    how to deal with knowledge about study protocol. value = 1 to select data
    based on \code{hrs.del.start}, \code{hrs.del.end}, and \code{maxdur}.
    Value = 2 to only use the data between the first midnight and the last
    midnight, value = 3 only selects the most active X days
    in the files. X is specified by argument \code{ndayswindow}
    See also \link{g.impute}
  }
  \item{hrs.del.start}{
    how many HOURS after start of experiment did wearing of monitor start?, 
    see \link{g.impute}
  }
  \item{hrs.del.end}{
    how many HOURS before the end of the experiment did wearing of monitor
    definitely end?, see \link{g.impute}
  }
  \item{maxdur}{
    how many DAYS after start of experiment did experiment
    definitely stop? (set to zero if unknown = default), see \link{g.impute}
  }
  \item{includedaycrit}{
    minimum required number of valid hours in day specific analysis
    (NOTE: there is no minimum required number of hours per day in the summary
    of an entire measurement, every available hour is used to make the best possible
    inference on average metric value per week)
  }
  \item{L5M5window}{
    start and end time, in 24 hour clock hours, over which L5M5 needs to be
    calculated. The calculation is done based on the average day
  }
  \item{M5L5res}{
    resoltion of L5 and M5 analysis in minutes (default: 10 minutes)
  }
   \item{winhr}{
    window size in hours of L5 and M5 analysis (dedault = 5 hours)
  }
  \item{qwindow}{
  start and end time, in 24 hour clock hours, over which distribution in metric 
  values need to be extracted. Value = c(0,24) will consider all 24 hours.
  }
  \item{qlevels}{
    array of percentiles for which value needs to be extracted. These need to
    be expressed as a fraction of 1, e.g. c(0.1, 0.5, 0.75). There is no limit
    to the number of percentiles. If left empty then percentiles will not be 
    extracted. Distribution will be derived from short epoch metric data, see
    \link{g.getmeta}.
  }
  \item{ilevels}{
    Levels for acceleration value frequency distribution in mg, e.g. c(0,100,200)
    There is no constriction to the number of levels.
  }
  \item{mvpathreshold}{
    Threshold for MVPA estimation. Threshold needs to be based on metric ENMO.
    This can be a single number or an array of numbers, e.g. c(100,120). In the 
    later case the code will estimate MVPA seperately for each threshold. If 
    this variable is left blank c() then MVPA is not estimated
  }
  \item{boutcriter}{
    The variable boutcriter is a number between 0 and 1 and defines what fraction
    of a bout needs to be above the mvpathreshold
   }
  \item{ndayswindow}{
    If \code{strategy} is set to 3 then this is the size of the window as a number
    of days
  }
  \item{idloc}{
    If value = 1 (default) the code assumes that ID number is stored in the
    obvious header field. If value = 2 the code uses the character string preceding
    the character '_' in the filename as the ID number
  }
     \item{do.imp}{
  Whether to impute missing values (e.g. suspected of monitor non-wear) or not
  by \link{g.impute}. Default and recommended setting is TRUE
  }
   \item{storefolderstructure}{
  Store folder structure of the accelerometer data
  }
     \item{overwrite}{
    Overwrite previously generated milestone data by this function for this
    particular dataset. If FALSE then it will skip the previously processed files
    (default = FALSE).
  }
     \item{epochvalues2csv}{
    If TRUE then epoch values are exported to a CSV spreadsheet. Here, non-wear time
    is imputed where possible (default = FALSE).
  }
  \item{mvpadur}{
    default = c(1,5,10). Three bout duration for which MVPA will be calculated
  }
  \item{selectdaysfile}{
  Functionality designed for the London Centre of Longidutinal studies.
  Csv file holding the relation between device serial numbers and measurement days of interest.
  }
    \item{dayborder}{
  Hour at which days start and end (default = 0), value = 4 would mean 4am
  }
    \item{window.summary.size}{
    Functionality designed for the London Centre of Longidutinal studies.
  Size in minutes of the summary window
  }
    \item{mvpa.2014}{
  If TRUE use the MVPA bout definition as has been available since 2014.
  With hindsight this included a bug in the calculation which incorrectly allocated too
  many minutes to each bout. In the revised calculation this has now been corrected.
  To use the new calculation set value to FALSE (default) and also set argument closedbout.
  
  }
  \item{closedbout}{
  If TRUE then count breaks in a bout towards the bout duration. If FALSE then only count
  time spent above the threshold towards the bout duration.
  }
}
\value{
 The function provides no values, it only ensures that other functions are called
 and that their output is stored in the folder structure as created with \link{g.part1}.
}
\examples{
\dontrun{
metadatadir = "C:/myresults/output_mystudy"
g.part2(metadatadir)
}
}
\author{
  Vincent T van Hees <vincentvanhees@gmail.com>
}
\references{
\itemize{
\item van Hees VT, Gorzelniak L, Dean Leon EC, Eder M, Pias M, et al. (2013) Separating
Movement and Gravity Components in an Acceleration Signal and Implications for the
Assessment of Human Daily Physical Activity. PLoS ONE 8(4): e61691.
doi:10.1371/journal.pone.0061691
\item van Hees VT, Fang Z, Langford J, Assah F, Mohammad A, da Silva IC, Trenell MI, 
White T, Wareham NJ, Brage S. Auto-calibration of accelerometer data for
free-living physical activity assessment using local gravity and temperature: 
an evaluation on four continents. J Appl Physiol (1985). 2014 Aug 7
}
}
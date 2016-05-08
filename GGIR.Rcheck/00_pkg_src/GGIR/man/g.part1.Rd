\name{g.part1}
\alias{g.part1}
\title{
function to load and pre-process acceleration files
}
\description{
Calls function \link{g.getmeta} and \link{g.calibrate}, and converts the
output to .RData-format which will be the input for \link{g.part2}. Here,
the function generates a folder structure to keep track of various output files.
The reason why these \link{g.part1} and \link{g.part2} are not merged as one
generic shell function is because g.part1 takes much
longer to and involves only minor decisions of interest to the movement scientist.
Function g.part2 on the other hand is relatively fast and comes with all the
decisions that directly impact on the variables that are of interest to the
movement scientist. Therefore, the user may want to run g.part1 overnight
or on a computing cluster, while g.part2 can then be the main playing ground
for the movement scientist. Function \link{g.shell.GGIR} provides the main shell
that allows for operating g.part1 and g.part2.
}
\usage{
g.part1(datadir=c(),outputdir=c(),f0=1,f1=c(),windowsizes = c(5,900,3600),
                   desiredtz = "Europe/London",chunksize=c(),studyname=c(),
                   do.enmo = TRUE,do.lfenmo = FALSE,do.en = FALSE,
                   do.bfen = FALSE,do.hfen=FALSE,do.hfenplus = FALSE,
                   do.teLindert2013=FALSE,do.anglex=FALSE,do.angley=FALSE,
                   do.anglez=FALSE,do.enmoa=FALSE,do.cal = TRUE,
                   lb = 0.2, hb = 15,  n = 4,
                   use.temp=TRUE,spherecrit=0.3,minloadcrit=72,
                   printsummary=TRUE,print.filename=FALSE,overwrite=FALSE,
                   backup.cal.coef=c(),selectdaysfile=c(),dayborder=0)
}
\arguments{
  \item{datadir}{
  Directory where the accelerometer files are stored or list of accelerometer
  filenames and directories
  }
  \item{outputdir}{
  Directory where the output needs to be stored. Note that this function will
  attempt to create folders in this directory and uses those folder to organise
  output
  }
  \item{f0}{
   File index to start with (default = 1). Index refers to the filenames sorted
   in increasing order
  }
  \item{f1}{
   File index to finish with (defaults to number of files available)
  }
  \item{windowsizes}{
   see \link{g.getmeta}
  }
  \item{desiredtz}{
  see \link{g.getmeta}
  }
  \item{chunksize}{
  see \link{g.getmeta}
  }
  \item{studyname}{
  If the datadir is a folder then the study will be given the name of the
  data directory. If datadir is a list of filenames then the studyname will be used
  as name for the analysis
  }  
  \item{do.bfen}{
  if TRUE, calculate metric BFEN with band-pass filter configuration set by
  \code{lb} and \code{hb}, see \link{g.getmeta}
  }
  \item{do.enmo}{
  if TRUE (default), calculate metric ENMO, see \link{g.getmeta}
  }
  \item{do.lfenmo}{
  if TRUE, calculate metric LFENMO with low-pass filter configuration set by
  \code{hb},see \link{g.getmeta}
  }
  \item{do.en}{
  if TRUE, calculate metric EN, see \link{g.getmeta}
  }
  \item{do.hfen}{
  if TRUE, calculate metric HFEN with low-pass filter configuration set by
  \code{hb}, see \link{g.getmeta}
  }
  \item{do.hfenplus}{
  if TRUE, calculate metric HFENplus with band-pass filter configuration set
  by \code{lb} and \code{hb}, see \link{g.getmeta}
  }
    \item{do.teLindert2013}{
  if TRUE, calculate the 5 sec epoch Actiwatch count replication as described 
  by te Lindert et al 2013 in the journal SLEEP (volume 36, issue 5, page 781)
  }
  \item{do.anglex}{
  if TRUE, calculate the angle of the x-axis relative to the horizontal plane
  (degrees) utilizing all three axes
  }
  \item{do.angley}{
  if TRUE, calculate the angle of the y-axis relative to the horizontal plane
  (degrees) utilizing all three axes
  }
  \item{do.anglez}{
  if TRUE, calculate the angle of the z-axis relative to the horizontal plane
  (degrees) utilizing all three axes
  }
    \item{do.enmoa}{if TRUE (default), calculate metric ENMOa which is equal to
  metric ENMO but with the absolute taken from the Euclidean norm minus one.
}
   \item{do.cal}{
  Whether to apply auto-calibration or not, see \link{g.calibrate}. Default and
  recommended setting is TRUE
  }
  \item{lb}{
  lower boundary of the frequency filter (in Hertz)
  }
  \item{hb}{
  upper boundary of the frequency filter (in Hertz), see \link{g.getmeta}
  }
  \item{n}{
  order of the frequency filter, see \link{g.getmeta}
  }
  \item{use.temp}{
  see \link{g.calibrate} use temperature sensor data if available (Geneactive only)
  }
  \item{spherecrit}{
  see \link{g.calibrate} the minimum required acceleration value (in g) on both 
  sides of 0 g for each axis. Used to judge whether the sphere is sufficiently 
  populated
  }
  \item{minloadcrit}{
  see \link{g.calibrate} the minimum number of hours the code needs to read for the
  autocalibration procedure to be effective (only sensitive to multitudes of 12 hrs, 
  other values will be ceiled). After loading these hours only extra data is loaded 
  if calibration error has not be reduced to under 0.01 g.
  }
   \item{printsummary}{
  see \link{g.calibrate} if TRUE will print a summary when done
  }
  
  \item{print.filename}{
  Whether to print the filename before before analysing it (default is FALSE).
  Printing the filename can be useful to investigate problems (e.g. to verify that
  which file is being read).
  }
   \item{overwrite}{
    Overwrite previously generated milestone data by this function for this
    particular dataset. If FALSE then it will skip the previously processed files
    (default = FALSE).
  }
  \item{backup.cal.coef}{
  If the auto-calibration fails then the user has the option to provide back-up
  calibration coefficients via this argument. The value of the argument needs to
  be the name and directory of a csv-spreadsheet with the following column names
  and subsequent values: 'filename' with the names of accelerometer files on which
  the calibration coefficients need to be applied in case auto-calibration fails;
  'scale.x', 'scale.y', and 'scale.z' with the scaling coefficients; 'offset.x',
  'offset.y', and 'offset.z' with the offset coefficients, and; 
  'temperature.offset.x', 'temperature.offset.y', and 'temperature.offset.z'
  with the temperature offset coefficients. The argument is intended for analysing
  short lasting laboratory experiments with insufficient sphere data, but for which
  calibration coefficients can be derived in an alternative way.
  It is the users responsibility to compile the csv-spreadsheet.
  }
  \item{selectdaysfile}{
  Optional functionality. Character pointing at a csv file holding the relationship
  between device serial numbers (first column) and measurement dates of interest 
  (second and third column). The date format should be dd/mm/yyyy. And the first row
  if the csv file is assumed to have a character variable names, e.g. "serialnumber"
  "Day1" and "Day2" respectively. Raw data will be extracted and stored in the output
  directory in a new subfolder named 'raw'.
  }
  \item{dayborder}{
  Hour at which days start and end (default = 0), value = 4 would mean 4am
  }

}
\value{
 The function provides no values, it only ensures that the output from other
 functions is stored in .RData(one file per accelerometer file) in folder structure
}
\examples{
\dontrun{
datafile = "C:/myfolder/mydata"
outputdir = "C:/myresults"
g.part1(datadir,outputdir)
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
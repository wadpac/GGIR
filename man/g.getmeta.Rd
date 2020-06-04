\name{g.getmeta}
\alias{g.getmeta}
\title{
function to extract meta-data (features) from data in accelerometer file
}
\description{
Reads a accelerometer file in blocks, extracts various features and stores average  feature
value per short or long epoch. Acceleration and angle metrics are stored at short
epoch length. The non-wear indication score, the clipping score, temperature
(if available), light (if available), and Euclidean norm are stored at long epoch
length. The function has been designed and thoroughly tested with accelerometer files 
from GENEA and GENEActiv. Further, the function should be able to cope with csv-format
data procuded by GENEActiv and Actigraph
}
\usage{
g.getmeta(datafile, desiredtz = "", 
windowsizes = c(5, 900, 3600),daylimit = FALSE, 
offset = c(0,0,0), scale = c(1,1,1),
tempoffset = c(0,0,0),do.bfen = FALSE, do.enmo = TRUE,
do.lfenmo = FALSE, do.en = FALSE,
do.hfen = FALSE, do.hfenplus = FALSE, do.mad = FALSE,
do.anglex=FALSE,do.angley=FALSE,do.anglez=FALSE,
do.roll_med_acc_x=FALSE,do.roll_med_acc_y=FALSE,
do.roll_med_acc_z=FALSE,do.dev_roll_med_acc_x=FALSE,
do.dev_roll_med_acc_y=FALSE,do.dev_roll_med_acc_z=FALSE,
do.enmoa = FALSE, do.lfen = FALSE, 
do.lfx=FALSE, do.lfy=FALSE, do.lfz=FALSE, 
                     do.hfx=FALSE, do.hfy=FALSE, do.hfz=FALSE,
                     do.bfx=FALSE, do.bfy=FALSE, do.bfz=FALSE, 
                     lb = 0.2,hb = 15, n = 4,
meantempcal=c(),chunksize=c(),
selectdaysfile=c(),dayborder=0,dynrange=c(),configtz=c(),
myfun=c(),...)
}
\arguments{
  \item{datafile}{name of accelerometer file
}
  \item{desiredtz}{desired timezone: see also http://en.wikipedia.org/wiki/Zone.tab
}
  \item{windowsizes}{ Three values to indicate the lengths of the windows as in 
  c(window1,window2,window3): window1 is the short epoch length in seconds and by
  default 5 this is the time window over which acceleration and angle metrics
  are calculated, window2 is the long epoch length in seconds for which non-wear and
  signal clipping are defined, default 900. However, window3 is the window length
  of data used for non-wear detection and by default 3600 seconds. So, when window3
  is larger than window2 we use overlapping windows, while if window2 equals window3
  non-wear periods are assessed by non-overlapping windows. Window2 is expected to be
  a multitude of 60 seconds.
}
  \item{daylimit}{number of days to limit (roughly), if set to FALSE no daylimit
  will be applied
}
  \item{offset}{offset correction value per axis, usage:
  value = scale(value,center = -offset, scale = 1/scale)
}
  \item{scale}{scaling correction value per axis, usage:
  value = scale(value,center = -offset, scale = 1/scale)
}
  \item{tempoffset}{temperature offset correction value per axis, usage:
  value = scale(value,center = -offset, scale = 1/scale)
  + scale(temperature, center = rep(averagetemperate,3), scale = 1/tempoffset)
}
  \item{do.bfen}{if TRUE, calculate metric BFEN with band-pass filter
  configuration set by \code{lb} and \code{hb}
}
  \item{do.enmo}{if TRUE (default), calculate metric ENMO with negative values
  rounded to zero
}
  \item{do.lfenmo}{if TRUE, calculate metric LFENMO with low-pass filter
  configuration set by \code{hb}
}
  \item{do.en}{if TRUE, calculate metric EN
}
  \item{do.hfen}{if TRUE, calculate metric HFEN with low-pass filter configuration
  set by \code{hb}
}
  \item{do.hfenplus}{if TRUE, calculate metric HFENplus with band-pass filter
  configuration set by \code{lb} and \code{hb}
}
  \item{do.mad}{if TRUE, calculate metric MAD (Mean Amplitude Deviarion)
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
  \item{do.roll_med_acc_x}{if TRUE, calculate rolling median for the x axis
  }
  \item{do.roll_med_acc_y}{if TRUE, calculate rolling median for the y axis
  }
  \item{do.roll_med_acc_z}{if TRUE, calculate rolling median for the z axis
  }
  \item{do.dev_roll_med_acc_x}{if TRUE, calculate deviations from rolling 
  median for the x axis
  }
  \item{do.dev_roll_med_acc_y}{if TRUE, calculate deviations from rolling 
  median for the y axis
  }
  \item{do.dev_roll_med_acc_z}{if TRUE, calculate deviations from rolling 
  median for the z axis
  }
  \item{do.lfen}{if TRUE, calculate metric EN with low-pass filtered
  inputed signals using configuration set by \code{hb}
  }
  
  \item{do.lfx}{ if TRUE, calculate average absolute low-pass filtered x-axis
  with filter configuration set by \code{hb}.
  }
  \item{do.lfy}{ if TRUE, calculate average absolute low-pass filtered y-axis
  with filter configuration set by \code{hb}.
  }
  \item{do.lfz}{ if TRUE, calculate average absolute low-pass filtered z-axis
  with filter configuration set by \code{hb}.
  }
    \item{do.hfx}{ if TRUE, calculate average absolute low-pass filtered x-axis
  with filter configuration set by \code{lb}.
  }
  \item{do.hfy}{ if TRUE, calculate average absolute low-pass filtered y-axis
  with filter configuration set by \code{lb}.
  }
  \item{do.hfz}{ if TRUE, calculate average absolute low-pass filtered z-axis
  with filter configuration set by \code{lb}.
  }
      \item{do.bfx}{ if TRUE, calculate average absolute band-pass filtered x-axis
  with filter configuration set by \code{lb} and by \code{hb}.
  }
  \item{do.bfy}{ if TRUE, calculate average absolute band-pass filtered y-axis
  with filter configuration set by \code{lb} and by \code{hb}.
  }
  \item{do.bfz}{ if TRUE, calculate average absolute band-pass filtered z-axis
  with filter configuration set by \code{lb} and by \code{hb}.
  }
  \item{lb}{lower boundary of the frequency filter (in Hertz)
  }
  \item{hb}{upper boundary of the frequency filter (in Hertz)
  }
  \item{n}{order of the frequency filter
  }
  \item{meantempcal}{mean temperature corresponding to the data as used for 
autocalibration. If autocalibration is not done or if temperature was not
available then leave blank (default)
  }
  \item{chunksize}{number between 0.2 and 1 to specificy the size of chunks to be 
loaded as a fraction of a 24 hour period, e.g. 0.5 equals 12 hour chunks.
The default is 1 (24 hrs). For machines with less than 4 Gb of RAM memory a value
below 1 is recommended.
  }
  \item{selectdaysfile}{
  see \link{g.part1}
  }
  \item{dayborder}{
   see \link{g.part1}
  }
  \item{dynrange}{
   see \link{g.part1}
  }
  \item{configtz}{
  Only functional for AX3 cwa data at the moment. Timezone in which the accelerometer
  was configured. Only use this argument if the timezone of configuration and
  timezone in which recording took place are different.
  }
  \item{myfun}{
    External function object to be applied to raw data. 
    See details \link{applyExtFunction}.
  }
  \item{...}{
   Please ignore. Only used by the code internally when called from within
   g.part1 with selectdaysfile specific.
  }  
}
\value{
  \item{metalong}{dataframe with long epoch meta-data: EN, non-wear score,
  clipping score, temperature}
  \item{metashort}{dataframe with short epoch meta-data: timestamp and metric}
  \item{tooshort}{indicator of whether file was too short for processing (TRUE or FALSE)}
  \item{corrupt}{indicator of whether file was considered corrupt (TRUE or FALSE)}
}
\examples{
\dontrun{
datafile = "C:/myfolder/testfile.bin"

#Extract meta-data:
M = g.getmeta(datafile)

#Inspect first couple of rows of long epoch length meta data:
print(M$metalong[1:5,])

#Inspect first couple of rows of short epoch length meta data:
print(M$metalong[1:5,])
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
\item Aittasalo M, Vaha-Ypya H, Vasankari T, Husu P, Jussila AM, and Sievanen H. Mean 
amplitude deviation calculated from raw acceleration data: a novel method for 
classifying the intensity of adolescents physical activity irrespective of accelerometer 
brand. BMC Sports Science, Medicine and Rehabilitation (2015).
}
}

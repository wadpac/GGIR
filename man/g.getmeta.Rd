\name{g.getmeta}
\alias{g.getmeta}
\title{
  Function to extract meta-data (features) from data in accelerometer file
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
g.getmeta(datafile,desiredtz = "",windowsizes = c(5,900,3600),
          daylimit=FALSE,offset=c(0,0,0),scale=c(1,1,1),tempoffset = c(0,0,0),
          do.bfen=FALSE, do.enmo=TRUE, do.lfenmo=FALSE,
          do.en=FALSE, do.hfen=FALSE,
          do.hfenplus=FALSE, do.mad=FALSE,
          do.anglex=FALSE, do.angley=FALSE, do.anglez=FALSE,
          do.roll_med_acc_x=FALSE, do.roll_med_acc_y=FALSE, do.roll_med_acc_z=FALSE,
          do.dev_roll_med_acc_x=FALSE, do.dev_roll_med_acc_y=FALSE,
          do.dev_roll_med_acc_z=FALSE, do.enmoa=FALSE,
          do.lfen=FALSE, do.lfx=FALSE, do.lfy=FALSE, do.lfz=FALSE,
          do.hfx=FALSE, do.hfy=FALSE, do.hfz=FALSE,
          do.bfx=FALSE, do.bfy=FALSE, do.bfz=FALSE,
          do.zcx=FALSE, do.zcy=FALSE, do.zcz=FALSE,
          do.brondcounts = FALSE,
          lb = 0.2, hb = 15,  n = 4,meantempcal=c(), chunksize=c(), selectdaysfile=c(),
          dayborder=0,dynrange=c(),configtz=c(),myfun=c(),
          interpolationType=1, imputeTimegaps = TRUE,
                     ...)
}
\arguments{
  \item{datafile}{
    name of accelerometer file
  }
  \item{desiredtz}{
    See \link{g.part1}
  }
  \item{windowsizes}{
    See \link{g.part1}
  }
  \item{daylimit}{
    number of days to limit (roughly), if set to FALSE no daylimit
    will be applied
  }
  \item{offset}{
    offset correction value per axis, usage:
    value = scale(value,center = -offset, scale = 1/scale)
  }
  \item{scale}{
    scaling correction value per axis, usage:
    value = scale(value,center = -offset, scale = 1/scale)
  }
  \item{tempoffset}{
    temperature offset correction value per axis, usage:
    value = scale(value,center = -offset, scale = 1/scale)
    + scale(temperature, center = rep(averagetemperate,3), scale = 1/tempoffset)
  }
  \item{do.bfen}{
    See \link{g.part1}
  }
  \item{do.enmo}{
    See \link{g.part1}
  }
  \item{do.lfenmo}{
    See \link{g.part1}
  }
  \item{do.en}{
    See \link{g.part1}
  }
  \item{do.hfen}{
    See \link{g.part1}
  }
  \item{do.hfenplus}{
    See \link{g.part1}
  }
  \item{do.mad}{
    See \link{g.part1}
  }
  \item{do.anglex}{
    See \link{g.part1}
  }
  \item{do.angley}{
    See \link{g.part1}
  }
  \item{do.anglez}{
    See \link{g.part1}
  }
  \item{do.enmoa}{
    See \link{g.part1}
  }
  \item{do.roll_med_acc_x}{
    See \link{g.part1}
  }
  \item{do.roll_med_acc_y}{
    See \link{g.part1}
  }
  \item{do.roll_med_acc_z}{
    See \link{g.part1}
  }
  \item{do.dev_roll_med_acc_x}{
    See \link{g.part1}
  }
  \item{do.dev_roll_med_acc_y}{
    See \link{g.part1}
  }
  \item{do.dev_roll_med_acc_z}{
    See \link{g.part1}
  }
  \item{do.lfen}{
    See \link{g.part1}
  }
  \item{do.lfx}{
    See \link{g.part1}
  }
  \item{do.lfy}{
    See \link{g.part1}
  }
  \item{do.lfz}{
    See \link{g.part1}
  }
  \item{do.hfx}{
    See \link{g.part1}
  }
  \item{do.hfy}{
    See \link{g.part1}
  }
  \item{do.hfz}{
    See \link{g.part1}
  }
  \item{do.bfx}{
    See \link{g.part1}
  }
  \item{do.bfy}{
    See \link{g.part1}
  }
  \item{do.bfz}{
    See \link{g.part1}
  }
  \item{do.zcx}{
    See \link{g.part1}
  }
  \item{do.zcy}{
    See \link{g.part1}
  }
  \item{do.zcz}{
    See \link{g.part1}
  }
  \item{do.brondcounts}{
    See \link{g.part1}
  }
  \item{lb}{
    See \link{g.part1}
  }
  \item{hb}{
    See \link{g.part1}
  }
  \item{n}{
    See \link{g.part1}
  }
  \item{meantempcal}{
    mean temperature corresponding to the data as used for
    autocalibration. If autocalibration is not done or if temperature was not
    available then leave blank (default)
  }
  \item{chunksize}{
    See \link{g.part1}
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
    see \link{g.part1}
  }
  \item{myfun}{
    External function object to be applied to raw data.
    See details \link{applyExtFunction}.
  }
  \item{interpolationType}{
    see \link{g.part1}
  }
  \item{imputeTimegaps}{
    see \link{g.part1}
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
  Vincent T van Hees <v.vanhees@accelting.com>
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

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
g.part1(datadir = c(), outputdir = c(), f0 = 1, f1 = c(),
        studyname = c(), myfun = c(), params_metrics = c(), params_rawdata = c(),
        params_cleaning = c(), params_general = c(), imputeTimegaps = TRUE, ...)
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
  \item{studyname}{
    If the datadir is a folder then the study will be given the name of the
    data directory. If datadir is a list of filenames then the studyname will be used
    as name for the analysis
  }

  \item{myfun}{
    External function object to be applied to raw data.
    See details \link{applyExtFunction}.
  }
  \item{params_metrics}{
    List of parameters used metrics:
    Including Boolean indicators of which metrics to extract, as discussed in see \link{g.getmeta}):
    do.anglex, do.angley, do.anglez, do.zcx, do.zcy, do.zcz, do.bfen, do.enmo, do.lfenmo, do.en,
    do.hfen, do.hfenplus, do.mad, do.roll_med_acc_x, do.roll_med_acc_y, do.roll_med_acc_z,
    do.dev_roll_med_acc_x, do.dev_roll_med_acc_y, do.dev_roll_med_acc_z, do.enmoa,
    do.lfen, do.lfx, do.lfy, do.lfz, do.hfx, do.hfy, do.hfz,do.bfx, do.bfy, do.bfz.
    Further, the list should included: lb, hb, and n (all numeric, see \link{g.getmeta})
  }
  \item{params_rawdata}{
    List of parameters used for reading and pre-processing raw data:
    do.cal (boolean, see details), chunksize (numeric, see \link{g.calibrate} and \link{g.getmeta}),
    spherecrit (numeric, see \link{g.calibrate}),
    minloadcrit (numeric, see \link{g.calibrate}), printsummary (boolean, see \link{g.calibrate}),
    do.cal (boolean, see \link{g.calibrate}), backup.cal.coef (boolean, see details),
    dynrange (numeric, see \link{g.getmeta}), minimumFileSizeMB (numeric, see details),
    interpolationType (numeric, see \link{g.getmeta}), and all argument from function \link{read.myacc.csv}
    that start with "rmc.".
  }
  \item{params_cleaning}{
    List of parameters releated to data cleaning, imputation or masking:
    strategy, hrs.del.start, hrs.del.end, maxdur, ndayswindow (all numeric, see \link{g.impute})
    includedaycrit (numeric, see \link{g.analyse}), do.imp (boolean, see details)
    selectdaysfile (character, see \link{g.analyse})
    TimeSegments2ZeroFile (character, see details), data_cleaning_file (character, see details)
    excludefirstlast.part5 (boolean, see details),
    includedaycrit.part5 (numeric, see \link{g.report.part5})
    excludefirst.part4, excludelast.part4, excludefirstlast (boolean, see details),
    includenightcrit (numeric, see details)
  }
  \item{params_general}{
    List of general parameters used accors GGIR: overwrite (boolean, see details),
    selectdaysfile (character, see details), configtz (character, see \link{g.getmeta}),
    dayborder (numeric, see details),  do.parallel (boolean, see details), maxNcores (numeric, see details).
    windowsizes, idloc (all numeric, see \link{g.analyse})
    desiredtz (character, see \link{g.getmeta})
    acc.metric (character, see details), part5_agg2_60seconds (boolean, see details)
    print.filename (boolean, see details), sensor.location (character, see \link{g.sib.det})
  }
  \item{imputeTimegaps}{
   Boolean to indicate whether timegaps larger than 0.25 seconds should be imputed.
   Currently onlly used for .gt3x data, where timegaps can be expected as a result of
   Actigraph's sleep.mode.
 }
  \item{...}{
    Any input arguments needed for function \link{read.myacc.csv} if you
    are working with a non-standard csv formatted files. To enable compatibility with older GGIR version,
    user can also provide individual params_metrics parameters as separate argument, e.g. do.anglex = TRUE.
  }
}
\details{
  Explanation of rawdata parameters that are not documented elsewhere in the technical manual.
  \cr
  \itemize{
    \item \code{backup.cal.coef} Default value is "retrieve".
      Option to use backed-up calibration coefficient instead of
      deriving the calibration coefficients when analysing the same file twice.
      Argument backup.cal.coef has two usecase. Use case 1: If the auto-calibration
      fails then the user has the option to provide back-up
      calibration coefficients via this argument. The value of the argument needs to
      be the name and directory of a csv-spreadsheet with the following column names
      and subsequent values: 'filename' with the names of accelerometer files on which
      the calibration coefficients need to be applied in case auto-calibration fails;
      'scale.x', 'scale.y', and 'scale.z' with the scaling coefficients; 'offset.x',
      'offset.y', and 'offset.z' with the offset coefficients, and;
      'temperature.offset.x', 'temperature.offset.y', and 'temperature.offset.z'
      with the temperature offset coefficients. This can be useful for analysing
      short lasting laboratory experiments with insufficient sphere data to perform
      the auto-calibration, but for which calibration coefficients can be derived
      in an alternative way.  It is the users responsibility to compile the
      csv-spreadsheet. Instead of building this file the user can also
      Use case 2: The user wants to avoid performing the auto-calibration repeatedly
      on the same file. If backup.cal.coef value is set to "retrieve" (default) then
      GGIR will look out for the  data_quality_report.csv  file in the outputfolder
      QC, which holds the previously generated calibration coefficients. If you
      do not want this happen, then deleted the data_quality_report.csv from the
      QC folder or set it to value "redo".
    \item \code{minimumFileSizeMB}  Minimum File size in MB required to enter processing,
      default 2MB. This argument can help
      to avoid having short uninformative files to enter the analyses. Given that a typical accelerometer
      collects several MBs per hour, the default setting should only skip the very short files.
    \item \code{do.cal}  Whether to apply auto-calibration or not, see \link{g.calibrate}. Default and
      recommended setting is TRUE.
  }
  \cr

  Explanation of cleaning parameters that are not documented elsewhere in the technical manual.
  \cr
  \itemize{
    \item \code{do.imp} Whether to impute missing values (e.g. suspected of monitor non-wear) or not
      by \link{g.impute}. Default and recommended setting is TRUE
      \item \code{TimeSegments2ZeroFil}  Csv-file holding the data.frame used for argument
      TimeSegments2Zero in function \link{g.impute}
    \item \code{data_cleaning_file} Optional path to a csv file you create that holds four
      columns: ID, day_part5, relyonguider_part4, and night_part4. ID should hold the participant ID.
      Columns day_part5 and night_part4 allow you to specify which day(s) and
      night(s) need to be excluded from part 5 and 4, respectively. So, this will be done regardless
      of whether the rest of GGIR thinks those day(s)/night(s)
      are valid. Column relyonguider_part4 allows you to specify for which nights
      part 4 should fully rely on the guider. See also package vignette.
    \item \code{excludefirstlast.part5} If TRUE then the first and last night of the measurement are ignored for the
      leep assessment.
    \item \code{excludefirstlast} If TRUE then the first and last night of the measurement are
      ignored for the sleep assessment.
    \item \code{excludefirst.part4} If TRUE then the first night of the measurement are
      ignored for the sleep assessment.
    \item \code{excludelast.part4}
      If TRUE then the last night of the measurement are ignored for the sleep assessment.
    \item \code{includenightcrit} Minimum number of valid hours per night (24 hour window between
    noon and noon), used for sleep.
  }

  \cr
  Explanation of general parameters that are not documented elsewhere in the technical manual.
  \cr
  \itemize{
    \item \code{overwrite} Do you want to overwrite analysis for which milestone data exists? 
      If overwrite=FALSE then milestone data from a previous analysis will 
      be used if available and visual reports will not be created again.
        
    \item \code{selectdaysfile} Do not used, this is legacy code for one specific data study. Character pointing at
    a csv file holding the relationship between device serial numbers (first column) and measurement dates of interest
    (second and third column). The date format should be dd/mm/yyyy. And the first row
    if the csv file is assumed to have a character variable names, e.g. "serialnumber"
    "Day1" and "Day2" respectively. Raw data will be extracted and stored in the output
    directory in a new subfolder named 'raw'.
    \item \code{dayborder} Hour at which days start and end (default = 0), value = 4 would mean 4 am
    \item \code{do.parallel} Boolean whether to use multi-core processing (only works if at least 4 CPU cores are available).
    \item \code{maxNcores} Maximum number of cores to use when argument do.parallel is set to true.
    GGIR by default uses the maximum number of available cores, but this argument
    allows you to set a lower maximum.
    \item \code{acc.metric}  Which one of the metrics do you want to consider to analyze L5.
    The metric of interest need to be calculated in M.
    \item \code{part5_agg2_60seconds} Boolean whether to use aggregate epochs to 60 seconds as part of the part 5 analysis.
    \item \code{print.filename} Whether to print the filename before before analysing it (default is FALSE).
    Printing the filename can be useful to investigate problems (e.g. to verify that which file is being read).
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
  Vincent T van Hees <v.vanhees@accelting.com>
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
    \item Aittasalo M, Vaha-Ypya H, Vasankari T, Husu P, Jussila AM, and Sievanen H. Mean
    amplitude deviation calculated from raw acceleration data: a novel method for
    classifying the intensity of adolescents physical activity irrespective of accelerometer
    brand. BMC Sports Science, Medicine and Rehabilitation (2015).
  }
}

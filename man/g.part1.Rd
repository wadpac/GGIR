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
          params_cleaning = c(), params_general = c(), ...)
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
   See details
  }
  \item{params_rawdata}{
   See details
  }
  \item{params_cleaning}{
   See details
  }
  \item{params_general}{
   See details.
  }
  \item{...}{
    If you are working with a non-standard csv formatted files, g.part1 also takes any input
    arguments needed for function \link{read.myacc.csv} and argument rmc.noise from
    \link{get_nw_clip_block_params}. First test these argument with function \link{read.myacc.csv} directly.
    To ensure compatibility with R scripts written for older GGIR versions, the user can also
    provide parameters listed in the params_ objects as direct argument.
  }
}
\details{
  GGIR comes with many processing parameters, which have been thematically grouped in
  parameter objects (R list). By running print(load_params()) you can
  see the default values of all the parameter objects. When g.part 1 is used via \link{g.shell.GGIR}
  you have the option to specifiy a configuration file, which will overrule the default
  parameter values. Further, as user you can set parameter values as input argument to both g.part1
  and \link{g.shell.GGIR}. Directly specified argument overrule the configuration file and default values.
  
  See the GGIR package vignette for a more elaborate overview of parameter objects and their usage across GGIR.
  
  GGIR part 1 (g.part1) takes the following parameter objects as input:
  
  \subsection{params_metrics}{
  A list of parameters used to specify the signal metrics that need to be extract in GGIR part 1.
  \describe{
      \item{do.anglex}{Boolean, if TRUE calculate metric. For computation
        specifics see source code of function \link{g.applymetrics}}
      \item{do.angley}{Boolean, if TRUE calculate metric. For computation
        specifics see source code of function \link{g.applymetrics}}
      \item{do.anglez}{Boolean, if TRUE calculate metric. For computation
        specifics see source code of function \link{g.applymetrics}}
      \item{do.zcx}{Boolean, if TRUE calculate metric zero-crossing count for x-axis.
        For computation specifics see source code of function \link{g.applymetrics}}
      \item{do.zcy}{Boolean, if TRUE calculate metric zero-crossing count for y-axis.
        For computation specifics see source code of function \link{g.applymetrics}}
      \item{do.zcz}{Boolean, if TRUE calculate metric zero-crossing count for z-axis.
        For computation specifics see source code of function \link{g.applymetrics}}
      \item{do.enmo}{Boolean, if TRUE calculate metric. For computation
        specifics see source code of function \link{g.applymetrics}}
      \item{do.lfenmo}{Boolean, if TRUE calculate metric. For computation
        specifics see source code of function \link{g.applymetrics}}
      \item{do.en}{Boolean, if TRUE calculate metric. For computation
        specifics see source code of function \link{g.applymetrics}}
      \item{do.mad}{Boolean, if TRUE calculate metric. For computation
        specifics see source code of function \link{g.applymetrics}}
      \item{do.enmoa}{Boolean, if TRUE calculate metric. For computation
        specifics see source code of function \link{g.applymetrics}}
      \item{do.roll_med_acc_x}{Boolean, if TRUE calculate metric. For computation
        specifics see source code of function \link{g.applymetrics}}
      \item{do.roll_med_acc_y}{Boolean, if TRUE calculate metric. For computation
        specifics see source code of function \link{g.applymetrics}}
      \item{do.roll_med_acc_z}{Boolean, if TRUE calculate metric.
        For computation specifics see source code of function \link{g.applymetrics}}
      \item{do.dev_roll_med_acc_x}{Boolean, if TRUE calculate metric.
        For computation specifics see source code of function \link{g.applymetrics}}
      \item{do.dev_roll_med_acc_y}{Boolean, if TRUE calculate metric.
        For computation specifics see source code of function \link{g.applymetrics}}
      \item{do.dev_roll_med_acc_z}{Boolean, if TRUE calculate metric.
        For computation specifics see source code of function \link{g.applymetrics}}
      \item{do.bfen}{Boolean, if TRUE calculate metric. For computation
        specifics see source code of function \link{g.applymetrics}}
      \item{do.hfen}{Boolean, if TRUE calculate metric. For computation
        specifics see source code of function \link{g.applymetrics}}
      \item{do.hfenplus}{Boolean, if TRUE calculate metric. For computation
        specifics see source code of function \link{g.applymetrics}}
      \item{do.lfen}{Boolean, if TRUE calculate metric. For computation
        specifics see source code of function \link{g.applymetrics}}
      \item{do.lfx}{Boolean, if TRUE calculate metric. For computation
        specifics see source code of function \link{g.applymetrics}}
      \item{do.lfy}{Boolean, if TRUE calculate metric. For computation
        specifics see source code of function \link{g.applymetrics}}
      \item{do.lfz}{Boolean, if TRUE calculate metric. For computation
        specifics see source code of function \link{g.applymetrics}}
      \item{do.hfx}{Boolean, if TRUE calculate metric. For computation
        specifics see source code of function \link{g.applymetrics}}
      \item{do.hfy}{Boolean, if TRUE calculate metric. For computation
        specifics see source code of function \link{g.applymetrics}}
      \item{do.hfz}{Boolean, if TRUE calculate metric. For computation
        specifics see source code of function \link{g.applymetrics}}
      \item{do.bfx}{Boolean, if TRUE calculate metric. For computation
        specifics see source code of function \link{g.applymetrics}}
      \item{do.bfy}{Boolean, if TRUE calculate metric. For computation
        specifics see source code of function \link{g.applymetrics}}
      \item{do.bfz}{Boolean, if TRUE calculate metric. For computation
        specifics see source code of function \link{g.applymetrics}}
      \item{do.brondcounts}{Boolean, if TRUE calculate metric via R
        package activityCounts. We call them BrondCounts because
        there are large number of acitivty counts in the physical activity and
        sleep research field. By calling them Brond Counts we clarify that 
        these are the counts proposed by Jan Brond and implemented in R by
        Ruben Brondeel. The Brond Counts are intended to be an imitation of
        one the counts produced by one of the closed source ActiLife software
        by ActiGraph.}
      \item{lb}{Numeric, lower boundary of the frequency filter (in Hertz) as
        used in the filter-based metrics.}
      \item{hb}{Numeric, higher boundary of the frequency filter (in Hertz) as
        used in the filter-based metrics.}
      \item{n}{Numeric, order of the frequency filter as used in a variety of metrics.}
    }
  }
  
  \subsection{params_rawdata}{
    A list of parameters used to related to reading and pre-processing 
    raw data, excluding parameters related to metrics as those are in
    the params_metrics object.
    \describe{
      \item{backup.cal.coef}{Character. Default value is "retrieve".
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
        QC folder or set it to value "redo".}
      \item{minimumFileSizeMB}{Numeric. Minimum File size in MB required to enter processing,
        default 2MB. This argument can help
        to avoid having short uninformative files to enter the analyses. Given that a typical accelerometer
        collects several MBs per hour, the default setting should only skip the very tiny files.}
      \item{do.cal}{Boolean. Whether to apply auto-calibration or not by \link{g.calibrate}. Default and
        recommended setting is TRUE.}
      \item{imputeTimegaps}{Boolean to indicate whether timegaps larger than 1 sample should be imputed.
        Currently onlly used for .gt3x data and ActiGraph .csv format, where timegaps can be expected as a result of
        Actigraph's idle sleep.mode configuration that is turned on in some studies.}
      \item{spherecrit}{The minimum required acceleration value (in g) on both sides of 0 g
        for each axis. Used to judge whether the sphere is sufficiently populated}
      \item{minloadcrit}{The minimum number of hours the code needs to read for the
        autocalibration procedure to be effective (only sensitive to multitudes of 12 hrs, 
        other values will be ceiled). After loading these hours only extra data is loaded 
        if calibration error has not been reduced to under 0.01 g.}
      \item{printsummary}{Boolean. If TRUE will print a summary when done}
      \item{chunksize}{Numeric. Value between 0.2 and 1 to specificy the size of chunks to be 
        loaded as a fraction of a 12 hour period, e.g. 0.5 equals 6 hour chunks.
        The default is 1 (12 hrs). For machines with less than 4Gb of RAM memory a value
        below 1 is recommended.}
      \item{dynrange}{Numeric, provide dynamic range for accelerometer data to
        overwrite hardcoded 6 g for GENEA and 8 g for other brands}
      \item{interpolationType}{Integer to indicate type of interpolation to be used
        when resampling time series (mainly relevant for Axivity sensors),
        1=linear, 2=nearest neighbour}
      \item{all arguments that start with "rmc.".}{see function \link{read.myacc.csv} and
      \link{get_nw_clip_block_params}}
    }
  }   
  \subsection{params_cleaning}{
    A list of parameters used across all GGIR parts releated to masking or 
    imputing data, abbreviated as 'cleaning'.
    \describe{
      \item{do.imp}{Boolean. Whether to impute missing values (e.g. suspected of monitor non-wear) or not
        by \link{g.impute} in GGIR part2. Default and recommended setting is TRUE}
      \item{TimeSegments2ZeroFile}{Character. Path to csv-file holding the data.frame used for argument
        TimeSegments2Zero in function \link{g.impute}}
      \item{data_cleaning_file}{Character. Optional path to a csv file you create that holds four
        columns: ID, day_part5, relyonguider_part4, and night_part4. ID should hold the participant ID.
        Columns day_part5 and night_part4 allow you to specify which day(s) and
        night(s) need to be excluded from part 5 and 4, respectively. So, this will be done regardless
        of whether the rest of GGIR thinks those day(s)/night(s)
        are valid. Column relyonguider_part4 allows you to specify for which nights
        part 4 should fully rely on the guider. See also package vignette.}
      \item{excludefirstlast.part5}{Boolean. If TRUE then the first and last window 
      (waking-waking or midnight-midnight) are ignored in part 5.}
      \item{excludefirstlast}{Boolean. If TRUE then the first and last night of the measurement are
        ignored for the sleep assessment (part 4).}
      \item{excludefirst.part4}{Boolean. If TRUE then the first night of the measurement are
        ignored for the sleep assessment (part 4.}
      \item{excludelast.part4}{Boolean. If TRUE then the last night of the measurement are
        ignored for the sleep assessment.}
      \item{includenightcrit}{Numeric. Minimum number of valid hours per night (24 hour window between
        noon and noon), used for sleep assessment (part 4).}
      \item{minimum_MM_length.part5}{Numeric. Minimum length in hours of a MM day to be included
      in the cleaned part 5 results.}
      \item{selectdaysfile}{Character, Functionality designed for the London Centre
        of Longidutinal studies. Csv file holding the relation between device 
        serial numbers and measurement days of interest.}
      \item{strategy}{Numeric, how to deal with knowledge about study protocol.
        value = 1 means select data based on \code{hrs.del.start} and \code{hrs.del.end}.
        Value = 2 makes that only the data between the first
        midnight and the last midnight is used for imputation. Value = 3 only selects
        the most active X days in the file where X is specified by argument \code{ndayswindow}. 
        Value = 4 to only use the data after the first midnight. Used in GGIR part 2}
      \item{hrs.del.start}{Numeric, how many HOURS after start of experiment did wearing
        of monitor start? Used in GGIR part 2}
      \item{hrs.del.end}{Numeric, how many HOURS before the end of the experiment did 
        wearing of monitor definitely end? Used in GGIR part 2}
      \item{maxdur}{Numeric, How many DAYS after start of experiment did experiment
        definitely stop? (set to zero if unknown = default). Used in GGIR part2}
      \item{ndayswindow}{Numeric,  If \code{strategy} is set to 3 then this is the 
        size of the window as a number of days. Used in GGIR part2}
      \item{includedaycrit.part5}{Numeric. see \link{g.report.part5}}
      \item{includedaycrit}{Numeric, minimum required number of valid hours
        in day specific analysis (NOTE: there is no minimum required number of
        hours per day in the summary of an entire measurement, every available 
        hour is used to make the best possible inference on average metric value
        per average day)}
        \item{max_calendar_days}{Numeric, the maximum number of calendar days to include}
    }
  }
   \subsection{params_general}{
    A list of parameters used across all GGIR parts that do not fall in any of the other
    categories.
    \describe{
      \item{overwrite}{Boolean. Do you want to overwrite analysis for which milestone data exists?
        If overwrite=FALSE then milestone data from a previous analysis will
        be used if available and visual reports will not be created again.}
      \item{selectdaysfile}{Character. Do not use, this is legacy code for one specific data study.
        Character pointing at a csv file holding the relationship between device serial
        numbers (first column) and measurement dates of interest
        (second and third column). The date format should be dd/mm/yyyy. And the first row
        if the csv file is assumed to have a character variable names, e.g. "serialnumber"
        "Day1" and "Day2" respectively. Raw data will be extracted and stored in the output
        directory in a new subfolder named 'raw'.}
      \item{dayborder}{Numeric. Hour at which days start and end (default = 0), 
      value = 4 would mean 4 am}
      \item{do.parallel}{Boolean. whether to use multi-core processing
        (only works if at least 4 CPU cores are available).}
      \item{maxNcores}{Numeric. Maximum number of cores to use when argument do.parallel is set to true.
        GGIR by default uses the maximum number of available cores, but this argument
        allows you to set a lower maximum.}
      \item{acc.metric}{Boolean. Which one of the metrics do you want to consider to analyze L5.
        The metric of interest need to be calculated in M.}
      \item{part5_agg2_60seconds}{Boolean. Wether to use aggregate epochs to 60 seconds
        as part of the part 5 analysis.}
      \item{print.filename}{Boolean. Whether to print the filename before before analysing
        it (default is FALSE). Printing the filename can be useful to investigate
        problems (e.g. to verify that which file is being read).}
      \item{desiredtz}{Character, desired timezone: see also http://en.wikipedia.org/wiki/Zone.tab}
      \item{configtz}{Character, Only functional for AX3 cwa data at the moment. 
        Timezone in which the accelerometer was configured. Only use this argument
        if the timezone of configuration and timezone in which recording took
        place are different.}
      \item{sensor.location}{Character, see \link{g.sib.det}}
      \item{acc.metric}{Character, see \link{g.sib.det}}
      \item{windowsizes}{Numeric vector, three values to indicate the lengths of the 
        windows as in c(window1,window2,window3): window1 is the short epoch length
        in seconds and by default 5 this is the time window over which acceleration and
        angle metrics are calculated, window2 is the long epoch length in seconds 
        for which non-wear and signal clipping are defined, default 900. However, 
        window3 is the window length of data used for non-wear detection and by default
        3600 seconds. So, when window3 is larger than window2 we use overlapping windows,
        while if window2 equals window3 non-wear periods are assessed by non-overlapping
        windows. Window2 is expected to be a multitude of 60 seconds.}
      \item{idloc}{Numeric. If idloc = 1 (default) the code assumes that ID
        number is stored in the obvious header field. Note that for ActiGraph data
        the ID never stored in the file header.  For value set to 2, 5, 6, and 7, GGIR
        looks at the filename and extracts the character string preceding the first 
        occurance of a '_', ' ' (space), '.' (dot), and '-', respecitvely. You may have 
        noticed that idloc 3 and 4 are skipped, they were used for one study in 2012,
        and not actively maintained anymore, but because it is legacy code not omitted.}
    }
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

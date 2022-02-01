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
  g.part2(datadir=c(), metadatadir=c(), f0=c(), f1=c(), 
        myfun=c(), params_cleaning = c(), params_247 = c(),
        params_phyact = c(), params_output = c(), params_general = c(), ...)
}
\arguments{
  \item{datadir}{
    Directory where the accelerometer files are stored or
    list, e.g. "C:/mydata" of accelerometer filenames
    and directories, e.g.
    c("C:/mydata/myfile1.bin", "C:/mydata/myfile2.bin").
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
  \item{myfun}{
    External function object to be applied to raw data.
    See details \link{applyExtFunction}.
  }
  \item{params_cleaning}{
    See \link{g.part1}
  }
  \item{params_247}{
    See details
  }
  \item{params_phyact}{
    See details
 }
  \item{params_output}{
    See details
  }
  \item{params_general}{
    See \link{g.part1}
  }
  \item{...}{
    To ensure compatibility with R scripts written for older GGIR versions,
    the user can also provide parameters listed in the params_ objects as direct argument.
  }
}
\value{
  The function provides no values, it only ensures that other functions are called
  and that their output is stored in the folder structure as created with \link{g.part1}.
}
\details{
  GGIR comes with many processing parameters, which have been thematically grouped in
  parameter objects (R list). By running print(load_params()) you can
  see the default values of all the parameter objects. When g.part 2 is used via \link{g.shell.GGIR}
  you have the option to specifiy a configuration file, which will overrule the default
  parameter values. Further, as user you can set parameter values as input argument to both g.part2
  and \link{g.shell.GGIR}. Directly specified argument overrule the configuration file and default values.
  
  See the GGIR package vignette for a more elaborate overview of parameter objects and their usage across GGIR.
  
  The parameter objects used by GGIR part 2 (g.part2) that are no already discussed in
  \link{g.part1} are:
  
  \subsection{params_output}{
    A list of parameters used to specify whether and how GGIR stores its output at various stages of the
    process.
    \describe{
      \item{storefolderstructure}{Boolean. Store folder structure of the accelerometer data.}
      \item{do.part3.pdf}{Boolean. In g.part3: Whether to generate a pdf for part 3 (default is TRUE).}
      \item{timewindow}{In g.part5: Timewindow over which summary statistics are derived.
        Value can be "MM" (midnight to midnight), "WW" (waking time to waking time), 
        or both c("MM","WW").}
      \item{save_ms5rawlevels}{Boolean, whether to save the time series 
        classification (levels) as a csv files.}
      \item{save_ms5raw_format}{Character string to specify how data should
        be stored: either "csv" (default) or "RData". Only used if save_ms5rawlevels=TRUE.}
      \item{save_ms5raw_without_invalid}{Boolean to indicate whether to remove
        invalid days from the time series output files. Only used if save_ms5rawlevels=TRUE.}
      \item{epochvalues2csv}{Boolean. If TRUE then epoch values are exported to a
        CSV spreadsheet. Here, non-wear time is imputed where possible (default = FALSE).}
      \item{do.sibreport}{Boolean. Applied in g.part5. Boolean to indicate whether
        to generate report for the sustained inactivity bouts (sib).}
      \item{do.visual}{Boolean. If g.part4 is run with do.visual == TRUE then
        the function will generate a pdf with a visual representation of the
        overlap between the sleeplog entries and the accelerometer detections.
        This can be used to visualy verify that the sleeplog entries do
        not come with obvious mistakes.}
      \item{outliers.only}{Boolean. Relevant for do.visual == TRUE. Outliers.only == FALSE
        will visualise all available nights in the data. Outliers.only == TRUE will visualise
        only for nights with a difference in onset or waking time
        larger than the variable of argument criterror.}
      \item{criterror}{Numeric. Relevant for do.visual == TRUE and outliers.only == TRUE.
        criterror specifies the number of minimum number of hours difference
        between sleep log and  accelerometer estimate for the night to be
        included in the visualisation.}
      \item{visualreport}{Boolean. If TRUE then generate visual report based on combined output from part 2
        and 4. This is in beta-version at the moment.}
      \item{viewingwindow}{Numeric. Centre the day as displayed around noon (value = 1) or
        around midnight (value = 2).}
      \item{week_weekend_aggregate.part5}{Boolean, see \link{g.report.part5}}
      \item{dofirstpage}{Boolean, see \link{g.plot5}}
      \item{timewindow}{Timewindow over which summary statistics are derived.
        Value can be "MM" (midnight to midnight), "WW" (waking time to waking time),
        or both c("MM","WW").}
    }
  }
  \subsection{params_phyact}{
    A list of parameters releated to physical activity as used in GGIRpart2 and GGIRpart5.
    \describe{
      \item{threshold.lig}{Numeric. In g.part5: Threshold for light physical activity to
        separate inactivity from light. Value can be one number or an array of multiple
        numbers, e.g. threshold.lig =c(30,40). If multiple numbers are entered then
        analysis will be repliced for each combination of threshold values. Threshold is
        applied to the first metric in the milestone data, so if you have only specified
        do.ENMO == TRUE then it will be applied to ENMO.}
      \item{threshold.mod}{Numeric. In g.part5: Threshold for moderate physical activity 
        to separate light from moderate. Value can be one number or an array of 
        multiple numbers, e.g. threshold.mod =c(100,110).
        If multiple numbers are entered then analysis will be repliced for each
        ombination of threshold values. Threshold is applied to the first metric in the
        milestone data, so if you have only specified do.ENMO == TRUE then it will be
        applied to ENMO.}
      \item{threshold.vig}{Numeric. In g.part5: Threshold for vigorous physical activity 
        to separate moderate from vigorous. Value can be one number or an array of 
        multiple numbers, e.g. threshold.mod =c(400,500). If multiple numbers are
        entered then analysis will be repliced for each combination of threshold values.
        Threshold is applied to the first metric in the milestone data, so if you
        have only specified do.ENMO == TRUE then it will be applied to ENMO.}
      \item{closedbout}{Boolean, see \link{g.getbout}}
      \item{frag.metrics}{Character, see \link{g.fragmentation}}
      \item{mvpathreshold}{Numeric, Acceleration threshold for MVPA estimation in GGIR part2.
        This can be a single number or an array of numbers, e.g. c(100,120). In the later case 
        the code will estimate MVPA seperately for each threshold. If this variable 
        is left blank c() then MVPA is not estimated}
      \item{boutcriter}{Numeric, The variable boutcriter is a number between
        0 and 1 and defines what fraction of a bout needs to be above the 
        mvpathreshold, only used in GGIR part 2}
      \item{mvpadur}{Numeric, default = c(1,5,10). Three bout duration for which 
        MVPA will be calculated. Only used in GGIR part 2}
      \item{bout.metric}{Numeric, Specify a metric for bout detection. A description of these
        bout metrics can be found in the new function \link{g.getbout}} 
      \item{boutdur.mvpa}{Numeric, see Durations of mvpa bouts in minutes to be extracted.
        The default values is c(1,5,10) and will start with the identification of
        10 minute bouts, followed by 5 minute bouts in the rest of the data, and followed
        by 1 minute bouts in the rest of the data.}
      \item{boutdur.in}{Numeric, see  Durations of inactivty bouts in minutes to be
        extracted. Inactivity bouts are detected in the segments of the data which
        were not labelled as sleep or MVPA bouts. The default duration values
        is c(10,20,30), this will start with the identification of 30 minute bouts,
        followed by 20 minute bouts in the rest of the data, and followed by 10 minute
        bouts in the rest of the data.}
      \item{boutdur.lig}{Numeric, see  Durations of light activty bouts in minutes
        to be extracted. Light activity bouts are detected in the segments of the data
        which were not labelled as sleep, MVPA, or inactivity bouts. The default
        duration values is c(1,5,10), this will start with the identification of 
        10 minute bouts, followed by 5 minute bouts in the rest of the data, and followed
        by 1 minute bouts in the rest of the data.}
      \item{boutcriter.in}{Numeric, see  A number between 0 and 1 and defines what fraction
        of a bout needs to be below the light threshold}
      \item{boutcriter.lig}{Numeric, see A number between 0 and 1 and defines what
        fraction of a bout needs to be between the light and moderage threshold}
      \item{boutcriter.mvpa}{Numeric, see A number between 0 and 1 and defines
        what fraction of a bout needs to be above the mvpathreshold}
    }
  }
  \subsection{params_247}{
    A list of parameters releated to description of 24/7 behaviours that do not fall
    under conventional physical activity or sleep outcomes, these parameters are used
    in GGIRpart2 and GGIRpart5: 
    \describe{
      \item{qwindow}{Numeric or character, To specify windows over which all
        variables are calculated, e.g. acceleration distirbution, number of valid
        hours, LXMX analysis, MVPA. If value = c(0,24), which is the default, 
        all variables will only be calculated over the full 24 hours in a day, If
        value =c(8,24) variables will be calculated over the window 0-8, 8-24 and 0-24.
        All days in the recording will be segmented based on these values. If you want
        to use a day specific segmentation then you can set qwindow to be
        the full path to activity diary file. See documentation \link{g.conv.actlog}
        for details.}
      \item{qwindow_dateformat}{Numeric, see \link{g.conv.actlog}}
      \item{M5L5res}{Numeric, resolution of L5 and M5 analysis in minutes 
        (default: 10 minutes)}
      \item{winhr}{Numeric, Vector of window size(s) (unit: hours) of L5 and
        M5 analysis (dedault = 5 hours)}
      \item{qlevels}{Numeric, array of percentiles for which value needs to be extracted.
        These need to be expressed as a fraction of 1, e.g. c(0.1, 0.5, 0.75). 
        There is no limit to the number of percentiles. If left empty then percentiles will not be 
        extracted. Distribution will be derived from short epoch metric data.}
      \item{ilevels}{Numeric, Levels for acceleration value frequency
        distribution in mg, e.g. c(0,100,200). There is no limit to the number of levels.}
      \item{window.summary.size}{Numeric, Functionality designed for the London Centre
        of Longidutinal studies. Size in minutes of the summary window}
      \item{iglevels}{Numeric, Levels for acceleration value frequency distribution
        in mg used for intensity gradient calculation (according to the method by 
        Rowlands 2018). By default this is argument is empty and the intensity gradient
        calculation is not done. The user can either provide a single value (any) to 
        make the intensity gradient use the bins c(seq(0,4000,by=25),8000) or the 
        user could specify their own distribution. There is no constriction to the 
        number of levels.}
      \item{IVIS_windowsize_minutes}{Numeric, see \link{g.IVIS}}
      \item{IVIS_epochsize_seconds}{depricated. Numeric, see \link{g.IVIS}}
      \item{IVIS.activity.metric}{Numeric, see \link{g.IVIS}}
      \item{qM5L5}{Numeric, see \link{g.getM5L5}}
      \item{MX.ig.min.dur}{Numeric, see \link{g.getM5L5}}
      \item{LUXthresholds}{Numeric. Vector with numeric sequece corresponding to
        the thresholds used to calculated time spent in LUX ranges.}
      \item{LUX_cal_constant}{Numeric, if both LUX_cal_constant and LUX_cal_exponent are 
        provided LUX LUX values are converted based on formula y = constant * exp(x * exponent)}
      \item{LUX_cal_exponent}{Numeric, if both LUX_cal_constant and LUX_cal_exponent are provided LUX
        LUX values are converted based on formula y = constant * exp(x * exponent)}
      \item{LUX_day_segments}{Numeric vector with hours at which the day should be segmented for
        the LUX analysis.}
      \item{L5M5window}{Argument depricated after version 1.5-24. 
        This argument used to define the start and end time, in 24 hour clock hours,
        over which L5M5 needs to be calculated. Now this is done with argument qwindow}
    }
  }
}
\examples{
  \dontrun{
    metadatadir = "C:/myresults/output_mystudy"
    g.part2(metadatadir)
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
  }
}
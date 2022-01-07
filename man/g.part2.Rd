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
    List, see \link{g.part1}
  }
  \item{params_247}{
    List of parameters releated to description of 24/7 behaviour that does not fall
    under conventional physical activity or sleep analysis, these parameters are used
    in GGIRpart2 and GGIRpart5: 
    This includes parameter qwindow, which can be numeric or a character (see \link{g.analyse})
    Numeric parameters to be included are: qwindow_dateformat (see \link{g.conv.actlog});
    M5L5res, winhr, qlevels, ilevels,window.summary.size, iglevels (see \link{g.analyse});
    IVIS_windowsize_minutes, IVIS_epochsize_seconds, IVIS.activity.metric (see \link{g.IVIS});
    qM5L5, MX.ig.min.dur (see \link{g.getM5L5}), and; LUXthresholds, LUX_cal_constant,
    LUX_cal_exponent, LUX_day_segments, L5M5window (see details).
  }
  \item{params_phyact}{
    List of parameters releated to physical activity as used in GGIRpart2 and GGIRpart5: 
    Boolean parameters to be included are: closedbout (see \link{g.getbout}).
    Character parameters to be included are: frag.metrics (see \link{g.fragmentation}).
    Numeric parameters to be included are: mvpathreshold, boutcriter, 
    mvpadur (see \link{g.analyse}); bout.metric (see \link{g.getbout}); 
    boutdur.mvpa, boutdur.in, boutdur.lig, boutcriter.in,boutcriter.lig, 
    boutcriter.mvpa (see \link{identify_levels}); threshold.lig, threshold.mod,
    threshold.vig (see details).
 }
  \item{params_output}{
    List of parameters releated to how GGIR stores its output at various stages of the
    process: 
    Boolean parameters to be included are: storefolderstructure, save_ms5rawlevels, 
    save_ms5raw_without_invalid, do.part3.pdf, epochvalues2csv, outliers.only, 
    do.sibreport, do.visual, visualreport (see details);
    week_weekend_aggregate.part5 (see \link{g.report.part5}; dofirstpage (see \link{g.plot5};
    Character parameters to be included are: save_ms5raw_format, timewindow (see details)
    Numeric parameters to be included are: criterror, viewingwindow (see details).
  }
  \item{params_general}{
    List, see \link{g.part1}
  }
  \item{...}{
    To enable compatibility with older GGIR version, 
    user can also provide individual parameters as separate argument, e.g. strategy = 2.
  }
}
\value{
  The function provides no values, it only ensures that other functions are called
  and that their output is stored in the folder structure as created with \link{g.part1}.
}
\details{
  params_output parameters not documented elsewhere:
  \cr
  \itemize{
    \item \code{storefolderstructure} Store folder structure of the accelerometer data.
    \item \code{do.part3.pdf} In g.part3: Whether to generate a pdf for part 3 (default is TRUE).
    \item \code{timewindow} In g.part5: Timewindow over which summary statistics are derived.
      Value can be "MM" (midnight to midnight), "WW" (waking time to waking time), 
      or both c("MM","WW").
    \item \code{save_ms5rawlevels} Boolean, whether to save the time series 
      classification (levels) as a csv files.
    \item \code{save_ms5raw_format} Character string to specify how data should
      be stored: either "csv" (default) or "RData". Only used if save_ms5rawlevels=TRUE.
    \item \code{save_ms5raw_without_invalid} Boolean to indicate whether to remove
      invalid days from the time series output files. Only used if save_ms5rawlevels=TRUE.
    \item \code{epochvalues2csv} If TRUE then epoch values are exported to a
      CSV spreadsheet. Here, non-wear time is imputed where possible (default = FALSE).
    \item \code{do.sibreport} Applied in g.part5. Boolean to indicate whether
      to generate report for the sustained inactivity bouts (sib).
    \item \code{do.visual} If g.part4 is run with do.visual == TRUE then
      the function will generate a pdf with a visual representation of the
      overlap between the sleeplog entries and the accelerometer detections.
      This can be used to visualy verify that the sleeplog entries do
      not come with obvious mistakes.
    \item \code{outliers.only} Relevant for do.visual == TRUE. Outliers.only == FALSE
      will visualise all available nights in the data. Outliers.only == TRUE will visualise
      only for nights with a difference in onset or waking time
      larger than the variable of argument criterror.
    \item \code{criterror} Relevant for do.visual == TRUE and outliers.only == TRUE.
      criterror specifies the number of minimum number of hours difference
      between sleep log and  accelerometer estimate for the night to be
      included in the visualisation.
    \item\code{visualreport} If TRUE then generate visual report based on combined output from part 2
      and 4. This is in beta-version at the moment.
    \item\code{viewingwindow} Centre the day as displayed around noon (value = 1) or
      around midnight (value = 2).
  }
  \cr
  params_phyact parameters not documented elsewhere:
  \cr
  \itemize{
    \item \code{threshold.lig} In g.part5: Threshold for light physical activity to
      separate inactivity from light. Value can be one number or an array of multiple
      numbers, e.g. threshold.lig =c(30,40). If multiple numbers are entered then
      analysis will be repliced for each combination of threshold values. Threshold is
      applied to the first metric in the milestone data, so if you have only specified
      do.ENMO == TRUE then it will be applied to ENMO.
    \item \code{threshold.mod} In g.part5: Threshold for moderate physical activity 
      to separate light from moderate. Value can be one number or an array of 
      multiple numbers, e.g. threshold.mod =c(100,110).
      If multiple numbers are entered then analysis will be repliced for each
      ombination of threshold values. Threshold is applied to the first metric in the
      milestone data, so if you have only specified do.ENMO == TRUE then it will be
      applied to ENMO.
    \item \code{threshold.vig} In g.part5: Threshold for vigorous physical activity 
      to separate moderate from vigorous. Value can be one number or an array of 
      multiple numbers, e.g. threshold.mod =c(400,500). If multiple numbers are
      entered then analysis will be repliced for each combination of threshold values.
      Threshold is applied to the first metric in the milestone data, so if you
      have only specified do.ENMO == TRUE then it will be applied to ENMO.
  }
  \cr
  params_247 parameters not documented elsewhere:
  \cr
  \itemize{
    \item \code{LUXthresholds} Vector with numeric sequece corresponding to the thresholds used to calculated time
      spent in LUX ranges.
    \item \code{LUX_cal_constant} Numeric, if both LUX_cal_constant and LUX_cal_exponent are provided LUX
      LUX values are converted based on formula y = constant * exp(x * exponent)
    \item \code{LUX_cal_exponent}  Numeric, if both LUX_cal_constant and LUX_cal_exponent are provided LUX
      LUX values are converted based on formula y = constant * exp(x * exponent)
    \item \code{LUX_day_segments} Numeric vector with hours at which the day should be segmented for
      the LUX analysis.
    \item \code{L5M5window} Argument depricated after version 1.5-24. This argument used to define the start
      and end time, in 24 hour clock hours, over which L5M5 needs to be calculated.
      Now this is done with argument qwindow
    }
  \cr
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

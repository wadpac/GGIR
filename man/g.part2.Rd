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
  g.part2(datadir = c(), metadatadir = c(), f0 = c(), f1 = c(), 
        myfun = c(), params_cleaning = c(), params_247 = c(),
        params_phyact = c(), params_output = c(), params_general = c(), ...)
}
\arguments{
  \item{datadir}{
    Directory where the accelerometer files are stored, e.g. 
    "C:/mydata", or list of accelerometer filenames and directories, e.g. 
    c("C:/mydata/myfile1.bin", "C:/mydata/myfile2.bin").
  }
  \item{metadatadir}{
    Directory that holds a folder 'meta' and inside this a folder 'basic'
    which contains the milestone data produced by \link{g.part1}. The folderstructure
    is normally created by \link{g.part1} and \link{g.shell.GGIR} will recognise what
    the value of metadatadir is.
  }
  \item{f0}{
    File index to start with (default = 1). Index refers to the filenames sorted
    in alphabetical order
  }
  \item{f1}{
    File index to finish with (defaults to number of files available, i.e., f1 = 0)
  }
  \item{myfun}{
    External function object to be applied to raw data.
    See details \link{applyExtFunction}.
  }
  \item{params_cleaning}{
   See details in \link{g.shell.GGIR}.
  }
  \item{params_247}{
   See details in \link{g.shell.GGIR}.
  }
  \item{params_phyact}{
   See details in \link{g.shell.GGIR}.
 }
  \item{params_output}{
   See details in \link{g.shell.GGIR}.
  }
  \item{params_general}{
   See details in \link{g.shell.GGIR}.
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
  
   See the GGIR package vignette or the details section in \link{g.shell.GGIR} for a more elaborate overview of parameter objects and their usage across GGIR.
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
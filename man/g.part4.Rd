\name{g.part4}
\alias{g.part4}
\title{
  Labels detected sustained inactivity periods by g.part3 as either
  part of the Sleep Period Time window or not
}
\description{
  Combines output from g.part3 and guider information to estimate
  sleep variables. See vignette paragraph "Sleep and full day
  time-use analysis in GGIR" for an elaborate descript of the sleep detection.
}
\usage{
 g.part4(datadir = c(), metadatadir = c(), f0 = f0, f1 = f1, idloc = 1,
    storefolderstructure = FALSE, overwrite = FALSE,
    desiredtz = "", data_cleaning_file = c(), params_sleep = c(), 
    params_metrics = c(), ...)
}
\arguments{
  \item{datadir}{
    Directory where the accelerometer files are stored or list
    of accelerometer filenames and directories
  }
  \item{metadatadir}{
    Directory that holds a folders 'meta' and inside this a folder
    'basic' which contains the milestone data produced by g.part1.
    The folderstructure is normally created by g.part3. When using
    g.part4 via g.shell.GGIR then g.shell.GGIR will automatically
    recognise what the value of metadatadir is, so the user does not
    need to specify this.
  }
  \item{f0}{
    File index to start with (default = 1). Index refers to the
    filenames sorted in increasing order
  }
  \item{f1}{
    File index to finish with (defaults to number of files available)
  }
  \item{idloc}{
    If value = 1 (default) the code assumes that ID number is stored in
    the obvious header field. If value = 2 the code uses the
    character string preceding the character '_' in the filename as
    the ID number
  }
  \item{storefolderstructure}{
    Store folder structure of the accelerometer data
  }
  \item{overwrite}{
    Overwrite previously generated milestone data by this function
    for this particular dataset. If FALSE then it will skip the
    previously processed files (default = FALSE).
  }
  \item{desiredtz}{
    See \link{g.getmeta}
  }
  \item{data_cleaning_file}{
    Optional path to a csv file you create that holds four
    columns: ID, day_part5, relyonguider_part4, 
    and night_part4. ID should hold the participant ID.
    Columns day_part5 and night_part4 allow you to 
    specify which day(s) and night(s) need to be excluded from
    part 5 and 4, respectively. So, this will be done regardless
    of whether the rest of GGIR thinks those day(s)/night(s)
    are valid.
    Column relyonguider_part4 allows you to specify for
    which nights part 4 should fully rely on the guider. See
    also package vignette.
  }
  \item{params_sleep}{
    List of parameters used for sleep analysis (GGIR part 3, 4, and 5): 
    see documentation \link{g.part3}.
  }
  \item{params_metrics}{
    List of parameters used for metrics extraction (GGIR part 1): 
    see documentation \link{g.part1}.
  }
  \item{...}{
    To enable compatibility with older GGIR version, user can also provide
    individual params_sleep parameters as separate argument, e.g. ignorenonwear = FALSE.
  }
}
\value{
  The function does not produce values but generates an RData file
  in the milestone subfolder ms4.out which incudes a dataframe
  named \code{nightsummary}. This dataframe is used in g.report.part4 to create
  two reports one per night and one per person. See package vignette 
  paragraph "Output part 4" for description of all the variables.
}

\examples{
  \dontrun{
    metadatadir = "C:/myfolder/meta" # assumes that there is a subfolder in
    # metadatadir named 'ms3.out' containing the output from g.part3
    g.part4(metadatadir=metadatadir)
  }
}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
}
\references{
  \itemize{
    \item van Hees VT, Sabia S, et al. (2018) AEstimating sleep parameters
    using an accelerometer without sleep diary, Scientific Reports.
    \item van Hees VT, Sabia S, et al. (2015) A novel, open access method
    to assess sleep duration using a wrist-worn accelerometer, PLoS ONE.
  }
}

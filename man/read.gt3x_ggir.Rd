\name{read.gt3x_ggir}
\alias{read.gt3x_ggir}
\title{Read GT3X}
\usage{
read.gt3x_ggir(
  path,
  verbose = FALSE,
  asDataFrame = FALSE,
  imputeZeroes = FALSE,
  flag_idle_sleep = FALSE,
  cleanup = FALSE,
  ...,
  add_light = FALSE
)
}
\arguments{
\item{path}{Path to gt3x folder}

\item{verbose}{print diagnostic messages}

\item{asDataFrame}{convert to an \code{activity_df}, see
\code{as.data.frame.activity}}

\item{imputeZeroes}{Impute zeros in case there are missingness?
  Default is FALSE, in which case
  the time series will be incomplete in case there is missingness.}

\item{flag_idle_sleep}{flag idle sleep mode.  If \code{imputeZeroes = TRUE},
this finds where all 3 axes are zero.}

\item{cleanup}{should any unzipped files be deleted?}

\item{...}{additional arguments to pass to \code{parseGT3X} C++ code}

\item{add_light}{add light data to the \code{data.frame} if data exists in the
GT3X}
}
\value{
  A numeric matrix with 3 columns (X, Y, Z) and the following
  attributes:
  \itemize{
    \item \code{start_time} :  Start time from info file in \code{POSIXct} format.
    \item \code{subject_name} : Subject name from info file
    \item \code{time_zone} : Time zone from info file
    \item \code{missingness} : Named integer vector. Names are \code{POSIXct}
    timestamps and values are the number of missing values.
  }
}
\description{
  Read activity samples from a GT3X file as a matrix.
  Please note that all timestamps are in local time (of the device)
  even though they are represented as \code{POSIXct} with GMT timezone.
  
  The code in this function is a modified version of the read.gt3x in that it aids batch-loading of modern gt3x files. A pull request has been made to feed these enhancements back into the original code base
  https://github.com/THLfi/read.gt3x/pull/40. If and when merged we intend to deprecate the GGIR version of the code and make a direct dependency.
}
\note{
  The timestamps in the .gt3x data format are saved in .NET format, which is
  nanoseconds in local time since 0001-01-01.
  This is a bit tricky to parse into an R datetime format. DateTimes are
  therefore represented as \code{POSIXct} format with the
  'GMT' timezone attribute, which is false; the datetime actually
  represents local time.
}

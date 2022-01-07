\name{parseGT3Xggir}
\alias{parseGT3Xggir}
\title{Parse activity samples from a GT3X file with batch loading}
\usage{
  parseGT3Xggir(
    filename,
    max_samples,
    scale_factor,
    sample_rate,
    start_time,
    batch_begin = 0L,
    batch_end = 0L,
    verbose = FALSE,
    debug = FALSE,
    impute_zeroes = FALSE
  )
}
\arguments{
\item{filename}{(char*) path to a log.bin file inside the unzipped gt3x folder, which contains the activity samples}

  \item{max_samples}{Maximum number of rows to parse. The returned matrix will always contain this number of rows, having zeroes if
  not data is found.}

\item{scale_factor}{Scale factor for the activity samples.}

\item{sample_rate}{sampling rate for activity samples.}

\item{start_time}{starting time of the sample recording.}

\item{batch_begin}{first second in time relative to start of raw non-imputed recording to include in this batch}

\item{batch_end}{last second in time relative to start of raw non-imputed recording to include in this batch}

\item{verbose}{Print the parameters from the log.bin file and other messages?}

\item{debug}{Print information for every activity second}

\item{impute_zeroes}{Impute zeros in case there are missingness?}
}
\value{
  Returns a matrix with max_samples rows and 3 columns with the acceleration samples.
  The matrix has attributes
  "time_index", "missingness", "start_time_log", "sample_rate", "impute_zeroes".
}
\description{
  Parse activity samples from a GT3X file.
  The code in this function is a modified version of the read.gt3x in that it aids batch-loading of modern gt3x files. A pull request has been made to feed these enhancements back into the original code base
  https://github.com/THLfi/read.gt3x/pull/40. If and when merged we intend to deprecate the GGIR version of the code and make a direct dependency.
}

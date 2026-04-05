\name{build_epoch_table}
\alias{build_epoch_table}
\title{
  Build a flat epoch table from ms5.outraw time series
}
\description{
  Reads epoch-level files from \code{meta/ms5.outraw} and constructs a single
  flat data frame with one row per epoch.
}
\usage{
build_epoch_table(
  metadatadir = c(),
  params_general = c(),
  params_phyact = c(),
  results_dir = NULL,
  verbose = TRUE
)
}
\arguments{
  \item{metadatadir}{
    Directory that holds a folder 'meta' and inside this a folder 'basic'
    which contains the milestone data produced by \link{g.part1}. The folder structure
    is normally created by \link{g.part1} and \link{GGIR} will recognise what
    the value of metadatadir is.
  }
  \item{params_general}{
    See details in \link{GGIR}.
  }
  \item{params_phyact}{
    See details in \link{GGIR}.
  }
  \item{results_dir}{
    Optional character scalar. Path to \code{results}. If \code{NULL}, it is
    derived from \code{metadatadir}.
  }
  \item{verbose}{
    See details in \link{GGIR}.
  }
}
\details{
  The function:
  \enumerate{
    \item Selects an ms5 configuration folder (prefer requested threshold
          combination if available).
    \item Reads time-series files (CSV preferred over RData).
    \item Infers missing \code{filename} and maps \code{filename -> ID}
          from \code{results/part2_summary.csv} when needed.
    \item Derives \code{calendar_date} and \code{weekday} from epoch
          \code{timenum} using timezone information.
    \item Harmonizes column sets across recordings by filling missing columns
          with \code{NA}.
  }
}
\value{
  Either \code{NULL} (if no usable epoch data were found), or a list with:
  \itemize{
    \item \code{epoch_df}: data frame with one row per epoch.
    \item \code{config_name}: selected ms5 configuration folder name.
  }
}
\seealso{
  \code{\link{write_epoch_parquet}},
  \code{\link{write_dashboard_parquet}}
}
\examples{
\dontrun{
res <- build_epoch_table(
  metadatadir = "path/to/output_run/output_test_run",
  params_general = list(desiredtz = "UTC"),
  params_phyact = list(part6_threshold_combi = "WW_L40M100V400_T5A5"),
  verbose = TRUE
)

if (!is.null(res)) {
  names(res)
  nrow(res$epoch_df)
  res$config_name
}
}
}
\author{
  Samuel Timileyin Afolabi <samuelafolabimails@gmail.com>
}
\keyword{internal}

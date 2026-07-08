\name{write_dashboard_parquet}
\alias{write_dashboard_parquet}
\title{
  Write consolidated dashboard Parquet output
}
\description{
  Creates a single dashboard-ready Parquet file by combining GGIR summary outputs
  and attaching nested epoch-level time series by day.
}
\usage{
write_dashboard_parquet(
  metadatadir = c(),
  params_output = c(),
  params_general = c(),
  params_phyact = c(),
  verbose = TRUE
)
}
\arguments{
  \item{metadatadir}{
    Directory that holds a folder 'meta' and inside this a folder 'ms5.outraw'
    which contains the milestone data produced by \link{g.part5}. The folder structure
    is normally created by \link{g.part5} and \link{GGIR} will recognise what
    the value of metadatadir is.
  }
  \item{params_output}{
    See details in \link{GGIR}.
  }
  \item{params_general}{
    See details in \link{GGIR}.
  }
  \item{params_phyact}{
    See details in \link{GGIR}.
  }
  \item{verbose}{
    See details in \link{GGIR}.
  }
}
\details{
  The function reads already-generated CSV reports, starts from Part 5 day summary
  as the base table, and left-joins additional fields from:
  \itemize{
    \item Part 4 night summary
    \item Part 2 day summary
    \item Part 2 participant summary
    \item quality-control report
  }

  It then calls \code{build_epoch_lists_by_day} to construct a nested
  \code{epochs} list-column and merges that into the day-level table by
  \code{ID} and \code{calendar_date}.

  Column names are cleaned for SQL-friendly use, common columns are type-cast,
  and Parquet key-value metadata is embedded (variable definitions, thresholds,
  behavioral-code mapping, acceleration metric, and estimated epoch length).

  Output filename is ID-based for single-participant exports and falls back to a
  multi-participant name otherwise.

  The generated Parquet file can be opened directly in the GGIR Web Dashboard
  (\url{https://wadpac.github.io/ggir-web-dashboard/}), where processing
  happens entirely in the browser.
}
\value{
  Invisibly returns the written Parquet file path, or \code{NULL} (invisibly) if
  export is skipped because required data are missing. The generated Parquet file
  can be opened directly in the GGIR Web Dashboard:
  \url{https://wadpac.github.io/ggir-web-dashboard/}
}
\seealso{
  \code{\link{GGIR}},
  \code{\link{write_epoch_parquet}},
  \code{\link{build_epoch_table}}
}
\examples{
\dontrun{
write_dashboard_parquet(
  metadatadir = "path/to/output_run/output_test_run",
  params_output = list(save_dashboard_parquet = TRUE),
  params_general = list(desiredtz = "", acc.metric = "ENMO"),
  params_phyact = list(part6_threshold_combi = "40_100_400"),
  verbose = TRUE
)
}
}
\author{
  Samuel Timileyin Afolabi <samuelafolabimails@gmail.com>
}
\keyword{IO}

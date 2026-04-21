\name{write_epoch_parquet}
\alias{write_epoch_parquet}
\title{
  Write flat epoch-level Parquet output
}
\description{
  Builds a flat epoch-level table from \code{meta/ms5.outraw} time-series data
  and writes it as a Parquet file. This is a legacy/compatibility export pathway;
  the preferred dashboard export is \code{\link{write_dashboard_parquet}}.
}
\usage{
write_epoch_parquet(
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
  This function uses \code{\link{build_epoch_table}} to read and harmonize
  epoch-level data (one row per epoch), then applies type coercions and writes
  \code{results/parquet/ggir_epochs.parquet}.

  Metadata stored in the Parquet footer includes export type, creation timestamp,
  selected ms5 configuration name, acceleration metric, and estimated epoch
  length in seconds.

  This export path is mainly useful for workflows that prefer a separate flat
  epoch table rather than nested epochs inside the dashboard parquet.
}
\value{
  Invisibly returns the written Parquet file path, or \code{NULL} (invisibly)
  when export is skipped.
}
\seealso{
  \code{\link{GGIR}},
  \code{\link{build_epoch_table}},
  \code{\link{write_dashboard_parquet}}
}
\examples{
\dontrun{
write_epoch_parquet(
  metadatadir = "path/to/output_run/output_test_run",
  params_output = list(),
  params_general = list(desiredtz = "", acc.metric = "ENMO"),
  params_phyact = list(part6_threshold_combi = "40_100_400"),
  verbose = TRUE
)
}
}
\author{
  Samuel Timileyin Afolabi <samuelafolabimails@gmail.com>
}
\keyword{internal}

\name{part6PairwiseAggregation}
\alias{part6PairwiseAggregation}
\title{part6PairwiseAggregation}
\description{
  Pairwise aggregation of the time series of a group.
}
\usage{
  part6PairwiseAggregation(outputdir = NULL, desiredtz = "", verbose = TRUE)
}
\arguments{
  \item{outputdir}{Directory where you would like to store your results}
  \item{desiredtz}{Character, specifying the timezone database name 
    of the timezone the data was collected in}
  \item{verbose}{
    See details in \link{GGIR}.
  }
}
\value{
  No object is returned, only files are created in the output directory
}

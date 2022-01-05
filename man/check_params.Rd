\name{check_params}
\alias{check_params}
\title{
  Check default parameters
}
\description{
  Checks paramter objects for class and logical combinations.
  Not intended for direct use by GGIR users.
}
\usage{
  check_params(params_sleep = c(), params_metrics = c(), params_rawdata = c())
}
\arguments{
  \item{params_sleep}{
    List with sleep parameters
  }
  \item{params_metrics}{
    List with parameters related to metrics
  }
  \item{params_rawdata}{
    List with parameters related to raw data reading and processing
  }
}
\value{
  Lists of updated parameter objects 
}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
}

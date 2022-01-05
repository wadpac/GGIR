\name{extract_params}
\alias{extract_params}
\title{
  Extract parameters from input and add them to params
}
\description{
  Extracts parameters separately provided by input and adds them to the params objects.
  Not intended for direct use by GGIR users.
}
\usage{
  extract_params(params_sleep = c(), params_metrics = c(), 
  params_rawdata = c(), input = c())
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
  \item{input}{
    All objects provided by users
  }
}
\value{
  Lists of updated parameter objects 
}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
}

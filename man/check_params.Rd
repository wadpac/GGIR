\name{check_params}
\alias{check_params}
\title{
  Check default parameters
}
\description{
  Checks parameter objects for class and logical combinations.
  Called from \link{extract_params}. Not intended for direct use by GGIR users.
}
\usage{
  check_params(params_sleep = c(), params_metrics = c(),
                        params_rawdata = c(), params_247 = c(),
                        params_phyact = c(), params_cleaning = c(),
                         params_output = c(), params_general = c())
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
  \item{params_247}{
    List with parameters related to 24/7 behavioural analysis, which includes anything
    that does not fit with physical activity or sleep research
  }
  \item{params_phyact}{
    List with parameters related to physical activity analysis
  }
  \item{params_cleaning}{
    List with parameters related to cleaning the time series, including masking and imputation
  }
  \item{params_output}{
    List with parameters related to how GGIR stores its output
  }
  \item{params_general}{
    List with parameters related to general topics
  }
}
\value{
  Lists of updated parameter objects 
}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
}

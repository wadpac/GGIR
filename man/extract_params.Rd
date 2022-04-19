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
                 params_rawdata = c(), params_247 = c(),
                 params_phyact = c(), params_cleaning = c(),
                 params_output = c(), params_general = c(), input = c(),
                 configfile_csv = c(), params2check = c("sleep", "metrics",
                 "rawdata", "247", "phyact",
                                           "cleaning", "output", "general"))
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
  \item{input}{
    All objects provided by users
  }
  \item{configfile_csv}{
    Csv configuration file
  }
  \item{params2check}{
    Character vector to indicate which params objects need to be checked.
    This allows us to prevent the function from checking params objects that are not used
    in the context where function extract_params is used.
  }
}
\value{
  Lists of updated parameter objects 
}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
}

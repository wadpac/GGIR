\name{load_params}
\alias{load_params}
\title{
  Load default parameters
}
\description{
  Loads default paramter values
  Not intended for direct use by GGIR users.
}
\usage{
  load_params(group = c("sleep", "metrics", "rawdata"))
}
\arguments{
  \item{group}{
    Character vector with parameter groups to load, options: "sleep", "metrics", "rawdata"
  }
}
\value{
  Lists of parameter objects 
}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
}

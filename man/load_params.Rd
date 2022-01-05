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
  load_params(group = c("sleep", "metrics", "rawdata", "247",
                        "phyact", "cleaning", "output", "general"))
}
\arguments{
  \item{group}{
    Character vector with parameter groups to be loaded.
  }
}
\value{
  Lists of parameter objects 
}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
}

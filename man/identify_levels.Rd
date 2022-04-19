\name{identify_levels}
\alias{identify_levels}
\title{
  Identifies levels of behaviour for g.part5 function.
}
\description{
  Identifies levels of behaviour from acceleratioon
  and sustained inactivity sibdetection (using angles). Function not
  intended for direct use by package user.
}
\usage{
  identify_levels(ts, TRLi,TRMi,TRVi,
                  ws3, params_phyact, ...)
}
\arguments{
  \item{ts}{
    Data.frame with time series genrated in .gpart5
  }
  \item{TRLi}{
    Numeric acceleration threshold light
  }
  \item{TRMi}{
    Numeric acceleration threshold moderate
  }
  \item{TRVi}{
    Numeric acceleration threshold vigorous
  }
  \item{ws3}{
    Numeric size of epoch in seconds
  }
  \item{params_phyact}{
    See \link{g.part2}
  }
  \item{...}{
     Any argument used in the previous version of identify_level, which will now
     be used to overrule the arguments specified with the parameter objects.
  }
}
\value{
  List with items:
  item{LEVELS}{}
  item{OLEVELS}{}
  item{Lnames}{}
  item{bc.mvpa}{}
  item{bc.lig}{}
  item{bc.in}{}
  item{ts}{}
}
\examples{
  \dontrun{
    levels = identify_levels(TRLi,TRMi,TRVi,
                               boutdur.mvpa,boutcriter.mvpa,
                               boutdur.lig,boutcriter.lig,
                               boutdur.in,boutcriter.in,
                               ws3,bout.metric)
  }
}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
}

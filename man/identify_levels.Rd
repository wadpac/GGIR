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
                 boutdur.mvpa,boutcriter.mvpa,
                 boutdur.lig,boutcriter.lig,
                 boutdur.in,boutcriter.in,
                 ws3,bout.metric)
}
\arguments{
  \item{ts}{ Data.frame with time series genrated in .gpart5
}
\item{TRLi}{
}
\item{TRMi}{
}
\item{TRVi}{
}
\item{boutdur.mvpa}{
}
\item{boutcriter.mvpa}{
}
\item{boutdur.lig}{
}
\item{boutcriter.lig}{
}
\item{boutdur.in}{
}
\item{boutcriter.in}{
}
\item{ws3,bout.metric}{
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

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
  Durations of mvpa bouts in minutes to be extracted. The default values is
  c(1,5,10) and will start with the identification of 10 minute bouts, followed by
  5 minute bouts in the rest of the data, and followed by
  1 minute bouts in the rest of the data.
}
\item{boutcriter.mvpa}{
  A number between 0 and 1 and defines what fraction of a bout needs to be above
  the mvpathreshold
}
\item{boutdur.lig}{
  Durations of light activty bouts in minutes to be extracted. Light activity
  bouts are detected in the segments of the data which were not labelled as sleep,
  MVPA, or inactivity bouts. The default duration values is c(1,5,10), this will
  start with the identification of 10 minute bouts, followed by 5 minute bouts
  in the rest of the data, and followed by 1 minute bouts in the rest of the data.
}
\item{boutcriter.lig}{
  A number between 0 and 1 and defines what fraction of a bout needs to be between
  the light and moderage threshold
}
\item{boutdur.in}{
  Durations of inactivty bouts in minutes to be extracted. Inactivity bouts are
  detected in the segments of the data which were not labelled as sleep or MVPA
  bouts. The default duration values is c(10,20,30), this will start with the
  identification of 30 minute bouts, followed by 20 minute bouts in the rest of
  the data, and followed by 10 minute bouts in the rest of the data.
}
\item{boutcriter.in}{
  A number between 0 and 1 and defines what fraction of a bout needs to be below
  the light threshold
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

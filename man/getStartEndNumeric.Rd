\name{getStartEndNumeric}
\alias{getStartEndNumeric}
\title{
Generate start and end page of a day
}
\description{
Generate start and end page of a day when working with argument selectdaysfile
in \link{g.part1}. The user provides a date and a start hour
which is used to generate the pages of the start hour minutes 5 minutes and the
start hour plus 24 hours. Function not designed for direct use by package user.
}
\usage{
  getStartEndNumeric(d, hhr, startHour = 4)
}
\arguments{
  \item{d}{
    Character with date (without time) format
  }
  \item{hhr}{
    GENEActiv::header.info(f) output
  }
  \item{startHour}{
    Hour that analysis starts at
  }
}
\value{
  Data.frame with two columns: a start page five minutes before startHour
  on day d and an end page 24 hours after startHour
}

\examples{
\dontrun{
hhr = GENEActiv::header.info("C:/myfile.bin")
mystartandendpage = getStartEndNumeric(d="20/5/2017", hhr, startHour = 4)
}
}
\author{
  Joe Heywood <j.heywood@ucl.ac.uk>
}
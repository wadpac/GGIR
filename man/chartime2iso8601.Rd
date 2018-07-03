\name{chartime2iso8601}
\alias{chartime2iso8601}
\title{
Convert character timestamps to iso8601 timestamp
}
\description{
To avoid ambiguities when sharing and comparing timestamps. All timestamps
are expressed in iso8601 format: https://en.wikipedia.org/wiki/ISO_8601
}
\usage{
chartime2iso8601(x,tz)	
}
\arguments{
  \item{x}{
Vector of timestamps in character format:
year-month-date and optional followed by hour:minute:second 
For example, "1980-01-01 18:00:00"
}
  \item{tz}{
 Timezone of data collection, e.g. "Europe/London".
 See https://en.wikipedia.org/wiki/List_of_tz_database_time_zones
 for full list
}
}
\examples{
x ="1980-1-1 18:00:00"
tz = "Europe/Amsterdam"
x_converted = chartime2iso8601(x,tz)
}

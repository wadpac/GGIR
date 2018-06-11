\name{parse.time}
\alias{parse.time}
\title{
Parses a character time representation to another format. 
}
\description{
Function copied from depricated package GENEAread.
Converts a character vector in a variety of forms into either the raw second,
second classed as POSIXct, or days since Unix epoch.
}
\usage{
parse.time(t="",format=c("seconds", "days", "POSIX"), tzone = 0, 
	start = NULL, startmidnight = NULL)
}
\arguments{
  \item{t}{A character string representation of a date-time expression.}
  \item{format}{A character string indicating which representation to
  output.  Can be either \code{seconds}, \code{days} or \code{POSIX}.}
  \item{tzone}{The time zone the time is given in, expressed as an 
  offset from UTC in hours.}
  \item{start}{Earliest allowable time stamp in the data, as seconds
  since Unix epoch.}
  \item{startmidnight}{Midnight of day '0' in the data, as seconds
  since Unix epoch.}
}
\details{
The function processes character vectors of the form "DATE TIME" -- that 
is to say, a maximum of two terms separated by a space per value. 

"TIME" is given in 24 hour format, seperated by colons, in "hh:mm", 
"hh:mm:ss", "hh:mm:ss:ms" or "hh:mm:ss.ms" format. If ommitted, the 
time is taken to be 00:00:00.000.

"DATE" can be a date representation as "YYYY-MM-DD", "DD/MM/YY" or "DD/MM/YYYY" 
(noting the use of a colon or backslash seperator to distinguish between the 
two). Alternatively, with \code{start} and/or \code{startmidnight} supplied,
an integer "NN" or string "DOW" corresponding to a day of the week can be
used instead. Then, the function will find the first timestamp matching the
correct "TIME", that falls NN midnights after \code{startmidnight} and is 
after \code{start}, or, in the latter case, the first timestamp after the 
day of \code{start} that matches the appropriate day of the week. If a blank
"DATE" is supplied, the function will either use the UNIX epoch, or find the
first match, corresponding to the case NN = 0.
 
Once this is done the time is converted to the required format: \code{POSIX} 
is the usual R POSIXct format; \code{days} is the julian days since UNIX 
epoch 1970-1-1; \code{seconds} is the number of seconds (including subseconds) 
since 1970-1-1. Note that for formats other than POSIX, the output is in the 
same timezone as \code{tzone}. POSIX stores the time internally as the time in 
UTC, and applies a format that gives this time local to the user.
}

\value{
A converted date-time string in the specified format. In the case of "seconds", 
or "days", a numeric. For POSIX, a \code{\link{POSIXct}} object.
}
\seealso{
\code{\link{convert.time}}, \code{\link{get.intervals}}
}
\examples{

t1 = parse.time("2012-06-21 13:04:01"); print(t1)
parse.time("21/06/12 13:04:01") #gives the same result

parse.time(c("19/07/70", "20/07/70"), format = "days")
#results here will depend on your locale
parse.time(c("19/07/70", "20/07/70"), format = "POSIX", tzone = -4)

#one is the same day, one can only find a match the next day
parse.time("13:05", start = t1) - t1
parse.time("13:00", start = t1) - t1
#asking to wait 1 midnight means both times are considered as 
#times on the same, full day of data
parse.time(c("1 13:05", "1 13:00"), start = t1) - t1
#2012-06-21 is a Thursday, so this is equivalent
parse.time(c("Fri 13:05", "Fri 13:00"), start = t1) - t1
#Longer form days of the week are also understood. Note that 
#the first day does not get matched.
parse.time(c("Thursday 13:05", "Thursday 13:00"), start = t1) - t1

}
\keyword{utilities}

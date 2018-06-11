\name{GRtime}
\alias{GRtime}
\alias{convert.time}
\alias{as.GRtime}
\alias{format.GRtime}
\alias{axis.GRtime}
\alias{pretty.GRtime}
\title{
Date time handling for the GENEAread package.
}
\description{
Function copied from depricated package GENEAread.
Stores date time data as a numeric, with facility for pretty printing
and axis commands.
}
\usage{
convert.time(x, format = NULL)
as.GRtime(x, format = NULL, \dots)
\method{format}{GRtime}(x, format = NULL, \dots)
\method{axis}{GRtime}(side, x=NULL, at=NULL, format = NULL,
labels  = TRUE, add = TRUE,  \dots)
\method{pretty}{GRtime}(x, n = 5, ...)
}
\arguments{
  \item{x}{Object to process. For \code{convert.time}, must be numeric.
  For \code{as.GRtime} may be numeric or character. 
  For \code{format.GRtime}, a GRtime object, or a numeric.}
  \item{format}{A character string indicating the form of output.
  See \code{\link{strptime}} for details. If NULL, will be automatically chosen.}
  \item{add}{logical. If TRUE, actually plot the axis.}
  \item{at, side, labels}{Additional arguments as in \code{\link{axis}}.}
  \item{n}{Approximate number of breakpoints.}
  \item{\dots}{Additional arguments to be passed to \code{\link{parse.time}}, 
  \code{\link{as.numeric}}, \code{\link{format.POSIXct}}, \code{\link{axis}},
  \code{\link{pretty.POSIXt}}. }
}
\details{
The GRtime class handles dates and times for the GENEAread class. The class treats
dates as numerics denoting seconds since the UNIX epoch, with potentially a string 
attached specifying the format to print in. Unlike \code{POSIXct}, we avoid some of
the processing, especially with respect to time zones, and allow some more
flexibility in time computation and display. A range of operators are defined.

\code{convert.time} converts numerics to GRtime objects. The \code{format} argument
allows a format string to be attached specifying the default format to display in.
\code{as.GRtime} is a wrapper to \code{convert.time}, that when supplied with
character input, coerces the value first to numeric using \code{parse.time}.

\code{format.GRtime} formats GRtime objects for pretty printing. If \code{format}
is provided as argument, that is used. Else, if the \code{format} attribute is
set on \code{x}, that is used. Finally, if formats are not provided, and \code{x} 
is of length greater than one, the range of values of \code{x} is used to decide 
the units displayed. Numerics are also accepted - they are coerced to GRtime.

\code{axis.GRtime} is used to plot GRtime axis, choosing, by default, breakpoints
that give 'pretty' sub intervals. Note that \code{\link{plot.default}} uses
\code{axis.GRtime} by default if supplied with a GRtime object in one of the
directions. However, \code{\link{image.default}} based functions do not use 
the class axis functions, so axes must be plotted manually.

\code{pretty.GRtime} computes 'pretty' breakpoints, using the algorithm of 
\code{pretty.POSIXt}. Attributes are preserved.

}

\value{
For \code{convert.time}, \code{as.GRtime} and \code{pretty.GRtime}, a
GRtime object.

For \code{format.GRtime} a character string representation.

For \code{axis.GRtime} a list containing positions and labels for axis markers.
}
\seealso{
\code{\link{parse.time}}, \code{\link{get.intervals}}, \code{\link{print.AccData}}
}
\examples{
as.GRtime("00:01")
#format is automatically set
convert.time(1:10)
convert.time(1:10*1000)
#we add a different default format
convert.time(1:10*1000, "\%H:\%M:\%OS3") -> t
t
str(t)
#we override format with our own
format(t, format = "\%a \%d/\%m/\%y \%H:\%M:\%OS3")

#plot calls axis.GRtime automatically. Notice
#that the format attribute is used.
plot(t, 1:10)
#strip out the default format
t2 = convert.time(t, format = NULL)
plot(t2, 1:10)

#image plots are a bit more complex

Z = matrix(rnorm(100), 10)
image(x = t, y = t2, z = Z, axes = FALSE)
axis.GRtime(x = t2, side = 2)
Axis(x = t, side = 1) #Axis also works
box() #complete the bounding box

#custom axes
plot(t2, 1:10, xaxt = "n")
axis.GRtime(at = pretty(t2, 20) , side = 1)
}
\keyword{methods}

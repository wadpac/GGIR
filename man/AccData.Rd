\name{AccData}
\alias{AccData}
\alias{[.AccData}
\alias{$.AccData}
\alias{print.AccData}
\alias{plot.AccData}
\alias{summary.AccData}
\title{
Methods for processing and summarising AccData.
}
\description{
Function copied from depricated package GENEAread.
A variety of functions and methods for handling processed AccData.
}
\usage{
\method{[}{AccData}(x, i=1:dim(x$data.out)[1], j=NULL, drop=TRUE)
\method{$}{AccData}(x, name)
\method{print}{AccData}(x, \dots)
\method{summary}{AccData}(object, \dots)
\method{plot}{AccData}(x, y=NULL, what = c("sd", "mean", "temperature", "light", 
"voltage"),draw = TRUE, resolution = 200,\dots)
}
\arguments{
  \item{x, object}{"AccData" object to process.}
  \item{i,j}{Coordinates for matrix like manipulation.}
  \item{drop}{logical. Coerce to vector if one dimensional?}
  \item{name}{list field to extract.}
  \item{y}{Optional variable to plot as y-axis.}
  \item{what}{Type of plot to create.}
  \item{draw}{logical. Whether to plot output.}
  \item{resolution}{Approximate number of time steps to plot.}
  \item{\dots}{Additional arguments to pass to default methods.}
}
\details{
These functions allow access and manipulation of AccData class objects.

\code{[} allows matrix style manipulations. If the first column (the timestamp 
column) is included, a data.frame is produced with the timestamp as a "GRtime" 
object via \code{\link{convert.time}}. This allows improved plotting of time
axes. If j is not specified, an "AccData" object is returned.

\code{$} allow list style manipulations. In addition to the internal components 
of "AccData" objects (see \code{\link{read.bin}}), a number of keywords are recognised:

"time": Timestamp
"x","y","z": x, y and z accelerometer components
"xyz": The three accelerometer components together
"temperature"
"button"
"voltage"
"light"
"svm": Sum of vector magnitudes

\code{print} and \code{summary} both provide useful summaries of the data. Summary 
returns invisibly an object representation of its output in list format - 
in particular, it gives summary statistics of epochal standard deviations
on a 10 second epoch.

Finally, plot provides a range of useful summary plots, depending on the 
specification of \code{what}. To reduce computational requirements,
epochs and so on are chosen or downsampling done so that a maximum of 
around 100 time points are plotted. If \code{plot} is called with a
specified \code{y}, \code{x} is considered as its vector of timestamps.
If \code{draw = FALSE}, plot produces no side effects but instead returns
the object that would have been plotted.
}
\seealso{
\code{\link{header.info}}, \code{\link{epoch.apply}}, \code{\link{get.intervals}}
}
\examples{

binfile  = system.file("binfile/TESTfile.bin", package = "GGIR")[1]

#Read in the entire file, calibrated
procfile<-read.bin(binfile)

print(procfile)
summary(procfile)

plot(procfile$temperature)
plot(procfile[,c(1,7)])

}
\keyword{methods}

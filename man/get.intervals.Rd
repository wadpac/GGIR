\name{get.intervals}
\alias{get.intervals}
\alias{print.VirtAccData}
\alias{VirtAccData}
\title{
Extract an interval of data.
}
\description{
Function copied from depricated package GENEAread.
Function for extracting sub intervals of data, and implementation of
just-in-time loading.
}
\usage{
get.intervals(x, start=0, end = 1, length = NULL, time.format = c("auto",
"seconds", "days",
"proportion", "measurements", "time"), incl.date = FALSE, simplify = TRUE ,
read.from.file=FALSE, size=Inf, \dots)
}
\arguments{
  \item{x}{Object to process. Can be array, }
  \item{start}{Start of interval.}
  \item{end}{End of interval.}
  \item{length}{Length of interval.}
  \item{time.format}{Method with which \code{start} and \code{end}
  should be understood.}
  \item{incl.date}{logical. Include a column denoting time?}
  \item{simplify}{logical. If TRUE, output an array. Otherwise 
  output a AccData object.}
 \item{read.from.file}{logical. If TRUE, re-read the relevant time 
 interval from the 
 original bin file.}
  \item{size}{Desired number of samples in output.}
  \item{\dots}{Additional arguments to be passed to \code{\link{read.bin}}, if
  \code{read.from.file} is TRUE.}
}
\details{
The function extracts the desired analysis time window specified
by \code{start} and \code{end}.
If length is specified, then the end is set to a point \code{length}
units after start. The 
times are interpreted in terms of \code{time.format}. For 
convenience, a variety of time window
formats are accepted:

"seconds": Seconds since start of dataset.

"days": Days since start of dataset.

"proportion": Proportional point within dataset, given as a 
numeric between 0 and 1.

"measurements": Raw number of samples since start of dataset.

"time": Time string, as understood via \code{\link{parse.time}}.

"auto": Default - attempt to determine time format from size and type of \code{start}.

Some capacity for using mixed types of inputs for \code{start} and \code{length} in
particular is present.

The input object \code{x} is typically an "AccData" object, though arrays are also 
accepted. "VirtAccData" are dealt with by using the timestamp and call information
recorded within them to do a new read of the original bin file, assuming this is 
still available. This is useful for 'just in time' reads of data. "AccData" can be 
dealt with in this way by setting \code{read.from.file}.

Note that for \code{read.from.file}, only "time" and "proportion"
\code{time.format} are presently supported.

}
\value{
With \code{simplify = FALSE}, an "AccData" S3 object with the desired records.

Otherwise, an array containing either 3 or 4 columns, containing the x, y, z 
acceleration vectors and optionally a time vector.
}
\seealso{
\code{\link{read.bin}}, \code{\link{[.AccData}}, \code{\link{get.intervals}}
}
\examples{

binfile  = system.file("binfile/TESTfile.bin", package = "GGIR")[1]

#Read in a highly downsampled version of the file
procfile<-read.bin(binfile, downsample = 100)
print(procfile)
#Plot the x component
plot(procfile[,1:2], type = "l")

#Overlay some segments in different colour
lines(get.intervals(procfile, start = 0.4, end = 0.5, 
time.format = "prop", incl.date = TRUE)[,1:2], col=2) 
lines(get.intervals(procfile, start = 0.4, end = 5,
time.format = "sec", incl.date = TRUE)[,1:2], col=3) 
lines(get.intervals(procfile, start = "16:51", end = "16:52",
time.format = "time", incl.date = TRUE)[,1:2], col=4) 
#Note that measurements will depend on the downsampling rate,
# not the original sampling rate of the data
lines(get.intervals(procfile, start = 100, length = 10, 
time.format = "measurement", incl.date = TRUE)[,1:2], col=5) 
#This is also understood
lines(get.intervals(procfile, start = "16:52:10", 30, 
incl.date = TRUE)[,1:2], col=6) 

#Now load in virtually
virtfile<-read.bin(binfile, virtual = TRUE)
#Notice that get.intervals with simplify = FALSE gives
# a genuine AccData object
realfile = get.intervals(virtfile, start = 0.5, end = 1, 
simplify = FALSE)
virtfile
realfile
#get.intervals calls read.bin automatically
points(get.intervals(virtfile, start = "16:52:10", "16:52:40",
incl.date = TRUE)[,1:2], col=4, pch = ".") 

#Alternatively, re-read procfile at a different resampling rate.
lines(get.intervals(procfile, start = "16:49:00", "16:49:30",  
incl.date = TRUE, read.from.file = TRUE, downsample = 300)[,1:2], col=2) 


}
\keyword{manip}

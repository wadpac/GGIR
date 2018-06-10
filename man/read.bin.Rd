\name{read.bin}
\alias{read.bin}
\title{
File processing function for binary files.
}
\description{
Function copied from depricated package GENEAread.
A function to process binary accelerometer files and convert the 
information into R objects.
}
\usage{
read.bin(binfile, outfile = NULL, start = NULL, end = NULL, 
    verbose = TRUE, do.temp = TRUE,do.volt = TRUE, calibrate = TRUE, 
    downsample = NULL, blocksize , virtual = FALSE,
    mmap.load = (.Machine$sizeof.pointer >= 8), pagerefs = TRUE, ...)
}
\arguments{
  \item{binfile}{
A filename of a file to process.
}
  \item{outfile}{
An optional filename specifying where to save the processed data object.
}
  \item{start}{Either:
A representation of when in the file to begin processing, see Details.
}
  \item{end}{Either:
A representation of when in the file to end processing, see Details.
}
  \item{verbose}{
	A boolean variable indicating whether some information should be printed
	during processing should be printed.
}
  \item{do.temp}{
	A boolean variable indicating whether the temperature signal should
	be extracted.
}
 \item{do.volt}{
	A boolean variable indicating whether the voltage signal should
	be extracted.
}
  \item{calibrate}{
	A boolean variable indicating whether the raw accelerometer
	values and the light variable should be calibrated according to
	the calibration data in the headers.
}
\item{downsample}{
        A variable indicating the type of downsampling to apply to 
        the data as it is loaded. Can take values:\cr\cr
	\code{NULL}: (Default) No downsampling\cr
	Single numeric: Reads every \code{downsample}-th value, starting 
	from the first.\cr
	Length two numeric vector: Reads every \code{downsample[1]}-th value,
	starting from the \code{downsample[2]}-th.\cr\cr

Non-integer, or non-divisor of 300 downsampling factors are allowed, 
but will lead to imprecise frequency calculations, leap seconds being 
introduced, and generally potential problems with other methods.
Use with care.
}

\item{blocksize}{
	Integer value giving maximum number of data pages to read in each 
	pass. Defaults to 10000 for larger data files. Sufficiently small
	sizes will split very large data files to read chunk by chunk, 
	reducing memory requirements for the read.bin function (without 
	affecting the final object), but conversely possibly increasing 
	processing time. Can be set to Inf for no splitting.
}

\item{virtual}{
	logical. If set TRUE, do not do any actual data reading. Instead
	construct a VirtualAccData object containing header information 
	to allow use with \code{\link{get.intervals}}.
}

\item{mmap.load}{
	logical. If TRUE (Default on 64bit R), use the \code{\link[mmap]{mmap}} 
	package to process the binfile.
}

\item{pagerefs}{
	A variable that can take two forms, and is considered only for 
	\code{mmap.load = TRUE}\cr\cr
	NULL or FALSE, in which case pagerefs are dynamically calculated 
	for each record. (Default) \cr
	A vector giving sorted byte offsets for each record for mmap 
	reading of data files.\cr
	TRUE, in which case a full page reference table is computed 
	before any processing occurs.\cr\cr

Computing pagerefs takes a little time and so is a little slower.
However, it is safer than dynamic computations in the case of 
missing pages and high temperature variations. Further, once page
references are calculated, future reads are much faster, so long
as the previously computed references are supplied.
}


\item{...}{Any other optional arguments can be supplied that affect
manual calibration and data processing.  These are: \cr\cr

  \code{gain}: a vector of 3 values for manual gain calibration of 
  the raw (x,y,z) axes.  If \code{gain=NULL}, the gain calibration 
  values are taken from within the output file itself.\cr

  \code{offset}: a vector of 3 value for manual offset calibration 
  of the raw (x,y,z) axes.  If \code{offset=NULL}, the offset 
  calibration values are taken from within the output file itself.\cr

  \code{luxv}: a value for manual lux calibration of the light meter.
  If \code{luxv=NULL}, the lux calibration value is taken from within
  the output file itself.\cr

  \code{voltv}: a value for manual volts calibration of the light meter. 
  If \code{voltv=NULL}, the volts calibration value is taken from within 
  the output file itself.\cr

  \code{warn}: if set to true, give a warning if input file is large, 
  and require user confirmation.
}

}
\details{
The read.bin package reads in binary files compatible with the GeneActiv 
line of Accelerometers, for further processing by the other functions in
this package. Most of the default options are those required in the most
common cases, though users are advised to consider setting start and end
to smaller intervals and/or choosing some level of downsampling when 
working with data files of longer than 24 hours in length.

The function reads in the desired analysis time window specified by 
\code{start} and \code{end}. For convenience, a variety of time window formats are accepted:

Large integers are read as page numbers in the dataset. Page numbers 
larger than that which is available in the file itself are constrained
to what is available. Note that the first page is page 1.

Small values (between 0 and 1) are taken as proportions of the data. 
For example, `start = 0.5` would specify that reading should begin at 
the midpoint of the data.

Strings are interpreted as dates and times using \code{\link{parse.time}}.
In particular, times specified as "HH:MM" or "HH:MM:SS" are taken as the 
earliest time interval containing these times in the file. Strings with an 
integer prepended, using a space seperator, as interpreted as that time after
the appropriate number of midnights have passed - in other words, the
appropriate time of day on the Nth *full* day. Days of the week and dates 
in "day/month", "day/month/year", "month-day", "year-month-day" are also
handled. Note that the time is interpreted in the same time zone as the
data recording itself.

Actual data reading proceeds by two methods, depending on whether
\code{mmap} is true or false. With \code{mmap = FALSE}, data is
read in line by line using \code{readLine} until blocksize is filled, 
and then processed. With \code{mmap = TRUE}, the \code{\link[mmap]{mmap}}
package is used to map the entire data file into an address file,
byte locations are calculated (depending on the setting of \code{pagerefs}),
\code{blocksize} chunks of data are loaded, and then processed as raw vectors. 

There are advantages and disadvantages to both methods: the mmap method 
is usually much faster, especially when we are only loading the final 
parts of the data. ReadLine will have to process the entire file in 
such a case. On the other hand, mmap requires a large amount of memory 
address space, and so can fail in 32 bit systems. Finally, reading of 
compressed bin files can only be done with the readLine method. 
Generally, if mmap reading fails, the function will attempt to catch 
the failure, and reprocess the file with the readLine method, giving 
a warning.

Once data is loaded, calibration is then either performed using values 
from the binary file, or using manually inputted values (using the
\code{gain}, \code{offset},\code{luxv} and \code{voltv} arguments).

}
\value{
With \code{virtual = FALSE}, an "AccData" S3 object with 9 components:
\item{data.out}{A 6 or 7 column matrix of the processed pages, the rows 
of which are the processed observations in order of processed pages.  
The matrix has columns (timestamp,x-axis,y-axis,z-axis,light,button) or
(timestamp,x-axis,y-axis,z-axis,light,button,temperature) if 
\code{do.temp=TRUE}. The timestamp is stored as seconds since 1 Jan 1970,
in the timezone that the data is recorded in.}
\item{page.timestamps}{The timestamps as POSIXct representations 
(as opposed to those within the \code{data.out} array.)}
\item{freq}{The effective sampling frequency (in Hz).}
\item{filename}{The file name of the bin file.}
\item{page.numbers}{The pages that were loaded.}
\item{call}{The function call that the object was created with.}
\item{page.volts}{The battery voltage associated with each loaded page,
if \code{do.volt} is TRUE.}
\item{pagerefs}{The page byte offsets that were computed.}
\item{header}{File header output, as given by \code{\link{header.info}}.}

Various processing methods are implemented so that \code{AccData} objects 
can be treated as an ordinary matrix in many cases. See 
\code{\link{print.AccData}} for info.

With \code{virtual = TRUE}, a "VirtAccData" S3 object with page.timestamps,
freq, filename, page.numbers, call, pagerefs, header as in the earlier case, 
but also,
\item{data.out}{A vector containing the timestamps of each page, using local 
seconds since 1970.}
\item{nobs}{Number of observations per page, after downsampling.}
}
\section{Warning}{
Reading in an entire .bin file will take a long time if the file contains a 
lot of datasets. Reading in such files without downsampling can use up all 
available memory. See \code{\link{memory.limit}}.

This function is specific to header structure in GENEActiv output files.
By design, it should be compatible with all firmware and software versions
to date (as of version of current release). If order or field names are
changed in future .bin files, this function may have to be updated
appropriately.
}
\seealso{
\code{\link{header.info}}, \code{\link{print.AccData}},
\code{\link{get.intervals}}
}
\examples{

binfile  = system.file("binfile/TESTfile.bin", package = "GGIR")[1]

#Read in the entire file, calibrated
procfile<-read.bin(binfile)
print(procfile)
procfile$data.out[1:5,]

#Uncalibrated, mmap off
procfile2<-read.bin(binfile, calibrate = FALSE)
procfile2$data.out[1:5,]

#Read in again, reusing already computed mmap pagerefs
procfile3<-read.bin(binfile, pagerefs = procfile2$pagerefs )

#Downsample by a factor of 10
procfilelo<-read.bin(binfile, downsample = 10)
print(procfilelo)
object.size(procfilelo) / object.size(procfile)

#Read in a 1 minute interval
procfileshort <- read.bin(binfile, start = "16:50", end = "16:51")
print(procfileshort)

##NOT RUN: Read, and save as a R workspace
#read.bin(binfile, outfile="tmp.Rdata")
#print(load("tmp.Rdata"))
#print(processedfile)


}
\keyword{IO}

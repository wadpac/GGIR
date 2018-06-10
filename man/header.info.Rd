\name{header.info}
\alias{header.info}
\title{
Get header info from GENEA output (.bin) file
}
\description{
Function copied from depricated package GENEAread.
Function to extract relevant header fields and values from a file.
}
\usage{
header.info(binfile, more=TRUE)
}
\arguments{
  \item{binfile}{The file from which to extract the header}
  \item{more}{logical. If TRUE, extract additional data from file
  useful for calibration and data reading.}
}
\details{
	The function extracts useful information from a .bin file, such 
	as information about the GENEA device used to produce the output,
	and characteristics of the subject who wore the device. 
	The function also accepts data that has been compressed in `gzip',
	`bzip2' or `xz' formats. See \code{file}.
	With \code{more} set to TRUE, additional data is extracted, 
	mainly for internal use in \code{read.bin}.
}
\value{
A \code{data.frame} with extracted header information, each row a 
particular header field with its value. 
If \code{more} is TRUE, an attribute "calibration" is attached to 
the object, consisting of a list with measurement offsets, sampling
frequency estimates, start times and time zones, data position 
offsets, and if mmap is detected, byte locations and increments for
mmap reading.
}

\section{Warning}{
This function is specific to header structure in GENEActiv output 
files. By design, it should be compatible with all firmware and 
software versions to date (as of version of current release). If 
order or field names are changed in future .bin files, this function
may have to be updated appropriately.
The function works by looking for appropriate section headings in
the .bin files.
} 
\seealso{
\code{\link{read.bin}}
}
\examples{

fileheader <- header.info(system.file("binfile/TESTfile.bin", 
package = "GGIR")[1], more = TRUE)
print(fileheader)
attr(fileheader, "calibration")
}
\keyword{IO}

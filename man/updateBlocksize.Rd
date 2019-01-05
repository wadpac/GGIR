\name{updateBlocksize}
\alias{updateBlocksize}
\title{
Update blocksize of data to be read depending on available memory.
}
\description{
Function queries available memory to either lower or increase the blocksize
used by function \link{g.readaccfile}
}
\usage{
updateBlocksize(blocksize, bsc_qc)
}
\arguments{
  \item{blocksize}{
Number of filepages (binary data) or rows (other dataformats).
}
\item{bsc_qc}{
  Data.frame with columns time (timestamp from Sys.time) and size (memory size).
  This is used for housekeeping in \link{g.calibrate} and \link{g.getmeta}
}
}
\value{
List with blocksize and bsc_qc, same format as input, although bsc_qc has one new
row.
}
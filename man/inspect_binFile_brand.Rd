\name{inspect_binFile_brand}
\alias{inspect_binFile_brand}
\title{Identify the Device Brand from a Binary File}
\description{
  This function identifies the brand of a device (e.g., GENEActiv or MATRIX) based on the contents 
  of a binary file. If the file is unrecognized, it returns a default value indicating the device is not recognized.
}
\usage{
inspect_binFile_brand(filename)
}
\arguments{
  \item{filename}{
    The path to the binary file to be inspected.
  }
}
\details{
  The function performs the following steps:
  \enumerate{
    \item Reads the file header to check for the presence of a \code{"Device Type"} field.
    \item If the device type contains the string \code{"GENEActiv"}, the device is identified as a GENEActiv device.
    \item If the \code{"Device Type"} field is not found, it checks for a MATRIX device by validating a specific header value (\code{"MDTC"}).
    \item If neither device type is detected, the function returns \code{"not_recognised"}.
  }
}
\value{
  A character string representing the detected device brand:
  \itemize{
    \item \code{2}: Indicates the device is a GENEActiv device.
    \item \code{7}: Indicates the device is a MATRIX device.
    \item \code{"not_recognised"}: Indicates the file's brand is not recognized.
  }
}
\author{
  Jairo H Migueles <jairo@jhmigueles.com>
}

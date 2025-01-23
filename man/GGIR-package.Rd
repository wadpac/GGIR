\name{GGIR-package}
\alias{GGIR-package}
\docType{package}
\title{
  A package to process multi-day raw accelerometer data
}
\description{
  Disclaimer: If you are a new GGIR user then please see
  \href{https://wadpac.github.io/GGIR/}{the GGIR github-pages}
   for a narrative overview of GGIR.\cr
  \cr
  This document is primarily aimed at documenting the functions and their input arguments.\cr
  \cr
  Please note that there is google discussion group for this package (link below).\cr
  \cr
  You can thank us for sharing the code in this package and for developing
  it as a generic purpose tool by citing the package name and by
  citing the supporting publications (e.g. Migueles et al. 2019) in your publications.
}
\details{
  \tabular{ll}{
  Package: \tab GGIR\cr
  Type: \tab Package\cr
  Version: \tab 3.1-11\cr
  Date: \tab 2025-01-23\cr
  License: \tab Apache License (== 2.0)\cr
  Discussion group: \tab https://groups.google.com/forum/#!forum/rpackageggir\cr
  }
}
\examples{
  \dontrun{
    #inspect file:
    I = g.inspectfile(datafile)

    #autocalibration:
    C = g.calibrate(datafile)

    #get meta-data:
    M = g.getmeta(datafile)
  }
  data(data.getmeta)
  data(data.inspectfile)
  data(data.calibrate)

  #impute meta-data:
  IMP = g.impute(M = data.getmeta, I = data.inspectfile)
  #analyse and produce summary:
  A = g.analyse(I = data.inspectfile, C = data.calibrate, M = data.getmeta, IMP, ID = "01wk0")
  #plot data
  g.plot(IMP, M = data.getmeta, I = data.inspectfile, durplot=4)
}
\author{
  \itemize{
    \item Vincent T van Hees <v.vanhees@accelting.com> main creator and developer
    \item Zhou Fang developed calibration algorithm used in function \link{g.calibrate}
    \item Joe Heywood helped develop the functionality to process specific recording days
    \item Severine Sabia, Mathilde Chen, and Manasa Yerramalla extensively tested and provided feedback on various functions
    \item Joan Capdevila Pujol helped to improve various functions
    \item Jairo H Migueles <jairohm@ugr.es> helped to improve various functions
    \item Matthew R Patterson helped with enhancing the visual report.
    \item Lena Kushleyeva helped fix bug in sleep detection.
    \item Taren Sanders helped tidy up the parallel processing functionality
  }
}
\references{
  \itemize{
    \item Migueles JH, Rowlands AV, et al. GGIR: A Research Community-Driven Open Source
    R Package for Generating Physical Activity and Sleep Outcomes From Multi-Day Raw
    Accelerometer Data. Journal for the Measurement of Physical Behaviour. 2(3) 2019.
    doi:10.1123/jmpb.2018-0063.
    \item van Hees VT, Gorzelniak L, Dean Leon EC, Eder M, Pias M, et al. (2013) Separating
    Movement and Gravity Components in an Acceleration Signal and Implications for the
    Assessment of Human Daily Physical Activity. PLoS ONE 8(4): e61691.
    doi:10.1371/journal.pone.0061691
    \item van Hees VT, Fang Z, Langford J, Assah F, Mohammad A, da Silva IC, Trenell MI,
    White T, Wareham NJ, Brage S. Auto-calibration of accelerometer data for
    free-living physical activity assessment using local gravity and temperature:
    an evaluation on four continents. J Appl Physiol (1985). 2014 Aug 7
    \item van Hees VT, Sabia S, et al. (2015) A novel, open access method to
    assess sleep duration using a wrist-worn accelerometer, PLoS ONE, November 2015
  }
}

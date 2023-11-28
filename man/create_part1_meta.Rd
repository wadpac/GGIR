\name{create_part1_meta}
\alias{create_part1_meta}
\title{
  Creates Part 1 Milestone Data.
}
\description{
  Creates part 1 milestone data and folder structure to be used in tests.
  Not intended for direct use by the user.
}
\usage{
   create_part1_meta(desired_outputdir = "output_test",
                     Ndays = 2, windowsizes = c(5, 900, 3600),
                     lux = FALSE)
}
\arguments{
  \item{desired_outputdir}{
    Character (default = "output_test"). Name for the output directory to be
    generated.
  }
  \item{Ndays}{
    Integer (default = 2). Number of desired days in the output.
  }
  \item{windowsizes}{
    See \link{GGIR}
  }
  \item{lux}{
    Logical (default = FALSE). Whether to append light recording to the 
    milestone data.
  }
}
\keyword{internal}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
  Jairo H Migueles <jairo@jhmigueles.com>
}
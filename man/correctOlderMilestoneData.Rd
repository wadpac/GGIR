\name{correctOlderMilestoneData}
\alias{correctOlderMilestoneData}
\title{
Corrects milestone data from g.part1 generated in older GGIR versions
}
\description{
Some older versions of GGIR stored the milestone data in part 1 as factor.
This function identifies when that occurs and convert the affected columns
to the appropriate class (e.g., numeric).}
\usage{
  correctOlderMilestoneData(x)
}
\arguments{
  \item{x}{
    Data frame with metashort or metalong data as generated in \link{g.part1}
  }
}
\value{
 Data frame with the class fixed in the appropriate columns (i.e., light and 
 temperature columns)
}
  
\examples{
  \dontrun{
    correctOlderMilestoneData(x)
  }
}

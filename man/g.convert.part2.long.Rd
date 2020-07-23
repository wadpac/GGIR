\name{g.convert.part2.long}
\alias{g.convert.part2.long}
\title{
  Convert part 2 report to long format
}
\description{
  Not for direct access by used. This function is used 
  inside g.report.part2 and convert2 part 2 report to long 
  ormat if there
  are multiple segments per day
}
\usage{
g.convert.part2.long(daySUMMARY)
}
\arguments{
  \item{daySUMMARY}{
  Object available inside g.report.part2
  }
}
\value{
  Data.frame with long format version of daySUMMARY
}
\author{
  Vincent T van Hees <vincentvanhees@gmail.com>
}

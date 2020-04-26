\name{checkgz}
\alias{checkgz}
\title{
Creates file connection if file is gz-file
}
\description{
Checks whether character string points to a gz file. If yes
a connection is opened and the connection object is returned
}
\usage{
checkgz(filename)	
}
\arguments{
  \item{filename}{
Character string of filename including path
}

}
  
\examples{
\dontrun{
filename = "mydata.csv.gz"
read.csv(checkgz(filename))
}
}
\author{
Vincent T van Hees <vincentvanhees@gmail.com>
}
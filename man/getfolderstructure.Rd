\name{getfolderstructure}
\alias{getfolderstructure}
\title{
Extracts folderstructure based on data directory.
}
\description{
Extracts folderstructure based on data directory. This is used when
accelerometer files are stored in a hierarchical folder structure and
the user likes to have a reference to the exact position in the folder
tree, rather than just the filename
}
\usage{
getfolderstructure(datadir)	
}
\arguments{
  \item{datadir}{
Argument datadir as used in various other functions in GGIR
}
}
\value{
List with items:
item{fullfilenames}{vector with all full paths to the folders 
  including the name of the file itself}
item{foldername}{vector with only the names of the folder in which each
file is stroed (so only the most distal folder in the folder tree)}
}
  
\examples{
\dontrun{
folderstructure = getfolderstructure(datadir)
}
}

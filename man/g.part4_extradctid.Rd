\name{g.part4_extractid}
\alias{g.part4_extractid}
\title{
 Extracts ID from filename and finds matching rows in sleeplog
}
\description{
  Extracts ID from filename and finds matching rows in sleeplog. Function not
  designed for direct use by GGIR users.
}
\usage{
  g.part4_extractid(idloc, fname, dolog, sleeplogidnum, sleeplog, accid = c())
}
\arguments{
  \item{idloc}{
    See \link{g.part4}
  }
  \item{fname}{
    Full patth to filename
  }
  \item{dolog}{
    Boolean to indicate whether to rely on a sleeplog
  }
  \item{sleeplogidnum}{
    Should the participant identifier as stored in the sleeplog be
    interpretted as a number (TRUE=default) or a character (FALSE)?
  }
  \item{sleeplog}{
    Sleeplog data.frame passed on from g.part4
  }
  \item{accid}{
    ID extracted from the acceleration file in GGIR part3. If not available leave blank.
  }
}
\value{
 List with accid the ID and matching_indices_sleeplog a vector with matching row
 indices in the sleeplog
}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
}

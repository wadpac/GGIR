\name{HDCZA}
\alias{HDCZA}
\title{
  Heuristic algorithm looking at Distribution of Change in Z-Angle (HDCZA).
}
\description{
  HDCZA algorithm for estimating the SPT window.
  As used in function \link{g.sib.det}. Function is not intended for direct use
  by GGIR user.
}
\usage{
HDCZA(angle, perc = 10, spt_threshold = 15, sptblocksize = 30, 
      spt_max_gap = 60, ws3 = 5, constrain2range = FALSE)
}
\arguments{
  \item{angle}{
    Vector of epoch level estimates of angle
  }
  \item{perc}{
    Number to indicate percentage threshold (default 10 corresponds to 2018
    paper)
  }
  \item{spt_threshold}{
    Numeric threshold used in HDCZA algorithm (default 15 corresponds to 
    2018 paper)
  }
  \item{sptblocksize}{
    Number to indicate minimum SPT block size (minutes)
  }
  \item{spt_max_gap}{
    Number to indicate maximum gap (minutes) in SPT window blocks.
  }
  \item{ws3}{
    Number representing epoch length in seconds
  }
  \item{constrain2range}{
    Bolean to indicate whether threshold should be constrained to a range
  }
}
\value{
  List with start and end times of the SPT window and the threshold as used.
}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
}
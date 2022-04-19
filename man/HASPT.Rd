\name{HASPT}
\alias{HASPT}
\title{
  Heuristic Algorithms estimating SPT window.
}
\description{
  As used in function \link{g.sib.det}. Function is not intended for direct use
  by GGIR user.
}
\usage{
HASPT(angle, perc = 10, spt_threshold = 15, sptblocksize = 30, 
      spt_max_gap = 60, ws3 = 5, constrain2range = FALSE,
      HASPT.algo="HDCZA", invalid, HASPT.ignore.invalid=FALSE)
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
    Numeric threshold used in HASPT algorithm (default 15 corresponds to 
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
  \item{HASPT.algo}{
    Character to indicate what algortihm should be used. Default "HDCZA" is 
    Heuristic algorithm looking at Distribution of Change in Z-Angle as
    described in van Hees et al. 2018. Other options included:
    "HorAngle", which is based on HDCZA but replaces non-movement detection of 
    the HDCZA algorithm by looking for time segments where the angle of the 
    longitudinal sensor axis has an angle relative to the horizontal plane
    between -45 and +45 degrees.
  }
  \item{invalid}{
    Integer vector with per epoch an indicator of valid(=0) or invalid(=1) epoch.
  }
  \item{HASPT.ignore.invalid}{
    Boolean to indicate whether invalid time segments should be ignored
  }
}
\value{
  List with start and end times of the SPT window and the threshold as used.
}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
}
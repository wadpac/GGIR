\name{g.create.sp.mat}
\alias{g.create.sp.mat}
\title{
Converts sleep period information. Not intended for direct use
}
\description{
Function to convert data into sleep period matrix part of g.part4.R.
Not intended for direct use by package user
}
\usage{
g.create.sp.mat(nsp,spo,sleepdet.t,daysleep=FALSE)

}
\arguments{
  \item{nsp}{
  Integer indicating the number of sleep periods
  } 
 \item{spo}{
  Empty matrix with overview of sleep periods, 5 columns and
  as along as nps
  }
 \item{sleepdet.t}{
   Part of detected sleep from g.sib.det for one night and one 
   sleep definition
  }
 \item{daysleep}{
  Boolean to indicator whether this person woke up
  after noon (daysleeper)
  }
 
}
\value{
 \itemize{
\item spo matrix with start and end of each sleep period
\item calendardate date corresponding to the day on which the night started
\item item wdayname weekdayname
}
}
\author{
  Vincent T van Hees <v.vanhees@accelting.com>
}

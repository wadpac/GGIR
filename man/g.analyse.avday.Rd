\name{g.analyse.avy}
\alias{g.analyse.avday}
\title{
 Function supports \link{g.analyse}. Not intended for direct use by user.
}
\description{
Generatess average day analyses and fills corresponding output
matrix, \link{g.analyse}.
}
\usage{
g.analyse.avday(qlevels, doquan, averageday, M, IMP, 
  t_TWDI, quantiletype, winhr, L5M5window, M5L5res, 
  ws3, IVIS_epochsize_seconds, 
  IVIS_windowsize_minutes, IVIS.activity.metric, doiglevels,
  firstmidnighti, ws2, midnightsi, iglevels, qM5L5, MX.ig.min.dur=10)
}

\arguments{
\item{qlevels}{see \link{g.analyse}}
\item{doquan}{Boolean whether quantile analysis should be done}
\item{averageday}{ As produced by \link{g.impute}}
\item{M}{ As produced by \link{g.getmeta}}
\item{IMP}{ As produced by \link{g.impute}}
\item{t_TWDI}{ Same as qwindow as described in \link{g.analyse}}
\item{quantiletype}{see \link{g.analyse}}
\item{winhr}{see \link{g.analyse}}
\item{L5M5window}{see \link{g.analyse}}
\item{M5L5res}{see \link{g.analyse}}
\item{ws3}{ Epoch size in seconds}
\item{IVIS_epochsize_seconds}{see \link{g.IVIS}}
\item{IVIS_windowsize_minutes}{see \link{g.IVIS}}
\item{IVIS.activity.metric}{see \link{g.IVIS}}
\item{doiglevels}{Boolean to indicate whether iglevels should be calculated}
\item{firstmidnighti}{see \link{g.detecmidnight}}
\item{ws2}{see \link{g.weardec}}
\item{midnightsi}{see \link{g.detecmidnight}}
\item{iglevels}{see \link{g.analyse}}
\item{qM5L5}{see \link{g.getM5L5}}
\item{MX.ig.min.dur}{see \link{g.getM5L5}}
}
\value{
\item{\code{InterdailyStability}}{}
\item{\code{IntradailyVariability}}{}
\item{\code{igfullr_names}}{}
\item{\code{igfullr}}{}
\item{\code{QUAN}}{}
\item{\code{qlevels_names}}{}
\item{\code{ML5AD}}{}
\item{\code{ML5AD_names}}{}
}

\author{
Vincent T van Hees <v.vanhees@accelting.com>
}
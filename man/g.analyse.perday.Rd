\name{g.analyse.perday}
\alias{g.analyse.perday}
\title{
 Function supports \link{g.analyse}. Not intended for direct use by user.
}
\description{
Generates day specific analyses and fills corresponding
output matrix, \link{g.analyse}.
}
\usage{
g.analyse.perday(selectdaysfile, ndays, firstmidnighti, 
  time, nfeatures, window.summary.size, qwindow, midnightsi,
  metashort, averageday, ENMOi, LFENMOi, BFENi, ENi,
  HFENi, HFENplusi, MADi, ENMOai, doiglevels, nfulldays, lastmidnight, 
  ws3, ws2, qcheck, fname, idloc, BodyLocation, wdayname,
  tooshort, includedaycrit, winhr,L5M5window, M5L5res, 
  doquan, qlevels, quantiletype, doilevels, 
  ilevels, iglevels, domvpa,
  mvpathreshold, boutcriter, closedbout,
  bout.metric, mvpadur, mvpanames, wdaycode, IDd, ID, ID2,
  deviceSerialNumber, qM5L5, ExtFunColsi, myfun, desiredtz, MX.ig.min.dur=10)
}

\arguments{
\item{selectdaysfile}{see \link{g.analyse}}
\item{ndays}{Number of days in file} 
\item{firstmidnighti}{see \link{g.detecmidnight}}
\item{time}{timestamp column from metalong converted to character}
\item{nfeatures}{estimate of number of variables that
need to be stored in the output matrix}
\item{window.summary.size}{see \link{g.analyse}} 
\item{qwindow}{see \link{g.analyse}}
\item{midnightsi}{see \link{g.detecmidnight}}
\item{metashort}{see \link{g.impute}}
\item{averageday}{As produced by \link{g.impute}}
\item{ENMOi}{column index of metahosrt where metric is stored}
\item{LFENMOi}{column index of metahosrt where metric is stored}
\item{BFENi}{column index of metahosrt where metric is stored}
\item{ENi}{column index of metahosrt where metric is stored}
\item{HFENi}{column index of metahosrt where metric is stored}
\item{HFENplusi}{column index of metahosrt where metric is stored}
\item{MADi}{column index of metahosrt where metric is stored}
\item{ENMOai}{column index of metahosrt where metric is stored}
\item{doiglevels}{Boolean to indicate whether iglevels should be calculated}
\item{nfulldays}{Number of days between the first and last midnight in the recording}
\item{lastmidnight}{see \link{g.detecmidnight}}
\item{ws3}{Epoch size in seconds}
\item{ws2}{see \link{g.weardec}}
\item{qcheck}{vector with zeros and ones for each epoch, respenting the
quality check derived with g.impute}
\item{fname}{RData filename produced by g.part1}
\item{idloc}{see \link{g.analyse}}
\item{BodyLocation}{as produced by \link{g.extractheadervars}} 
\item{wdayname}{character with weekdayname}
\item{tooshort}{0 (file not too short) or 1 (file too short)} 
\item{includedaycrit}{see \link{g.analyse}} 
\item{winhr}{see \link{g.analyse}}
\item{L5M5window}{see \link{g.analyse}} 
\item{M5L5res}{see \link{g.analyse}}
\item{doquan}{Boolean whether quantile analysis should be done}
\item{qlevels}{see \link{g.analyse}}
\item{quantiletype}{see \link{g.analyse}} 
\item{doilevels}{Boolean whether to generate ilevels, see \link{g.analyse}} 
\item{ilevels}{see \link{g.analyse}}
\item{iglevels}{see \link{g.analyse}} 
\item{domvpa}{Boolean whether to do mvpa analysis}
\item{mvpathreshold}{see \link{g.analyse}}
\item{boutcriter}{see \link{g.analyse}}
\item{closedbout}{see \link{g.analyse}}
\item{bout.metric}{see \link{g.analyse}} 
\item{mvpadur}{see \link{g.analyse}}
\item{mvpanames}{Matrix with 6 columns and 1 row
holding the names for the six mvpa variables}
\item{wdaycode}{Equal to M$wday as produced by \link{g.getmeta}}
\item{IDd}{As produced by \link{g.extractheadervars}}
\item{ID}{As produced by \link{g.extractheadervars}}
\item{ID2}{gnerated inside \link{g.analyse} either a copy of ID, or
as character string before the first hyphen}
\item{deviceSerialNumber}{As produced by \link{g.extractheadervars}}
\item{qM5L5}{see \link{g.getM5L5}}
\item{ExtFunColsi}{column index of metahosrt where metric is stored}
\item{myfun}{External function object to be applied to raw data, see \link{g.getmeta}.}
\item{desiredtz}{see \link{g.getmeta}}
\item{MX.ig.min.dur}{see \link{g.getM5L5}}
}
\value{
\item{\code{daysummary}}{Summary per day for the file that was analysed}
\item{\code{ds_names}}{Variable names in daysummary}
\item{\code{windowsummary}}{Window summary, only used when
selectdayfile is specified}
\item{\code{ws_names}}{Variable names in windowsummary}}

\author{
Vincent T van Hees <v.vanhees@accelting.com>
}
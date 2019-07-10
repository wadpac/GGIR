\name{GGIR-package}
\alias{GGIR-package}
\alias{GGIR}
\docType{package}
\title{
A package to process multi-day raw accelerometer data
}
\description{
GGIR is an R-package to process multi-day raw accelerometer data. It was developed
in the context of research on human daily physical activity with wearable
tri-axial acceleration sensors. The term raw accelerometry refers to data being
expressed in m/s2 or gravitational acceleration as opposed to the previous
generation accelerometers which stored only processed summary measures.\cr
\cr
For a tutorial and more background in formation on GGIR, please see the
package vignette: Accelerometer data processing with GGIR\cr
\cr
The package has been developed with and for the accelerometer brands Genea and GENEActive.
Additionally, it should work for .csv data from GENEActiv, .csv data from
Actigraph, and .wav, .csv () and .cwa format from AX3 (Axivity). Although, I have tested
this less thoroughly compared with the binary data formats from Genea and Geneactiv.\cr
Note for Actigraph users: please do not
export timestamps to the csv-file as this causes memory issues. To cope with the
absense of timestamps the code will re-caculate timestamps from the sample frequency
and the start time and date as presented in the file header.\cr
\cr
A non-exhaustive overview of publications related to GGIR can be found
\href{https://github.com/wadpac/GGIR/wiki/Publication-list}{here}\cr
\cr
Function \link{g.inspectfile} assessess to which monitor brand the file belongs
and extracts the file header; function \link{g.calibrate} helps to investigate
calibration error based on free-living data and proposes correction factors;
function \link{g.getmeta} extracts the signal features; \link{g.impute} takes
that information, identifies unreliable signal sections (e.g. monitor not worn
or signal clips near its extreme) and replaces these
sections by imputed values; and finally  \link{g.analyse} takes the output from
all the functions, runs a basic descriptive analysis and then summarises the output
both per measurement and per day of measurement.\cr
\cr
To enhance the feasibility of using these individual functions I am providing a
couple of shell functions to ease implementing the above functions in study data
by less experienced R-users. Here, the main shell function is \link{g.shell.GGIR}
and allows for automating the full analysis of a dataset
including all necessary calls to the functions above. Function
g.shell.GGIR relies on functions \link{g.part1} and \link{g.part2} also part
of this package. In summary, the user is expected to specify the location of the
acceleerometer data and the desired output folder. Next, data is loaded and pre-
processed with g.getmeta and g.calibrate. Next, the output is converted
to a conveniently portable .RData-format away from the R workspace.
Next, these .RData files are used as input for g.part2.\cr
\cr
Note that g.part1 generates a folder structure to help the user keep track of
various output files and milestone data. The folder structure entails:
One master folder with a name output_xx where xx is equal to the name of the
original data folder. Inside the output_xx folder there will be one folder
named meta including all the milestone and a folder results with all the results.
Inside the meta folder the following subfolders are created:
basic, ms2.out, ms3out, ms4out, and ms5out for respectively g.part1, g.part2, g.part3,
g.part4, and g.part5 milestone data.\cr
\cr
The reason why g.part1 and g.part2 are not merged as one generic shell
function is because g.part1 takes much longer to
run and involves only minor decisions of interest to the movement scientist.
Function g.part2 on the other hand is relatively fast and comes with all the
decisions that directly impact on the variables that are of interest to the movement
scientist. Therefore, the user may want to run g.part1 overnight or on a computing
cluster, while g.part2 can then be the main playing ground for the movement scientist.
So, function g.shell.GGIR basically is the central point for operating both g.part1 and
g.part2 and most users should not really need to interact with g.part1 or g.part2
directly. More recently I expanded the package with g.part3 and g.part4 which
provide functionality for estimating sleep and sustained inactivity bouts.\cr
\cr
g.part5 finally takes the output from parts 2 and 4 to describe time spent between
waking up in the morning and waking up the next day subdivided by behavioural category.
g.part5 calculates for each of these categories the time spent, the number of bouts,
the average acceleration and the number of blocks.\cr
\cr
If you want to use this package for a different data format (e.g. from a different
accelerometer brand) then please provide me with: the R-code to read the data and
example files for testing purposes.\cr
\cr
Please note that there is google discussion group for this package (link below).\cr
\cr
You can thank me for sharing the code in this package and for developing
it as a generic purpose tool by citing the package name and by
citing the supporting publications in your own scientific
journal/conference publications.
}
\details{
\tabular{ll}{
Package: \tab GGIR\cr
Type: \tab Package\cr
Version: \tab 1.9-2\cr
Date: \tab 2019-07-03\cr
License: \tab LGPL (>= 2.0, < 3)\cr
Discussion group: \tab https://groups.google.com/forum/#!forum/rpackageggir\cr
}
}
\examples{
\dontrun{
  #inspect file:
  I = g.inspectfile(datafile)

  #autocalibration:
  C = g.calibrate(datafile)

  #get meta-data:
  M = g.getmeta(datafile)
}
data(data.getmeta)
data(data.inspectfile)
data(data.calibrate)

#impute meta-data:
IMP = g.impute(M = data.getmeta, I = data.inspectfile)
#analyse and produce summary:
A = g.analyse(I = data.inspectfile, C = data.calibrate, M = data.getmeta, IMP)
#plot data
g.plot(IMP, M = data.getmeta, I = data.inspectfile, durplot=4)
}
\author{
\itemize{
\item Vincent T van Hees <vincentvanhees@gmail.com> main developer
\item Zhou Fang co-developed function \link{g.calibrate}
\item Jing Hua Zhao <jinghua.zhao@mrc-epid.cam.ac.uk> co-developed function \link{g.binread}
\item Joe Heywood helped develop the functionality to process only specific days
\item Evgeny Mirkes developed function \link{g.cwaread}
\item Severine Sabia tested and provided feedback on various functions
\item Joan Capdevila Pujol helped to improve various function
\item Jairo H Migueles <jairohm@ugr.es> helped to improve various functions
}
}
\references{
\itemize{
\item van Hees VT, Gorzelniak L, Dean Leon EC, Eder M, Pias M, et al. (2013) Separating
Movement and Gravity Components in an Acceleration Signal and Implications for the
Assessment of Human Daily Physical Activity. PLoS ONE 8(4): e61691.
doi:10.1371/journal.pone.0061691
\item van Hees VT, Fang Z, Langford J, Assah F, Mohammad A, da Silva IC, Trenell MI,
White T, Wareham NJ, Brage S. Auto-calibration of accelerometer data for
free-living physical activity assessment using local gravity and temperature:
an evaluation on four continents. J Appl Physiol (1985). 2014 Aug 7
\item van Hees VT, Sabia S, et al. (2015) A novel, open access method to
assess sleep duration using a wrist-worn accelerometer, PLoS ONE, November 2015
}
}

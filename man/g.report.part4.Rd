\name{g.report.part4}
\alias{g.report.part4}
\title{
Generate report from milestone data produced by \link{g.part4}
}
\description{
Creates report from milestone data produced by \link{g.part4}. Not intended
  for direct use by package user
}
\usage{
g.report.part4(datadir=c(),metadatadir=c(),loglocation = c(),f0=c(),
  f1=c(),storefolderstructure=TRUE, data_cleaning_file=c())
  
}
\arguments{
  \item{datadir}{
  see \link{g.part4}
  }
  \item{metadatadir}{
  see \link{g.part4}
  }
  \item{loglocation}{
  see \link{g.part4}
  }
  \item{f0}{
  see \link{g.part4}
  }
  \item{f1}{
  see \link{g.part4}
  }
  \item{storefolderstructure}{
  see \link{g.part4}
  }
    \item{data_cleaning_file}{
  see \link{g.part4}
  }

}
\value{
Function does not produce data, but only writes reports
in csv format and a visual report in pdf.

The following files are stored in the root of the results folder:
part4_nightsummary_sleep_cleaned.csv
part4_summary_sleep_cleaned.csv

The following files are stored in the folder results/QC:
part4_nightsummary_sleep_full.csv
part4_summary_sleep_full.csv

If a sleeplog is used *_full.csv as stored in the QC folder includes estimates 
for all nights in the data, and *_cleaned.csv in the results folder includes
estimates for all nights in the data excluding the nights that did not had a
sleeplog entry or had no valid accelerometer data.

If a sleep log is not used then * _cleaned.csv includes the nights that are
in *_full.csv excluding the nights with insufficient data.

If you have a study where the sleeplog was available for a subset of the
participants, but you want to include all individuals in your analysis, 
then use the *_full.csv output and clean the night level data yourself
by excluding  rows with cleaningcode > 1 which are the cases where no
or invalid accelerometer data was present.

The above means that for studies with missing sleeplog entries for some
individuals and some nights using the *_full.csv output and excluding
rows (nights) with cleaningcode > 1 will lead to the same as
* _cleaned.csv plus sleep estimates for the nights with missing
sleeplog, providing that there was enough accelerometer data for 
those nights.

In other words, *_cleaned.csv is perfect if you only want to rely on 
nights with a sleeplog or if you do not use a sleeplog at all. For all
other scenarios We advise using the *_full.csv report and to clean it 
yourself.

See package vignette sections "Sleep analysis" and "Output part 4"
for a more elaborative description of the sleep analysis and reporting.
}
\author{
  Vincent T van Hees <vincentvanhees@gmail.com>
}

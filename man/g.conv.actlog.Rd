\name{g.conv.actlog}
\alias{g.conv.actlog}
\title{
  Function to read activity log and make it useful for the rest of GGIR.
}
\description{
 Function to read activity log and convert it into data.frame
 that has for each ID and date a different qwindow vector
  
}
\usage{
  g.conv.actlog(qwindow, qwindow_dateformat="\%d-\%m-\%Y")	
}
\arguments{
  \item{qwindow}{
    Path to csv file with activity log. Expected format of the activity diary is:
    First column headers followed by one row per recording, first column is recording ID,
    which needs to match with the ID GGIR extracts from the accelerometer file.
    Followed by date column in format "23-04-2017", where date format is specified by 
    argument qwindow_dateformat (below). Use the character combination date, Date or 
    DATE in the column name. This is followed by 
    one or multiple columns with start times for the activity types in that day format in 
    hours:minutes:seconds. The header of the column will be used as label for each activity
    type. Insert a new date column before continuing with activity types for next day.
    Leave missing values empty. If an activitylog is used then individuals who do 
    not appear in the activitylog will still be processed with value c(0,24).
    Dates with no activiy log data can be skipped, no need to have a column with the 
    date followed by a column with the next date.
  }
  \item{qwindow_dateformat}{
    Character specifying the date format used in the activity log.
  }
}
\value{
Data.frame with column ID, date and qwindow, where each
qwindow value is a qwindow vector
}
\author{
Vincent T van Hees <v.vanhees@accelting.com>
}

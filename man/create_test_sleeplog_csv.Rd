\name{create_test_sleeplog_csv}
\alias{create_test_sleeplog_csv}
\title{
  Creates csv sleeplog file for testing purposes
}
\description{
  Creates sleeplog file in the format as expected by g.part4
with dummy data (23:00 onset, 07:00 waking time for every night).
}
\usage{
  create_test_sleeplog_csv(Nnights = 7, storagelocation = c(),
                            advanced = FALSE, sep = ",", begin_date = "2016/06/25",
                            type = "sleeplog")
}
\arguments{
  \item{Nnights}{
    Number of nights (minimum is 1)
  }
  \item{storagelocation}{
    Location where the test file named testfile.csv will be stored
    If no value is provided then the function uses the current 
    working directory
  }
  \item{advanced}{
    Boolean to indicate whether to create an advanced sleeplog that also includes
    logs of nap times and nonwear
  }
  \item{sep}{
    Character to indicate the column separator of the csv file.
  }
  \item{begin_date}{
    Character to indicate first date (in format "2016/06/25") to be used
    in the advanced sleeplog format. Ignored when generated basic sleeplog format.
  }
  \item{type}{
    Character to indicate which type of advanced sleeplog to create,
    either "sleeplog" with "wakeup" and "onset" columns, "bedlog" with 
    "bedstart" and "bedend" columns, or "both" with both.
  }
}
\value{
 The function does not produce any output values. Only the file is
 stored
}
  
\examples{
  \dontrun{
    create_test_sleeplog_csv()
  }
}

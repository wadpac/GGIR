\name{tidyup_df}
\alias{tidyup_df}
\title{
  Round numeric columns and replace NA/NaN values by blank
}
\description{
  Identifies columns that can be coerced to numeric in a data frame, 
  transforms these columns to numeric and round them to the specified digits. 
  It also replaces NA and NaNs values by blank.
}
\usage{
  tidyup_df(df = c(), digits = 3)
}
\arguments{
  \item{df}{
    Data frame
}
\item{digits}{
    Integer indicating the number of decimal places (round) or significant digits (signif) to be used
}
}
\value{
  Data frame with all possible columns as numeric and rounded to the specified number of digits
}

\author{
  Jairo H Migueles
}
\examples{
  # Test data frame
  df = data.frame(a = c("a", "b"), b = as.character(c(1.543218, 8.216856483)))
  tidyup_df(df = df, digits = 3)
}
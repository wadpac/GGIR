\name{round_decimals_df}
\alias{round_decimals_df}
\title{
  Round numeric columns in a data frame to n digit places
}
\description{
  Identifies columns that can be coerced to numeric in a data frame, 
  transforms these columns to numeric and then round them to the specified digits
}
\usage{
  round_decimals_df(df = c(), digits = 3)
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
  round_decimals_df(df = df, digits = 3)
}

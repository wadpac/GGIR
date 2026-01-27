# Round numeric columns and replace NA/NaN values by blank

Identifies columns that can be coerced to numeric in a data frame,
transforms these columns to numeric and round them to the specified
digits. It also replaces NA and NaNs values by blank.

## Usage

``` r
tidyup_df(df = c(), digits = 3)
```

## Arguments

- df:

  Data frame

- digits:

  Integer indicating the number of decimal places (round) or significant
  digits (signif) to be used

## Value

Data frame with all possible columns as numeric and rounded to the
specified number of digits

## Author

Jairo H Migueles

## Examples

``` r
  # Test data frame
  df = data.frame(a = c("a", "b"), b = as.character(c(1.543218, 8.216856483)))
  tidyup_df(df = df, digits = 3)
#>   a     b
#> 1 a 1.543
#> 2 b 8.217
```

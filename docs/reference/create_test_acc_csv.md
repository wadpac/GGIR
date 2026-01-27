# Creates csv data file for testing purposes

Creates file in the Actigraph csv data format with dummy data that can
be used for testing. The file includes accelerometer data with bouts of
higher acceleration, variations non-movement periods in a range of
accelerometer positions to allow for testing the auto-calibration
functionality.

## Usage

``` r
create_test_acc_csv(sf=3,Nmin=2000,storagelocation=c(),
                      start_time = NULL, starts_at_midnight = FALSE)
```

## Arguments

- sf:

  Sample frequency in Hertz, the default here is low to minimize file
  size

- Nmin:

  Number of minutes (minimum is 720)

- storagelocation:

  Location where the test file named testfile.csv will be stored If no
  value is provided then the function uses the current working directory

- start_time:

  Start time of the recording, in the hh:mm:ss format.

- starts_at_midnight:

  Boolean indicating whether the recording should start at midnight.
  Ignored if start_time is specified.

## Value

The function does not produce any output values. Only the file is stored

## Examples

``` r
  if (FALSE) { # \dontrun{
    create_test_acc_csv()
  } # }
```

# Impute gaps in three axis raw accelerometer data

Removes all sample with a zero in each of the three axes, and then (as
default) imputes time gaps by the last recorded value per axis
normalised to 1 \_g\_

## Usage

``` r
g.imputeTimegaps(x, sf, k = 0.25, impute = TRUE, 
                   PreviousLastValue = c(0,0,1), 
                   PreviousLastTime = NULL, epochsize = NULL)
```

## Arguments

- x:

  Data.frame with raw accelerometer data, and a timestamp column with
  millisecond resolution.

- sf:

  Sample frequency in Hertz

- k:

  Minimum time gap length to be imputed

- impute:

  Boolean to indicate whether the time gaps identified should be imputed

- PreviousLastValue:

  Automatically identified last value in previous chunk of data read.

- PreviousLastTime:

  Automatically identified last timestamp in previous chunk of data
  read.

- epochsize:

  Numeric vector of length two, with short and long epoch sizes.

## Value

List including: - x, data.frame based on input x with timegaps imputed
(as default) or with recordings with 0 values in the three axes removed
(if impute = FALSE) - QClog, data.frame with information on the number
of time gaps found and the total time imputed in minutes

## Author

Vincent T van Hees \<v.vanhees@accelting.com\>

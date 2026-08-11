# Intensity gradient calculation

Calculates the intensity gradient based on Rowlands et al. 2018. The
function assumes that the user has already calculated the value
distribution.

## Usage

``` r
g.intensitygradient(x,y)
```

## Arguments

- x:

  Numeric vector of mid-points of the bins (mg)

- y:

  Numeric vector of time spent in bins (minutes)

## Value

- y_intercept:

  y-intercept of a linear regression line in log-log space

- gradient:

  Beta coefficient of a linear regression line in log-log space

- rsquared:

  R squared of x and y values in log-log space

## Author

Vincent T van Hees \<v.vanhees@accelting.com\>

## References

Rowlands A, Edwardson CL, et al. (2018) Beyond Cut Points: Accelerometer
Metrics that Capture the Physical Activity Profile. MSSE 50(6):1.
doi:10.1249/MSS.0000000000001561

# Calculates IV and IS

To extract interdaily stability and interdaily variability as originally
proposed by van Someren.

## Usage

``` r
g.IVIS(Xi, epochSize = 60, threshold = NULL)
```

## Arguments

- Xi:

  Vector with acceleration values, e.g. ENMO metric.

- epochSize:

  Epoch size of the values in Xi expressed in seconds.

- threshold:

  Acceleration threshold to distinguish inactive from active

## Value

- InterdailyStability:
- IntradailyVariability:

## Author

Ian Meneghel Danilevicz \<ian.meneghel-danilevicz@inserm.fr\> Vincent T
van Hees \<v.vanhees@accelting.com\>

## References

- Eus J. W. Van Someren, Dick F. Swaab, Christopher C. Colenda, Wayne
  Cohen, W. Vaughn McCall & Peter B. Rosenquist. Bright Light Therapy:
  Improved Sensitivity to Its Effects on Rest-Activity Rhythms in
  Alzheimer Patients by Application of Nonparametric Methods
  Chronobiology International. 1999. Volume 16, issue 4.

## Examples

``` r
  Xi = abs(rnorm(n = 10000,mean = 0.2))
  IVISvariables = g.IVIS(Xi=Xi)
```

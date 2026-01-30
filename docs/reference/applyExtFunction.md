# Apply external function to acceleration data.

Applies external function to the raw acceleration data within GGIR. This
makes it easier for new algorithms developed to be pilotted on
accelerometer data while taking advantage of the existing comprehensive
GGIR data management and analysis infrastructure. This function is not
for direct interaction by user, please supply object `myfun` to
[GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md) or
[g.part1](https://wadpac.github.io/GGIR/reference/g.part1.md). Object
`myfun` is a list as detailed below.

## Usage

``` r
applyExtFunction(data, myfun, sf, ws3, interpolationType=1)
```

## Arguments

- data:

  Data data.frame as present internally in
  [g.getmeta](https://wadpac.github.io/GGIR/reference/g.getmeta.md). It
  has at least four columns of which the first is the timestamp followed
  by the x, y, and z acceleration.

- myfun:

  See details, in short: myfun is a list object that holds the external
  function to be applied to the data and various parameters to aid in
  the process.

- sf:

  Sample frequency (Hertz) of the data object

- ws3:

  Short epoch size (first value of windowsizes in
  [g.getmeta](https://wadpac.github.io/GGIR/reference/g.getmeta.md)).

- interpolationType:

  Integer to indicate type of interpolation to be used when resampling
  time series (mainly relevant for Axivity sensors), 1=linear, 2=nearest
  neighbour.

## Value

The output of the external algorithm aggregated or repeated to fit the
short epoch length of GGIR. Therefore, the short epoch length of GGIR
should be a multitude of the resolution of the external function output,
or visa versa.

## Details

See package vignette for detailed tutorial with examples on how to use
the function embedding:
https://cran.r-project.org/web/package=GGIR/vignettes/applyExtFunction.pdf
Function applyExtFunction is typically not used by the GGIR user
directly.

## Author

Vincent T van Hees \<v.vanhees@accelting.com\>

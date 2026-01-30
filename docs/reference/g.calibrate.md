# function to estimate calibration error and make recommendation for addressing it

Function starts by identifying ten second windows of non-movement. Next,
the average acceleration per axis per window is used to estimate
calibration error (offset and scaling) per axis. The function provides
recommended correction factors to address the calibration error and a
summary of the callibration procedure.

## Usage

``` r
g.calibrate(datafile, params_rawdata = c(), params_general = c(),
              params_cleaning = c(), inspectfileobject = c(), verbose = TRUE, ...)
```

## Arguments

- datafile:

  Name of accelerometer file

- params_rawdata:

  See [g.part1](https://wadpac.github.io/GGIR/reference/g.part1.md)

- params_general:

  See [g.part1](https://wadpac.github.io/GGIR/reference/g.part1.md)

- params_cleaning:

  See [g.part1](https://wadpac.github.io/GGIR/reference/g.part1.md)

- inspectfileobject:

  Output from the function
  [g.inspectfile](https://wadpac.github.io/GGIR/reference/g.inspectfile.md).

- verbose:

  Boolean (default = TRUE). to indicate whether console message should
  be printed. Note that warnings and error are always printed and can be
  suppressed with suppressWarning() or suppressMessages().

- ...:

  Any argument used in the previous version of g.calibrate, which will
  now be used to overrule the arguments specified with the parameter
  objects.

## Value

- `scale`:

  scaling correction values, e.g. c(1,1,1)

- `offset`:

  offset correction values, e.g. c(0,0,0)

- `tempoffset`:

  correction values related to temperature, e.g. c(0,0,0)

- `cal.error.start`:

  absolute difference between Euclidean norm during all non-movement
  windows and 1 g before autocalibration

- `cal.error.end`:

  absolute difference between Euclidean norm during all non-movement
  windows and 1 g after autocalibration

- `spheredata`:

  average, standard deviation, Euclidean norm and temperature (if
  available) for all ten second non-movement windows as used for the
  autocalibration procedure

- `npoints`:

  number of 10 second no-movement windows used to populate the sphere

- `nhoursused`:

  number of hours of measurement data scanned to find the ten second
  time windows with no movement

- `meantempcal`:

  mean temperature corresponding to the data as used for
  autocalibration. Only applies to data where temperate data is
  collected and available to GGIR, such as GENEActiv, Axivity, and in
  some instances ad-hoc .csv data.

## Author

Vincent T van Hees \<v.vanhees@accelting.com\> Zhou Fang

## References

- van Hees VT, Fang Z, Langford J, Assah F, Mohammad A, da Silva IC,
  Trenell MI, White T, Wareham NJ, Brage S. Auto-calibration of
  accelerometer data for free-living physical activity assessment using
  local gravity and temperature: an evaluation on four continents. J
  Appl Physiol (1985). 2014 Aug 7

## Examples

``` r
  if (FALSE) { # \dontrun{
    datafile = "C:/myfolder/testfile.bin"
    
    #Apply autocalibration:
    C = g.calibrate(datafile)
    print(C$scale)
    print(C$offset)
  } # }
```

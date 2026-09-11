# function to inspect accelerometer file for brand, sample frequency and header

Inspects accelerometer file for key information, including: monitor
brand, sample frequency and file header

## Usage

``` r
g.inspectfile(datafile, desiredtz = "", params_rawdata = c(),
                         configtz = c(), ...)
```

## Arguments

- datafile:

  name of data file

- desiredtz:

  Desired timezone, see documentation
  [g.getmeta](https://wadpac.github.io/GGIR/reference/g.getmeta.md)

- params_rawdata:

  See [g.part1](https://wadpac.github.io/GGIR/reference/g.part1.md)

- configtz:

  ...

- ...:

  Any argument used in the previous version of g.getmeta, which will now
  be used to overrule the arguments specified with the parameter
  objects.

## Value

- header:

  fileheader

- monn:

  monitor name (genea, geneactive)

- monc:

  monitor brand code (0 - ad-hoc file format, 1 = genea
  (non-commercial), 2 = GENEActive, 3 = actigraph, 4 = Axivity (AX3,
  AX6), 5 = Movisense, 6 = Verisense)

- dformn:

  data format name, e.g bin, csv, cwa, gt3x

- dformc:

  data format code (1 = .bin, 2 = .csv, 3 = .wav, 4 = .cwa, 5 = ad-hoc
  .csv, 6 = .gt3x)

- sf:

  samplefrequency in Hertz

- filename:

  filename

## Author

Vincent T van Hees \<v.vanhees@accelting.com\>

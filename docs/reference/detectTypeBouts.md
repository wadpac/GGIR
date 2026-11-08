# Detect bouts for each type as produced by external function

Detect bouts for each type produced by an external function. Bout
detection is done with `g.getbout` using the `tbout.dur` and
`tbout.criter` parameters that may be specified in `myfun`.

## Usage

``` r
detectTypeBouts(myfun, varnum_type, 
                   daysummary, ds_names,
                   di, fi, ws3, boutnameEnding)
```

## Arguments

- myfun:

  See [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md)

- varnum_type:

  Vector (usually of class factor) containing the type metric as
  produced by the external function.

- daysummary:

  Matrix with preliminary day summary.

- ds_names:

  Daysummary column names

- di:

  Row index of daysummary representing the recording day

- fi:

  Column index of daysummary

- ws3:

  Numeric epochsize in seconds of the timeseries

- boutnameEnding:

  Character with ending suffix to be pasted to the bout variable names,
  indicating the segment of the day over which the bouts have been
  calculated.

## Value

A list containing the updated `daysummary`, `ds_names`, `fi`, and `di`
objects.

## Author

Vincent T van Hees \<v.vanhees@accelting.com\> Jairo H Migueles
\<jairo@jhmigueles.com\>

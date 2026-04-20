# Assesses whether decimals in fileheader are stored with comma or dot separated decimals

The function is used by
[g.readaccfile](https://wadpac.github.io/GGIR/reference/g.readaccfile.md)
to assess how numeric data should be interpretted

## Usage

``` r
g.dotorcomma(inputfile, dformat, mon, ...)
```

## Arguments

- inputfile:

  full path to inputfile

- dformat:

  Data format code: 1=.bin, 2=.csv, 3=.wav, 4=.cwa, 5=.csv for ad-hoc
  monitor brand

- mon:

  Monitor code (accelorometer brand): 0=undefined, 1=GENEA, 2=GENEActiv,
  3=Actigraph, 4=Axivity, 5=Movisense, 6=Verisense

- ...:

  Any input arguments needed for function
  [read.myacc.csv](https://wadpac.github.io/GGIR/reference/read.myacc.csv.md)
  if you are working with a non-standard csv formatted files.

## Value

Character object showing how decimals are separated

## Author

Vincent T van Hees \<v.vanhees@accelting.com\>

## Examples

``` r
  if (FALSE) { # \dontrun{
    decn = g.dotorcomma(inputfile="C:/myfile.bin",dformat=1,mon=2)
  } # }
```

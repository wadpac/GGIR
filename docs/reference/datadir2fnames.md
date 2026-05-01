# Generates vector of file names out of datadir input argument

Uses input argument datadir from
[g.part1](https://wadpac.github.io/GGIR/reference/g.part1.md) and the
output from
[isfilelist](https://wadpac.github.io/GGIR/reference/isfilelist.md) to
generate vector of filenames

## Usage

``` r
datadir2fnames(datadir,filelist)
```

## Arguments

- datadir:

  See [g.part1](https://wadpac.github.io/GGIR/reference/g.part1.md)

- filelist:

  Produced by
  [isfilelist](https://wadpac.github.io/GGIR/reference/isfilelist.md)

## Value

Character vector of filenames

## Author

Vincent T van Hees \<v.vanhees@accelting.com\>

## Examples

``` r
  if (FALSE) { # \dontrun{
  datadir2fnames(datadir = "C:/mydatafolder",filelist=TRUE)
  } # }
```

# Checks whether datadir is a directory or a vector with filenames

Checks whether argument datadir used in various other functions in GGIR
is the name of a directory that includes data files or whether it is a
vector with the full paths to one or more data files

## Usage

``` r
isfilelist(datadir)
```

## Arguments

- datadir:

  Argument datadir as used in various other functions in GGIR

## Value

Boolean whether it is a list of files (TRUE) or not (FALSE)

## Examples

``` r
if (FALSE) { # \dontrun{
isitafilelist = isfilelist(datadir)
} # }
```

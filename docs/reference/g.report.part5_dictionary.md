# Generate data dictionary for reports from milestone data produced by [g.part5](https://wadpac.github.io/GGIR/reference/g.part5.md)

Creates a data dictionary with the definitions of the outcomes exported
in the reports from milestone data produced by
[g.part5](https://wadpac.github.io/GGIR/reference/g.part5.md). Not
intended for direct use by package user.

## Usage

``` r
g.report.part5_dictionary(metadatadir, params_output)
```

## Arguments

- metadatadir:

  Directory that holds a folder 'meta' and inside this a folder 'basic'
  which contains the milestone data produced by
  [g.part1](https://wadpac.github.io/GGIR/reference/g.part1.md). The
  folderstructure is normally created by
  [g.part1](https://wadpac.github.io/GGIR/reference/g.part1.md) and
  [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md) will recognise
  what the value of metadatadir is.

- params_output:

  Parameters object, see
  [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md)

## Value

Function does not produce data, but only writes data dictionaries for
the reports in csv format

The following files are stored in the root of the results folder:
part5_dictionary_daysummary\_\* part5_dictionary_personsummary\_\*

## Author

Vincent T van Hees \<v.vanhees@accelting.com\> Jairo Hidalgo Migueles
\<jairo@jhmigueles.com\>

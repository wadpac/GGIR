# Extracts folderstructure based on data directory.

Extracts folderstructure based on data directory. This is used when
accelerometer files are stored in a hierarchical folder structure and
the user likes to have a reference to the exact position in the folder
tree, rather than just the filename. Function not intended for direct
use by package user.

## Usage

``` r
getfolderstructure(datadir=c(),referencefnames=c())
```

## Arguments

- datadir:

  Argument datadir as used in various other functions in GGIR

- referencefnames:

  vector with filename to filter on

## Value

List with items:

- fullfilenames: vector with all full paths to the folders including the
  name of the file itself

- foldername: vector with only the names of the folder in which each
  file is stroed (so only the most distal folder in the folder tree).

## Examples

``` r
  if (FALSE) { # \dontrun{
    folderstructure = getfolderstructure(datadir)
  } # }
```

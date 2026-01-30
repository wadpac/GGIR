# Generic functiont to read large blocks of accelerometer data

The function is used by
[g.getmeta](https://wadpac.github.io/GGIR/reference/g.getmeta.md) and
[g.calibrate](https://wadpac.github.io/GGIR/reference/g.calibrate.md) to
read large blocks of the accelerometer file, which are processed and
then deleted from memory. This is needed for memory management.

## Usage

``` r
g.readaccfile(filename, blocksize, blocknumber, filequality,
                           ws, PreviousEndPage = 1, inspectfileobject = c(),
                           PreviousLastValue = c(0,0,1), PreviousLastTime = NULL,
                           params_rawdata = c(), params_general = c(), header = NULL, ...)
```

## Arguments

- filename:

  filename

- blocksize:

  Size of blocks (in file pages) to be read

- blocknumber:

  Block number relative to start of file, starting with 1.

- filequality:

  Single row dataframe with columns: filetooshort, filecorrupt, and
  filedoesnotholdday. All with the value TRUE or FALSE

- ws:

  Larger windowsize for non-detection, see documentation
  [g.part2](https://wadpac.github.io/GGIR/reference/g.part2.md)

- PreviousEndPage:

  Page number on which previous block ended (automatically assigned
  within g.getmeta and g.calibrate).

- inspectfileobject:

  Output from the function
  [g.inspectfile](https://wadpac.github.io/GGIR/reference/g.inspectfile.md).

- PreviousLastValue:

  Automatically identified last value in previous chunk of data read.

- PreviousLastTime:

  Automatically identified last timestamp in previous chunk of data
  read.

- params_rawdata:

  See [g.part1](https://wadpac.github.io/GGIR/reference/g.part1.md)

- params_general:

  See [g.part1](https://wadpac.github.io/GGIR/reference/g.part1.md)

- header:

  Header information that was extracted the previous time this file was
  read, to be re-used instead of being extracted again.

- ...:

  Any input arguments needed for function
  [read.myacc.csv](https://wadpac.github.io/GGIR/reference/read.myacc.csv.md)
  if you are working with a non-standard csv formatted files. Furter,
  any argument used in the previous version of g.readaccfile, which will
  now be used to overrule the arguments specified with the parameter
  objects.

## Value

- `P` Block object extracted from file with format specific to
  accelerometer brand

- `filequality` Same as in function arguments

- `isLastBlock` Boolean indicating whether this was the last block to
  read

- `endpage` Page number on which blocked ends, this will be used as
  input for argument PreviousEndPage when reading the next block.

## Author

Vincent T van Hees \<v.vanhees@accelting.com\>

## Examples

``` r
  if (FALSE) { # \dontrun{
    filequality = data.frame(filetooshort = FALSE, filecorrupt = FALSE,
    filedoesnotholdday = FALSE)
    output = g.readaccfile(filename = "C:/myfile.bin", 
    blocksize = 20000, blocknumber = 1,
    selectdaysfile = c(), filequality = filequality,
    dayborder = 0, PreviousEndPage = c()) 
  } # }
```

# Update blocksize of data to be read depending on available memory.

Function queries available memory to either lower or increase the
blocksize used by function
[g.readaccfile](https://wadpac.github.io/GGIR/reference/g.readaccfile.md)

## Usage

``` r
updateBlocksize(blocksize, bsc_qc)
```

## Arguments

- blocksize:

  Number of filepages (binary data) or rows (other dataformats).

- bsc_qc:

  Data.frame with columns time (timestamp from Sys.time) and size
  (memory size). This is used for housekeeping in
  [g.calibrate](https://wadpac.github.io/GGIR/reference/g.calibrate.md)
  and [g.getmeta](https://wadpac.github.io/GGIR/reference/g.getmeta.md)

## Value

List with blocksize and bsc_qc, same format as input, although bsc_qc
has one new row.

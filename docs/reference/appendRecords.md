# Append GGIR milestone data from neighbouring or overlapping recordings

Append GGIR part 1 milestone data format from neighbouring or
overlapping recording. When recordings overlap we use data from newest
recordings. When time gap is larger than 2 days the recordings are not
appended. Not intended for direct use by the user.

## Usage

``` r
appendRecords(metadatadir, desiredtz = "", idloc = 1, maxRecordingInterval = NULL)
```

## Arguments

- metadatadir:

  See [g.part2](https://wadpac.github.io/GGIR/reference/g.part2.md)

- desiredtz:

  See [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md)

- idloc:

  See [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md)

- maxRecordingInterval:

  See [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md)

## Author

Vincent T van Hees \<v.vanhees@accelting.com\>

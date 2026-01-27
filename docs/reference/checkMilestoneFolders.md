# Checks for existence of folders to process

Checks whether milestone folders exist, create them if needed, and check
whether folders are not empty. Only done for part 1 to 5 and not part 6,
which is different and handled inside
[g.part6](https://wadpac.github.io/GGIR/reference/g.part6.md).

## Usage

``` r
checkMilestoneFolders(metadatadir, partNumber)
```

## Arguments

- metadatadir:

  Character, path to root of outputfolder.

- partNumber:

  Numeric, number from the set 2, 3, 4 or 5.

## Value

No value is produced

## Author

Vincent T van Hees \<v.vanhees@accelting.com\>

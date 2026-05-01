# Fix missing night in part 4 output

Not intended for direct use by GGIR users. If a night is missing in the
part4 output then this function tries to fix as part of
[g.part5](https://wadpac.github.io/GGIR/reference/g.part5.md).

## Usage

``` r
g.part5.fixmissingnight(summarysleep, sleeplog = c(), ID)
```

## Arguments

- summarysleep:

  Object produced by
  [g.part4](https://wadpac.github.io/GGIR/reference/g.part4.md)

- sleeplog:

  Sleeplog object as passed on from
  [g.part5](https://wadpac.github.io/GGIR/reference/g.part5.md)

- ID:

  ID of participant

## Value

Corrected summarysleep_tmp2 object.

## Author

Vincent T van Hees \<v.vanhees@accelting.com\>

# Identifies levels of behaviour for g.part5 function.

Identifies levels of behaviour from acceleratioon and sustained
inactivity sibdetection (using angles). Function not intended for direct
use by package user.

## Usage

``` r
identify_levels(ts, TRLi,TRMi,TRVi,
                  ws3, params_phyact, ...)
```

## Arguments

- ts:

  Data.frame with time series genrated in .gpart5

- TRLi:

  Numeric acceleration threshold light

- TRMi:

  Numeric acceleration threshold moderate

- TRVi:

  Numeric acceleration threshold vigorous

- ws3:

  Numeric size of epoch in seconds

- params_phyact:

  See [g.part2](https://wadpac.github.io/GGIR/reference/g.part2.md)

- ...:

  Any argument used in the previous version of identify_level, which
  will now be used to overrule the arguments specified with the
  parameter objects.

## Value

List with items:

- LEVELS

- OLEVELS

- Lnames

- bc.mvpa

- bc.lig

- bc.in

- ts

## Author

Vincent T van Hees \<v.vanhees@accelting.com\>

## Examples

``` r
  if (FALSE) { # \dontrun{
    levels = identify_levels(TRLi,TRMi,TRVi,
                               boutdur.mvpa,boutcriter.mvpa,
                               boutdur.lig,boutcriter.lig,
                               boutdur.in,boutcriter.in,
                               ws3,bout.metric)
  } # }
```

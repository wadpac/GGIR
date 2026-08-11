# Calculates Sleep Regularity Index

Calculates Sleep Regularity Index per day pair proposed by Phillips and
colleagues in 2017 expanded with day-pair level estimates.

## Usage

``` r
CalcSleepRegularityIndex(data = c(), epochsize = c(), desiredtz= c(),
                            SRI1_smoothing_wsize_hrs = NULL,
                            SRI1_smoothing_frac = NULL)
```

## Arguments

- data:

  Data.frame produced by function
  [g.sib.det](https://wadpac.github.io/GGIR/reference/g.sib.det.md).

- epochsize:

  Numeric value of epoch size in seconds.

- desiredtz:

  See [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md).

- SRI1_smoothing_wsize_hrs:

  See [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md).

- SRI1_smoothing_frac:

  See [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md).

## Value

Data.frame with columns: day (day number); Sleep Regularity Index, which
by definition must lie in the range -100 (reversed regularity), to 0
(random pattern), to 100 (perfect regularity); weekday (e.g. Wednesday);
frac_valid, number between 0 and 1 indicating the fraction of the 24
hour period for which valid data was available in both the current and
the next day, and; date.

## Details

Calculates Sleep Regularity Index per day pair. Absense of missing data
is not used as a criteria for calculation. Instead the code asses the
fraction of the time for which matching valid data points were found in
both days. Later in g.part4 this fraction is used to include or exclude
days based on the excludenightcrit criteria it also uses for the other
sleep variables. In g.report.part4 these day-level SRI values are
stored, but also aggregated across all recording days, all weekend days,
and all weekend days, respectively. Therefore, this function is broader
in functionality than the algorithm proposed by Phillips and colleagues
in 2017.

## Author

Vincent T van Hees \<v.vanhees@accelting.com\>

## References

- Andrew J. K. Phillips, William M. Clerx, et al. Irregular sleep/wake
  patterns are associated with poorer academic performance and delayed
  circadian and sleep/wake timing. Scientific Reports. 2017 June 12

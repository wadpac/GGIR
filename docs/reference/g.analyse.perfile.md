# Function supports [g.analyse](https://wadpac.github.io/GGIR/reference/g.analyse.md). Not intended for direct use by user.

Generates recording specific analyses and fills corresponding output
matrix,
[g.analyse](https://wadpac.github.io/GGIR/reference/g.analyse.md).

## Usage

``` r
g.analyse.perfile(I, C, metrics_nav,
                 AveAccAve24hr, doquan, doiglevels, tooshort,
                 params_247, params_cleaning, params_general,
                 output_avday, output_perday,
                 dataqual_summary, file_summary)
```

## Arguments

- I:

  output
  [g.inspectfile](https://wadpac.github.io/GGIR/reference/g.inspectfile.md)

- C:

  output
  [g.calibrate](https://wadpac.github.io/GGIR/reference/g.calibrate.md)

- metrics_nav:

  List with three objects to help navigate the acceleration metrics

- AveAccAve24hr:

  Average acceleration in an average 24 hour cycle

- doquan:

  Boolean whether quantile analysis should be done

- doiglevels:

  Boolean to indicate whether iglevels should be calculated

- tooshort:

  0 (file not too short) or 1 (file too short)

- params_247:

  see [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md)

- params_cleaning:

  see [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md)

- params_general:

  see [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md)

- output_avday:

  Output from g.analyse.avday

- output_perday:

  Output from g.analyse.perday

- dataqual_summary:

  Data.frame with data quality summary indicators produced in
  [g.analyse](https://wadpac.github.io/GGIR/reference/g.analyse.md)

## Value

- `filesummary`:

  summary for the file that was analysed

- `daysummary`:

  Summary per day for the file that was analysed

## Author

Vincent T van Hees \<v.vanhees@accelting.com\>

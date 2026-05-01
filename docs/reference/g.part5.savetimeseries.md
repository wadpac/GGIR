# Saves part 5 time series to csv files

Not intended for direct use by GGIR users. Saves part 5 time series to
csv files as part of
[g.part5](https://wadpac.github.io/GGIR/reference/g.part5.md).

## Usage

``` r
g.part5.savetimeseries(ts, LEVELS, desiredtz, rawlevels_fname,
                            DaCleanFile = NULL,
                            includedaycrit.part5 = 2/3,
                            includenightcrit.part5 = 0,
                            ID = NULL,
                            params_output,
                            params_247 = NULL,
                            Lnames = NULL, timewindow = NULL,
                            filename = "")
```

## Arguments

- ts:

  Data.frame with time series

- LEVELS:

  As produced as one of the objects in the output of
  [identify_levels](https://wadpac.github.io/GGIR/reference/identify_levels.md)

- desiredtz:

  See [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md).

- rawlevels_fname:

  Path to the file where the output (time series) should be stored

- DaCleanFile:

  Content of data_cleaning_file as documented in
  [g.report.part5](https://wadpac.github.io/GGIR/reference/g.report.part5.md).
  Only used in this function if save_ms5rawlevels is TRUE, and it only
  affects the time series files stored.

- includedaycrit.part5:

  See [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md). Only used
  in this function if save_ms5rawlevels is TRUE, and it only affects the
  time series files stored.

- includenightcrit.part5:

  See [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md). Only used
  in this function if save_ms5rawlevels is TRUE, and it only affects the
  time series files stored.

- ID:

  If data_cleaning_file is used then this argument specifies which
  participant ID the data correspond with.

- params_output:

  Parameters object, see
  [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md)

- params_247:

  See [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md)

- Lnames:

  Level names as passed on from
  [identify_levels](https://wadpac.github.io/GGIR/reference/identify_levels.md),
  these are the names corresponding the ID of the behavioural classes as
  stored in column class_id.

- timewindow:

  See [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md)

- filename:

  Character (default = "") indicating the name of the accelerometer data
  file that was used as input. This name will be stored inside the time
  series output file.

## Value

Function does not provide output, it only prepare data for saving and
saves it to a file. For documention on columns see main vignette.

## Author

Vincent T van Hees \<v.vanhees@accelting.com\>

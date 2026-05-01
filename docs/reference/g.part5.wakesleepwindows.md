# Label wake and sleepperiod window

Not intended for direct use by GGIR users. Label wake and sleepperiod
window as part of
[g.part5](https://wadpac.github.io/GGIR/reference/g.part5.md).

## Usage

``` r
g.part5.wakesleepwindows(ts, part4_output, desiredtz,
                    nightsi, sleeplog, epochSize,ID, Nepochsinhour)
```

## Arguments

- ts:

  data.frame with time series

- part4_output:

  cleaned output from part 4

- desiredtz:

  [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md)

- nightsi:

  vector with indices for the midnights

- sleeplog:

  Data.frame with sleeplog information as loaded by
  [g.loadlog](https://wadpac.github.io/GGIR/reference/g.loadlog.md)

- epochSize:

  Short epochsize in seconds

- ID:

  ID of the participant

- Nepochsinhour:

  Number of epochs in an hour

## Value

Object ts

## Author

Vincent T van Hees \<v.vanhees@accelting.com\>

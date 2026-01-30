# Correct guider estimates

After guider has been derived for all nights in the recording this
function is optionally applied to correct individual nights based on
knowledge about other nights in the recording.

## Usage

``` r
g.part3_correct_guider(SLE, desiredtz, epochSize,
                    params_sleep)
```

## Arguments

- SLE:

  Object extracted with
  [g.sib.det](https://wadpac.github.io/GGIR/reference/g.sib.det.md)

- desiredtz:

  See [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md)

- epochSize:

  First value of parameter `windowsizes` as discussed in See
  [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md).

- params_sleep:

  Complete params_sleep object, see
  [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md)

## Author

Vincent T van Hees \<v.vanhees@accelting.com\>

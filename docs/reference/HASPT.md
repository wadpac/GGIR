# Heuristic Algorithms estimating SPT window.

As used in function
[g.sib.det](https://wadpac.github.io/GGIR/reference/g.sib.det.md).
Function is not intended for direct use by GGIR user.

## Usage

``` r
HASPT(angle, params_sleep = NULL, ws3 = 5, 
      HASPT.algo="HDCZA", invalid, activity = NULL,
      marker = NULL, sibs = NULL)
```

## Arguments

- angle:

  Vector of epoch level estimates of angle

- params_sleep:

  See [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md)

- ws3:

  Number representing epoch length in seconds

- HASPT.algo:

  See [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md)

- invalid:

  Integer vector with per epoch an indicator of valid(=0) or invalid(=1)
  epoch.

- activity:

  Numeric vector with magnitude of acceleration per epoch, only used
  when HASPT.algo is set to NotWorn. Acceleration metric used is
  specified by argument `acc.metric` elsewhere in GGIR.

- marker:

  Optional, numeric vector with for each epoch an indication of whether
  the marker button was pressed (1) or not (0). Only used for
  Actiwatch-like devices such as MotionWare.

- sibs:

  Numeric vector of classified sibs with `HASIB`, experimental not used
  at the moment.

## Value

List with start and end times of the SPT window and the threshold as
used.

## Author

Vincent T van Hees \<v.vanhees@accelting.com\>

# Extract metrics from acceleration signals

Function to extract metrics from acceleration signal. Not intended for
direct use by user

## Usage

``` r
g.applymetrics(data, sf, ws3, metrics2do,
                          n = 4, lb = 0.2, hb = 15,
                          zc.lb = 0.25, zc.hb = 3, 
                          zc.sb = 0.01, zc.order = 2,
                          actilife_LFE = FALSE)
```

## Arguments

- data:

  Three column matrix with x, y, and z acceleration data

- n:

  filter order, see
  [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md) for details

- sf:

  sample frequency

- ws3:

  Epoch size in seconds

- metrics2do:

  Dataframe with Boolean indicator for all metrics whether they should
  be extracted or not. For instance, metrics2do\$do.bfen = TRUE,
  indicates that the bfen metric should be extracted

- lb:

  Lower boundery of cut-off frequencies, see
  [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md).

- hb:

  Higher boundery of cut-off frequencies, see
  [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md).

- zc.lb:

  See [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md)

- zc.hb:

  See [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md)

- zc.sb:

  See [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md)

- zc.order:

  See [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md)

- actilife_LFE:

  See [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md)

## Value

Dataframe with metric values in columns average per epoch (ws3)

## Author

Vincent T van Hees \<v.vanhees@accelting.com\>

## Examples

``` r
  Gx = runif(n=10000,min=0,max=2)
  Gy = runif(n=10000,min=1,max=3)
  Gz = runif(n=10000,min=0,max=2)
  data = cbind(Gx, Gy, Gz)
  colnames(data) = c("x", "y", "z")
  metrics2do = data.frame(do.bfen=TRUE,do.enmo=TRUE,do.lfenmo=FALSE,
  do.en=FALSE,do.hfen=FALSE,do.hfenplus=FALSE,do.mad=FALSE,do.anglex=FALSE,
  do.angley=FALSE,do.anglez=FALSE,do.roll_med_acc_x=FALSE,
  do.roll_med_acc_y=FALSE,do.roll_med_acc_z=FALSE,
  do.dev_roll_med_acc_x=FALSE,do.dev_roll_med_acc_y=FALSE,
  do.dev_roll_med_acc_z=FALSE,do.enmoa=FALSE,
  do.lfx=FALSE, do.lfy=FALSE, do.lfz=FALSE, 
  do.hfx=FALSE, do.hfy=FALSE, do.hfz=FALSE, 
  do.bfx=FALSE, do.bfy=FALSE, do.bfz=FALSE,
  do.zcx=FALSE, do.zcy=FALSE, do.zcz=FALSE, 
  do.brondcounts=FALSE, do.neishabouricounts=FALSE)
  
  extractedmetrics = g.applymetrics(data,n=4,sf=40,ws3=5,metrics2do)
```

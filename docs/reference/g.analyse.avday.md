# Function supports [g.analyse](https://wadpac.github.io/GGIR/reference/g.analyse.md). Not intended for direct use by user.

Generatess average day analyses and fills corresponding output matrix,
[g.analyse](https://wadpac.github.io/GGIR/reference/g.analyse.md).

## Usage

``` r
g.analyse.avday(doquan, averageday, M, IMP, t_TWDI, quantiletype,
                   ws3, doiglevels, firstmidnighti, ws2, midnightsi,
                   params_247 = c(), qcheck = c(), acc.metric = c(),
                   params_phyact = NULL, ...)
```

## Arguments

- doquan:

  Boolean whether quantile analysis should be done

- averageday:

  As produced by
  [g.impute](https://wadpac.github.io/GGIR/reference/g.impute.md)

- M:

  As produced by
  [g.getmeta](https://wadpac.github.io/GGIR/reference/g.getmeta.md)

- IMP:

  As produced by
  [g.impute](https://wadpac.github.io/GGIR/reference/g.impute.md)

- t_TWDI:

  Same as qwindow as described in
  [g.analyse](https://wadpac.github.io/GGIR/reference/g.analyse.md)

- quantiletype:

  see [g.analyse](https://wadpac.github.io/GGIR/reference/g.analyse.md)

- ws3:

  Epoch size in seconds

- doiglevels:

  Boolean to indicate whether iglevels should be calculated

- firstmidnighti:

  see
  [g.detecmidnight](https://wadpac.github.io/GGIR/reference/g.detecmidnight.md)

- ws2:

  see [g.weardec](https://wadpac.github.io/GGIR/reference/g.weardec.md)

- midnightsi:

  see
  [g.detecmidnight](https://wadpac.github.io/GGIR/reference/g.detecmidnight.md)

- params_247:

  See [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md)

- qcheck:

  Vector with indicators of when data is valid (value=0) or invalid
  (value=1).

- acc.metric:

  Character, see
  [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md). Here, it is
  used to decided which acceleration metric to use for IVIS and cosinor
  analyses.

- params_phyact:

  See [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md)

- ...:

  Any argument used in the previous version of g.analyse.avday, which
  will now be used to overrule the arguments specified with the
  parameter objects.

## Value

- `igfullr_names`:

  Intensity gradient variable names

- `igfullr`:

  Intensity gradient values

- `QUAN`:

  Quantiles

- `qlevels_names`:

  Quantile level names

- `ML5AD`:

- `ML5AD_names`:

## Author

Vincent T van Hees \<v.vanhees@accelting.com\>

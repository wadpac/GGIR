# Heuristic algorithms for sustiained inactivty bouts detection

Apply heuristic algorithms for sustiained inactivty bouts detection.
Function not intended for direct use by package user

## Usage

``` r
HASIB(HASIB.algo = "vanHees2015", timethreshold = c(), anglethreshold = c(), 
                 time = c(), anglez = c(), ws3 = c(), zeroCrossingCount = c(),
                 NeishabouriCount = c(), activity = NULL,
                 oakley_threshold = NULL)
```

## Arguments

- HASIB.algo:

  Character to indicator which sib algorithm should be used. Default
  value: "vanHees2015". Other options: "Sadeh1994", "Galland2012",
  "ColeKripke1992"

- anglethreshold:

  See [g.sib.det](https://wadpac.github.io/GGIR/reference/g.sib.det.md)

- timethreshold:

  See [g.sib.det](https://wadpac.github.io/GGIR/reference/g.sib.det.md)

- time:

  Vector with time per short epoch

- anglez:

  Vector with z-angle per short epoch

- ws3:

  See [g.getmeta](https://wadpac.github.io/GGIR/reference/g.getmeta.md)

- zeroCrossingCount:

  Vector with zero crossing counts per epoch as required for count-based
  algorithms

- NeishabouriCount:

  Vector with Neishabouri counts per epoch to be used by the count-based
  algorithms

- activity:

  Magnitude of acceleration, only used when HASIB.algo is set to NotWorn
  or Oakley1997. Acceleration metric used is specified by argument
  `acc.metric` elsewhere in GGIR.

- oakley_threshold:

  See [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md)

## Value

Vector with binary indicator of sustained inactivity bout, 1 is yes, 0
is no.

## Author

Vincent T van Hees \<v.vanhees@accelting.com\>

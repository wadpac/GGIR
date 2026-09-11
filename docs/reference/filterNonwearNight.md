# Filters short lasting nonwear during the night

Filters short lasting nonwear during the night. Not intended for direct
use by GGIR user.

## Usage

``` r
filterNonwearNight(r1, metalong, qwindowImp, desiredtz, params_cleaning, ws2)
```

## Arguments

- r1:

  Object r1 as created inside function
  [g.weardec](https://wadpac.github.io/GGIR/reference/g.weardec.md)
  which is a vector of zeros and ones derived from metalong where 1
  indicates nonwear and 0 wear.

- metalong:

  Object metalong created in
  [g.part1](https://wadpac.github.io/GGIR/reference/g.part1.md)

- qwindowImp:

  See [g.impute](https://wadpac.github.io/GGIR/reference/g.impute.md)

- desiredtz:

  See [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md)

- params_cleaning:

  Parameters object with cleaning paramete, see
  [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md).

- ws2:

  Object ws2 which equals the second value of vector parameter
  `windowsizes`

## Value

r1 object with short lasting nonwear during the night removed

## Author

Vincent T van Hees \<v.vanhees@accelting.com\>

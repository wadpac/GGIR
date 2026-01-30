# Detect all midnights in a time series

Detect all midnights in a time series

## Usage

``` r
g.detecmidnight(time,desiredtz, dayborder)
```

## Arguments

- time:

  Vector of timestamps, either in iso8601 or in POSIX format

- desiredtz:

  See [g.part2](https://wadpac.github.io/GGIR/reference/g.part2.md)

- dayborder:

  see [g.analyse](https://wadpac.github.io/GGIR/reference/g.analyse.md)

## Value

Output of the function is list containing the following objects:  

- firstmidnight = timestamp of first midnight

- firstmidnighti = index of first midnight

- lastmidnight = timestamp of last midnight

- lastmidnighti = index of last midnight

- midnights = timestamps of midnights

- midnightsi = indeces of midnights

## Author

Vincent T van Hees \<v.vanhees@accelting.com\>

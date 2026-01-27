# Convert POSIX to iso8601 timestamp

To avoid ambiguities when sharing and comparing timestamps. All
timestamps are expressed in iso8601 format:
https://en.wikipedia.org/wiki/ISO_8601

## Usage

``` r
POSIXtime2iso8601(x,tz)
```

## Arguments

- x:

  Vector of timestamps in POSIX format

- tz:

  Timezone of data collection, e.g. "Europe/London". See
  https://en.wikipedia.org/wiki/List_of_tz_database_time_zones for full
  list

## Author

Vincent T van Hees \<v.vanhees@accelting.com\>

## Examples

``` r
if (FALSE) { # \dontrun{
x ="2017-05-07 13:15:17 CEST"
tz = "Europe/Amsterdam"
x_converted = POSIXtime2iso8601(x,tz)
} # }
```

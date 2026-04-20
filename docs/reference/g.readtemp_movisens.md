# Reads the temperature from movisens files.

Reads the temperature from movisens files, resamples it and adds it to
the matrix where accelerations are stored

## Usage

``` r
g.readtemp_movisens(datafile, from = c(), to = c(), acc_sf, acc_length,
                    interpolationType=1)
```

## Arguments

- datafile:

  Full path to the folder where the movisens bin files are stored. Note
  that movisens store a set of bin file in one folder per recording.
  GGIR will read the pertinent bin file to access to the temperature
  data.

- from:

  Origin point to derive the temperature from movisens files
  (automatically calculated by GGIR)

- to:

  End point to derive the temperature from movisens files (automatically
  calculated by GGIR)

- acc_sf:

  Sample frequency of acceleration data

- acc_length:

  number of acceleration data samples

- interpolationType:

  Integer to indicate type of interpolation to be used when resampling
  time series (mainly relevant for Axivity sensors), 1=linear, 2=nearest
  neighbour.

## Value

Data matrix with the temperature values resampled at 64 Hz.

## Examples

``` r
if (FALSE) { # \dontrun{
  P = g.readtemp_movisens(datafile, from = c(), to = c(), acc_sf = 64, acc_length = 3000)
} # }
```

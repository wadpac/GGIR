# Extracts ID from filename and finds matching rows in sleeplog

Extracts ID from filename and finds matching rows in sleeplog. Function
not designed for direct use by GGIR users.

## Usage

``` r
g.part4_extractid(idloc, fname, dolog, sleeplog, accid = c())
```

## Arguments

- idloc:

  See [g.part4](https://wadpac.github.io/GGIR/reference/g.part4.md)

- fname:

  Full patth to filename

- dolog:

  Boolean to indicate whether to rely on a sleeplog

- sleeplog:

  Sleeplog data.frame passed on from g.part4

- accid:

  ID extracted from the acceleration file in GGIR part3. If not
  available leave blank.

## Value

List with accid the ID and matching_indices_sleeplog a vector with
matching row indices in the sleeplog

## Author

Vincent T van Hees \<v.vanhees@accelting.com\>

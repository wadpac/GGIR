# Checks myfun object before it is passed to applyExtfunction

Checks that object myfun is a list and check the elements of the list
for: that element names are as expected, that value of each element is
of the expected type and length.

## Usage

``` r
check_myfun(myfun, windowsizes)
```

## Arguments

- myfun:

  See
  [applyExtFunction](https://wadpac.github.io/GGIR/reference/applyExtFunction.md)

- windowsizes:

  See
  [g.getmeta](https://wadpac.github.io/GGIR/reference/g.getmeta.md)).

## Value

0 if all checkes passed, 1 if one or more checks did not pass. Error
message are printed to the console with feedback on which checks did not
pass.

## Author

Vincent T van Hees \<v.vanhees@accelting.com\>

# Wrapper function around function GGIR

This function used to be the central function in the package, but has
been renamed GGIR. You can still use function call g.shell.GGIR but all
arguments will be passed on to function GGIR. We have done this to
preserve consistency with older use cases of the GGIR package. All
documentation can now be found in
[GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md).

## Usage

``` r
g.shell.GGIR(...)
```

## Arguments

- ...:

  Any of the parameters used by
  [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md).

## Value

The function provides no values, it only ensures that other functions
are called and that their output is stored. See
[GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md).

## Author

Vincent T van Hees \<v.vanhees@accelting.com\>

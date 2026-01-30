# Builds Section for Parameters Vignette

Function extracts the documentation for a given GGIR argument as
provided in the [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md)
documentation and builds the structure for the Parameters Vignette.
Function not designed for direct use by package user.

## Usage

``` r
parametersVignette(params = "sleep")
```

## Arguments

- params:

  Character (default = "sleep"). Name of the parameters object to build
  its corresponding section in the Parameters vignette.

## Value

Structure for the vignette subsection.

## Author

Jairo Hidalgo Migueles \<jairo.hidalgo.migueles@gmail.com\>

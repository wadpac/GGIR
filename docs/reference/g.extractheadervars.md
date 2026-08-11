# Extracts header variables from header object

Function is not intended for direct interaction by package end user

## Usage

``` r
g.extractheadervars(I)
```

## Arguments

- I:

  Object produced by
  [g.inspectfile](https://wadpac.github.io/GGIR/reference/g.inspectfile.md)

## Value

- ID = participant identifier

- iid = investigator identifier

- HN = handedness

- BodyLocation = Attachement location of the sensor

- SX = sex

- deviceSerialNumber = serial number

## Author

Vincent T van Hees \<v.vanhees@accelting.com\>

## Examples

``` r
data(data.inspectfile)
headervars = g.extractheadervars(I=data.inspectfile)
```

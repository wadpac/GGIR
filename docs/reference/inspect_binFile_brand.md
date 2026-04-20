# Identify the Device Brand from a Binary File

This function identifies the brand of a device (e.g., GENEActiv or
MATRIX) based on the contents of a binary file. If the file is
unrecognized, it returns a default value indicating the device is not
recognized.

## Usage

``` r
inspect_binFile_brand(filename)
```

## Arguments

- filename:

  The path to the binary file to be inspected.

## Details

The function performs the following steps:

1.  Reads the file header to check for the presence of a `"Device Type"`
    field.

2.  If the device type contains the string `"GENEActiv"`, the device is
    identified as a GENEActiv device.

3.  If the `"Device Type"` field is not found, it checks for a MATRIX
    device by validating a specific header value (`"MDTC"`).

4.  If neither device type is detected, the function returns
    `"not_recognised"`.

## Value

A character string representing the detected device brand:

- `2`: Indicates the device is a GENEActiv device.

- `7`: Indicates the device is a MATRIX device.

- `"not_recognised"`: Indicates the file's brand is not recognized.

## Author

Jairo H Migueles \<jairo@jhmigueles.com\>

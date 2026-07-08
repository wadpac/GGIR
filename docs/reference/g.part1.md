# function to load and pre-process acceleration files

Calls function
[g.getmeta](https://wadpac.github.io/GGIR/reference/g.getmeta.md) and
[g.calibrate](https://wadpac.github.io/GGIR/reference/g.calibrate.md),
and converts the output to .RData-format which will be the input for
[g.part2](https://wadpac.github.io/GGIR/reference/g.part2.md). Here, the
function generates a folder structure to keep track of various output
files. The reason why these g.part1 and
[g.part2](https://wadpac.github.io/GGIR/reference/g.part2.md) are not
merged as one generic shell function is because g.part1 takes much
longer to and involves only minor decisions of interest to the movement
scientist. Function g.part2 on the other hand is relatively fast and
comes with all the decisions that directly impact on the variables that
are of interest to the movement scientist. Therefore, the user may want
to run g.part1 overnight or on a computing cluster, while g.part2 can
then be the main playing ground for the movement scientist. Function
[GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md) provides the
main shell that allows for operating g.part1 and g.part2.

## Usage

``` r
g.part1(datadir = c(), metadatadir = c(), f0 = 1, f1 = c(),
          myfun = c(), params_metrics = c(), params_rawdata = c(),
          params_cleaning = c(), params_general = c(), verbose = TRUE, ...)
```

## Arguments

- datadir:

  Directory where the accelerometer files are stored, e.g. "C:/mydata",
  or list of accelerometer filenames and directories, e.g.
  c("C:/mydata/myfile1.bin", "C:/mydata/myfile2.bin").

- metadatadir:

  Directory where the output needs to be stored. Note that this function
  will attempt to create folders in this directory and uses those folder
  to keep output.

- f0:

  File index to start with (default = 1). Index refers to the filenames
  sorted in alphabetical order

- f1:

  File index to finish with (defaults to number of files available,
  i.e., f1 = 0)

- myfun:

  External function object to be applied to raw data. See details
  [applyExtFunction](https://wadpac.github.io/GGIR/reference/applyExtFunction.md).

- params_metrics:

  See details in
  [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md).

- params_rawdata:

  See details in
  [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md).

- params_cleaning:

  See details in
  [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md).

- params_general:

  See details in
  [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md).

- verbose:

  See details in
  [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md).

- ...:

  If you are working with a non-standard csv formatted files, g.part1
  also takes any input arguments needed for function
  [read.myacc.csv](https://wadpac.github.io/GGIR/reference/read.myacc.csv.md)
  and argument rmc.noise from
  [get_nw_clip_block_params](https://wadpac.github.io/GGIR/reference/get_nw_clip_block_params.md).
  First test these argument with function
  [read.myacc.csv](https://wadpac.github.io/GGIR/reference/read.myacc.csv.md)
  directly. To ensure compatibility with R scripts written for older
  GGIR versions, the user can also provide parameters listed in the
  params\_ objects as direct argument.

## Details

GGIR comes with many processing parameters, which have been thematically
grouped in parameter objects (R list). By running print(load_params())
you can see the default values of all the parameter objects. When g.part
1 is used via [GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md)
you have the option to specifiy a configuration file, which will
overrule the default parameter values. Further, as user you can set
parameter values as input argument to both g.part1 and
[GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md). Directly
specified argument overrule the configuration file and default values.

See the GGIR package vignette or the details section in
[GGIR](https://wadpac.github.io/GGIR/reference/GGIR.md) for a more
elaborate overview of parameter objects and their usage across GGIR.

## Value

The function provides no values, it only ensures that the output from
other functions is stored in .RData(one file per accelerometer file) in
folder structure

## Author

Vincent T van Hees \<v.vanhees@accelting.com\>

## References

- van Hees VT, Gorzelniak L, Dean Leon EC, Eder M, Pias M, et al. (2013)
  Separating Movement and Gravity Components in an Acceleration Signal
  and Implications for the Assessment of Human Daily Physical Activity.
  PLoS ONE 8(4): e61691. doi:10.1371/journal.pone.0061691

- van Hees VT, Fang Z, Langford J, Assah F, Mohammad A, da Silva IC,
  Trenell MI, White T, Wareham NJ, Brage S. Auto-calibration of
  accelerometer data for free-living physical activity assessment using
  local gravity and temperature: an evaluation on four continents. J
  Appl Physiol (1985). 2014 Aug 7

- Aittasalo M, Vaha-Ypya H, Vasankari T, Husu P, Jussila AM, and
  Sievanen H. Mean amplitude deviation calculated from raw acceleration
  data: a novel method for classifying the intensity of adolescents
  physical activity irrespective of accelerometer brand. BMC Sports
  Science, Medicine and Rehabilitation (2015).

## Examples

``` r
  if (FALSE) { # \dontrun{
    datafile = "C:/myfolder/mydata"
    outputdir = "C:/myresults"
    g.part1(datadir,outputdir)
  } # }
```

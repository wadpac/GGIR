# Extract parameters from input and add them to params

Extracts parameters separately provided by input and adds them to the
params objects. Not intended for direct use by GGIR users.

## Usage

``` r
extract_params(params_sleep = c(), params_metrics = c(),
                 params_rawdata = c(), params_247 = c(),
                 params_phyact = c(), params_cleaning = c(),
                 params_output = c(), params_general = c(), input = c(),
                 configfile_csv = c(), params2check = c("sleep", "metrics",
                 "rawdata", "247", "phyact",
                                           "cleaning", "output", "general"))
```

## Arguments

- params_sleep:

  List with sleep parameters

- params_metrics:

  List with parameters related to metrics

- params_rawdata:

  List with parameters related to raw data reading and processing

- params_247:

  List with parameters related to 24/7 behavioural analysis, which
  includes anything that does not fit with physical activity or sleep
  research

- params_phyact:

  List with parameters related to physical activity analysis

- params_cleaning:

  List with parameters related to cleaning the time series, including
  masking and imputation

- params_output:

  List with parameters related to how GGIR stores its output

- params_general:

  List with parameters related to general topics

- input:

  All objects provided by users

- configfile_csv:

  Csv configuration file

- params2check:

  Character vector to indicate which params objects need to be checked.
  This allows us to prevent the function from checking params objects
  that are not used in the context where function extract_params is
  used.

## Value

Lists of updated parameter objects

## Author

Vincent T van Hees \<v.vanhees@accelting.com\>

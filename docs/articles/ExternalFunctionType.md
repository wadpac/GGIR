# External functions with categorical output

GGIR allows users to [embed external
functions](https://wadpac.github.io/GGIR/articles/ExternalFunction.html)
for processing raw accelerometer data. This functionality can be used
when an algorithm that is not part of GGIR needs to be applied to the
raw data before summarising the results.

Here we describe a specific use case: external functions that classify
each epoch into one of a set of categorical types, such as activity
types or postures, and the output GGIR generates for them. This
functionality is activated by setting `reporttype = "type"` in the
`myfun` object.

This vignette complements [Embedding external functions in
GGIR](https://wadpac.github.io/GGIR/articles/ExternalFunction.html),
which describes all elements of `myfun`, how output columns are
aggregated to the GGIR epoch size, and how missing values are handled.
Those topics are not repeated here.

## Defining an external function

An external function used with GGIR is supplied through a `myfun` list.
For `reporttype = "type"`, the output of the function is interpreted as
categorical information rather than as a numeric signal.

For example:

``` r

classify_activity <- function(data, parameters) {
  acc = data[, 1]

  result = ifelse(
    acc < parameters[1],
    "rest",
    ifelse(acc < parameters[2], "walking", "running")
  )

  return(result)
}
```

The corresponding `myfun` object can be defined as:

``` r

myfun = list(
  FUN = classify_activity,
  parameters = c(0.05, 0.20),
  expected_sample_rate = 100,
  expected_unit = "g",
  colnames = "activity",
  minlength = 1,
  outputres = 5,
  outputtype = "character",
  timestamp = FALSE,
  reporttype = "type"
)
```

For `reporttype = "type"`, the external function returns categorical
information rather than a numeric measure that GGIR should average or
sum. In this example, it could return values such as:

``` text
c("rest", "rest", "walking", "walking", "running", "running", "rest")
```

The values returned in the output column become the categorical levels
used by GGIR.

The elements of `myfun` are described in [Embedding external functions
in GGIR](https://wadpac.github.io/GGIR/articles/ExternalFunction.html).
For a categorical output, `outputtype` should be `"character"` and
`reporttype` should be `"type"`.

## Multiple categorical outputs

Multiple categorical output columns are supported, specified as
described in [Multiple output
columns](https://wadpac.github.io/GGIR/articles/ExternalFunction.html#multiple-output-columns).
Each output column is treated as an independent categorical variable.

For example, an external function could classify both activity and
posture:

``` r

classify_activity_and_posture <- function(data, parameters) {

  activity = ifelse(
    data[, 1] < parameters[1],
    "rest",
    ifelse(data[, 1] < parameters[2], "walking", "running")
  )

  posture = ifelse(
    data[, 2] < parameters[3],
    "sitting",
    "standing"
  )

  return(data.frame(
    activity = activity,
    posture = posture
  ))
}
```

The corresponding `myfun` object could be defined as:

``` r

myfun = list(
  FUN = classify_activity_and_posture,
  parameters = c(0.05, 0.20, 0.10),
  expected_sample_rate = 100,
  expected_unit = "g",
  colnames = c("activity", "posture"),
  minlength = 1,
  outputres = 5,
  outputtype = c("character", "character"),
  timestamp = FALSE,
  reporttype = c("type", "type")
)
```

The two categorical outputs are processed independently. The categorical
levels within `activity` are mutually exclusive with each other, and the
levels within `posture` are mutually exclusive with each other. However,
an epoch can contribute simultaneously to one `activity` level and one
`posture` level.

For example, an epoch can be classified as `"walking"` for `activity`
and `"standing"` for `posture`.

This allows several independent categorical dimensions to be obtained
from the same external function.

## Classifiers with a higher output resolution

If the classifier produces output at a shorter resolution than the GGIR
epoch size, the classes need to be aggregated to one class per epoch
with `aggfunction`, for example by selecting the most frequent class.
See [Aggregation and missing
values](https://wadpac.github.io/GGIR/articles/ExternalFunction.html#aggregation-and-missing-values)
for a suitable function and for how missing values are handled. Epochs
that remain `NA` after aggregation are reported as unclassified, see
[Part 2](#part-2) below.

## Additional parameters for `reporttype = "type"`

In addition to the elements required to define the external function,
several optional parameters can be added to `myfun` to control how the
categorical output is summarised. These parameters are `ilevels`,
`tbout.dur`, `tbout.criter`, and `tbout.order`.

### `ilevels`: acceleration intensity levels

The `ilevels` parameter defines acceleration intensity thresholds used
when summarising the categorical output in part 2.

For example:

``` r

myfun$ilevels = c(0, 80, 200)
```

defines three acceleration ranges:

``` text
0-80 mg
80-200 mg
200 mg or more
```

These levels are used to obtain type-specific summaries of the time
spent at different acceleration intensities.

### `tbout.dur`: bout duration thresholds

The `tbout.dur` parameter defines the duration thresholds used to
identify bouts within each categorical type.

For example:

``` r

myfun$tbout.dur = c(1, 3, 5, 10)
```

defines the following bout categories:

``` text
1-3 minutes
3-5 minutes
5-10 minutes
10 minutes or longer
```

These thresholds determine the bout categories for which type-specific
bout summaries are generated.

### `tbout.criter`: bout criterion

The `tbout.criter` parameter specifies the criterion used to determine
whether a sequence of epochs qualifies as a bout.

For example:

``` r

myfun$tbout.criter = 0.8
```

can be used to require that at least 80% of the epochs within the bout
meet the criterion for the corresponding type. This allows the bout
definition to tolerate some epochs that do not belong to the target type
while still classifying the surrounding sequence as a bout.

### `tbout.order`: type priority for bout assignment

The `tbout.order` parameter specifies the order in which categorical
levels are considered when identifying bouts in part 5.

This is relevant when more than one categorical level is present within
the same output column. Part 5 assigns bouts within a categorical output
mutually exclusively, so the order determines which type is considered
first when bout periods overlap.

For example:

``` r

myfun$tbout.order = c("running", "walking", "rest")
```

means that GGIR first identifies `running` bouts, then identifies
`walking` bouts using the remaining time, and finally identifies `rest`
bouts using the time remaining after the previous types have been
assigned.

Within each type, bout durations are considered from longest to
shortest.

The spelling and case of classification labels are preserved in the
output. Values in `tbout.order` are matched to those labels
case-insensitively.

The priority is applied independently to each categorical output column.
For example, the bout assignment of an `activity` output does not
prevent the same epoch from being classified into a bout in an
independent `posture` output.

## Running GGIR

The parameters can be combined in the same `myfun` object. For example:

``` r

myfun = list(
  FUN = classify_activity,
  parameters = c(0.05, 0.20),
  expected_sample_rate = 100,
  expected_unit = "g",
  colnames = "activity",
  minlength = 1,
  outputres = 5,
  outputtype = "character",
  timestamp = FALSE,
  reporttype = "type",
  ilevels = c(0, 80, 200),
  tbout.dur = c(1, 3, 5, 10),
  tbout.criter = 0.8,
  tbout.order = c("running", "walking", "rest")
)
```

Once `myfun` has been defined, it is supplied to
[`GGIR()`](https://wadpac.github.io/GGIR/reference/GGIR.md) in the same
way as other external functions:

``` r

library(GGIR)

GGIR(
  datadir = "~/mydata",
  outputdir = "~/myresults",
  mode = 1:5,
  myfun = myfun
)
```

## Type-specific summaries

When `reporttype = "type"`, GGIR uses the categorical levels returned by
the external function to generate type-specific summaries. The summaries
generated by part 2 and part 5 differ because the two parts process the
categorical output in different ways.

### Part 2

Part 2 calculates type-specific summaries within each calendar day. For
each categorical type, the output includes the total time assigned to
the type and summaries of that time according to the acceleration
intensity levels defined by `ilevels`.

GGIR also reports two categories for epochs that are not assigned to a
type: `unclassified` is valid time for which the external function
returned `NA`, while `invalid` is time marked invalid by GGIR’s data
quality checks. Padding added only to construct complete analysis
windows is excluded from both categories.

Part 2 also provides bout summaries for each type and bout definition
specified by `tbout.dur`. These include:

- `totdur`: total duration of the bouts;

- `meandur`: mean bout duration; and

- `Nbouts`: number of bouts.

Bout summaries are calculated independently for each type and bout
definition.

When multiple `type` output columns are provided, these summaries are
generated independently for each output column.

The daily and person-level results are written to
`part2_daytypesummary.csv` and `part2_typesummary.csv`, respectively.

### Part 5

Part 5 summarises the categorical output separately for the sustained
sleep period (`spt`) and the rest of the day (`day`). For each type, the
output distinguishes between:

- unbouted time;

- time occurring within each defined bout category; and

- total time assigned to the type.

The corresponding metrics include duration (`dur`), mean acceleration
(`ACC`), and the number of contiguous blocks or bouts
(`Nblocks`/`Nbouts`).

For example, for an output column called `activity` with a level called
`walking`, variables in the dedicated report follow this structure:

``` r

activity_dur_spt_walking_unbt_min
activity_ACC_spt_walking_unbt_mg
activity_Nblocks_spt_walking_unbt
```

Bout-specific variables use the bout categories defined by `tbout.dur`,
while total variables describe all time assigned to the type regardless
of whether it belongs to a defined bout.

The same type-specific metrics are generated for both the `spt` and
`day` windows. The exact variables depend on the categorical levels
returned by the external function and the bout definitions specified in
`myfun`.

When multiple categorical output columns are provided, Part 5 generates
the corresponding type-specific metrics separately for each output
column.

The CSV summaries include every categorical output column. The visual
PDF report currently displays only the first output column whose
`reporttype` is `"type"`; additional categorical outputs are not shown
in that plot.

For each categorical output column, Part 5 writes cleaned daily and
person-level reports named
`part5_extfuntype_<column>_daysummary_<window>_<sleep-definition>.csv`
and
`part5_extfuntype_<column>_personsummary_<window>_<sleep-definition>.csv`.
The corresponding unfiltered daily report is stored under `results/QC`
with `daysummary_full` in its filename.

## Part 2 versus part 5

Part 2 and part 5 use different processing strategies for the
categorical output.

In **part 2**, the recording is divided into calendar days before the
type-specific summaries are calculated. In **part 5**, the categorical
classification is first obtained over the continuous analysis segment
and is then summarised within the relevant day, `spt`, and `day`
windows.

This difference is particularly relevant for bouts. In part 2, bout
summaries are calculated independently within each calendar day, whereas
part 5 can identify a bout across a calendar-day boundary when the
classification is continuous.

In addition, part 5 uses mutually exclusive type assignment for bouts
within each categorical output column, with the priority between types
determined by `tbout.order`.

Therefore, type-specific results from part 2 and part 5 are not expected
to be identical; they represent summaries obtained through different
processing strategies.

## Related documentation

For the general external-function framework, including numeric and
event-based outputs, see [Embedding external functions in
GGIR](https://wadpac.github.io/GGIR/articles/ExternalFunction.html).

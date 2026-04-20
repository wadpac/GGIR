# Sleep Regularity Index

## What is SRI?

The Sleep Regularity Index (SRI) was proposed by Phillips and colleagues
([2017](https://doi.org/10.1038/s41598-017-03171-4)) and further
developed and shared as open-source R package
[sleepreg](https://github.com/dpwindred/sleepreg) by Windred and
colleagues ([2021](https://doi.org/10.1093/sleep/zsab254)). The SRI can
have a value between -100 and 100, where 100 reflects perfect regularity
(identical days), 0 reflects random pattern, and -100 reflects perfect
reversed regularity in sleep.

## Multiple implementations

R package ‘sleepreg’ was tailored to GGIR 2.0-0 as available at the time
and does not work with more recent GGIR versions. We now know that GGIR
2.0-0 came with various bugs and is best not used. Therefore, an effort
has been made to address this. Once the ‘sleepreg’ maintainers accept
the [proposed changes](https://github.com/dpwindred/sleepreg/pull/10),
it will upgrade the sleepreg package code to work with both historical
and later GGIR versions. In the mean time you can use the proposed
upgrade to package ‘sleepreg’ by installing it via R command:
`remotes::install_github("vincentvanhees/sleepreg", ref = "upgradeGGIRversion")`.

Complementary to this effort, GGIR offers two modified implementations
of SRI, which we refer to as SRI1 and SRI2. In this Annex we discuss
each aspect of their calculation and how they differ from the original
SRI calculation in ‘sleepreg’.

However, first we would like to list all aspects that all these
implementations have in common:

- SRI values are expressed on a scale from -100 to 100.
- Detected non-wear is treated as missing data and this is accounted for
  in the SRI calculation.
- Calculations are done based on 30 second epochs. Even if GGIR is
  configured to do its other analyses with a shorter epoch duration,
  e.g. 5 seconds, GGIR makes sure that values are aggregated to 30
  seconds for the SRI calculation.
- All implementations derive regularity between days that are defined
  from noon to noon.

## Regularity of what?

### SRI as derived with github.com/dpwindred/sleepreg

In ‘sleepreg’, ‘Sleep’ is by default defined as the combination of:

- GGIR detected sleep period time window minus WASO episodes that last
  at least 30 minutes.
- Any 30 minute interval outside the GGIR_detect sleep period time
  window with 95% or more sustained inactivity bout. This intervals are
  assumed to equal naps.

The ‘sleepreg’ package comes various options to modify the configuration
of the calculation as discussed in the package documentation and in the
supplementary materials to [Windred et
al. 2021](https://doi.org/10.1093/sleep/zsab254).

### SRI1 as stored in GGIR part 4

In SRI1, ‘Sleep’ is defined as all [sustained inactivity
bouts](https://wadpac.github.io/GGIR/articles/chapter8_SleepFundamentalsSibs.html),
which can optionally be smoothed with a rolling average window. The
rolling window size in hours is specified with parameter
`SRI1_smoothing_wsize_hrs`, and the fraction of time that is needed to
be sustained inactivity bout is set with parameter
`SRI1_smoothing_frac`.

The advantage of this calculation is that it does not rely on hard
assumptions about accurate detection of sleep and naps by GGIR. However,
SRI1 with default non-smoothing settings may need to be interpreted as a
rest regularity index. Only when using the smoothing parameters SRI1 may
better reflect a true indicator of sleep regularity.

SRI1 is calculated in GGIR part 3 and stored in the csv-output of GGIR
part 4.

### SRI2 as optionally stored in GGIR part 6

GGIR offers experimental functionality to classify daytime naps, which
is subject to ongoing investigation in young population of
pre-schoolers. We are still looking for community support to help
improve and evaluate the nap detection for adult populations. Please
reach out (v.vanhees at acceleting dot com) if you would be interested
in applying for funding or have funding and want to sponsor this
development. Once the nap detection functionality has matured we will
document it more elaborately. However, if you have been helping to
explore the nap detection and are confident that GGIR classifies both
sleep and naps accurately, e.g. after inspecting the visual reports,
then SRI2 would be superior over SRI1 as measure of sleep regularity.

In SRI2, ‘Sleep’ is defined as all sleep classified with GGIR part 4,
minus WASO, plus daytime naps as classified in GGIR part 5.

SRI2 comes with parameter `SRI2_WASOmin` which allows us to define the
minimum WASO length to be considered, any WASO shorter than this will be
ignored and treated as sleep. In part 6, SRI is only derived when
parameter `SRI2_WASOmin` is specified. If no nap parameters are set the
resulting SRI estimate will only reflect the main SPT window in the day.

## At what resolution is SRI calculated?

### SRI as derived with github.com/dpwindred/sleepreg

In the initial publication by Phillips 2017 it was proposed to only
calculate SRI based on seven, or a multitude of seven, consecutive days
of data without missing values. This to avoid a possible role of
imbalanced data to the final estimate. However, this renders many
datasets unsuitable for analysis and leads to a painful loss in sample
size and statistical power. This was improved upon by Windred 2021,
which allows for gaps in data. Next, SRI is calculated as one value
across all valid days in the recording.

### SRI1 and SRI2 in GGIR

GGIR calculates and stores the SRI per day-pair.

The benefit of the approach taken in GGIR is that it enables the user to
study the day-pair to day-pair variation in SRI, and the role of
day-pair inclusion criteria. Further, the access to SRI at day-pair
level makes it possible to account for an imbalanced datasets via
multilevel regression analysis applied to the output of GGIR, with
day-pair as one of the model levels.

## What level of nonwear is acceptable?

### SRI as derived with github.com/dpwindred/sleepreg

Entire days are ignored when there are more than 6 hours of nonwear.

### SRI1 and SRI2 in GGIR

Per day-pair GGIR calculates the fraction of the 30 second epoch-pairs
between both days that are valid, which can be found in the GGIR part 4
output under the variable name `SriFractionValid`.

By default, day-pairs are excluded if this fraction is below 0.66 or if
the fraction of valid data for the individual day is below 0.66. This
threshold is coupled with the 16-hour default value for parameter
`includenightcrit` (16/24 = 0.66). For example, if you set parameter
`includenightcrit = 12`, the fraction threshold will be:
`12 / 24 = 0.5`.

The benefit of the approach taking by GGIR is that it looks at both the
completeness of matching data points between day pairs and data
completeness in the day itself.

## Where to find SRI in the GGIR output?

### SRI1 as stored in GGIR part 4

The day-pair level estimates are stored as variable
`SleepRegularityIndex1` in the GGIR part 4 .csv-report on sleep.
Further, GGIR also stores the person-level aggregates such as: the plain
average over all valid days, the average of all valid weekend days, and
the average of all valid week days. No GGIR input arguments are needed
to invoke the SRI calculation in part 3. The calculation is
automatically performed after updating GGIR and processing your data.

### SRI2 as optionally stored in GGIR part 6

SRI2 estimates are stored as variable `SleepRegularityIndex2` in the
GGIR part 6 output.

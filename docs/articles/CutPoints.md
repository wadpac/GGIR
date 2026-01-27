# Published cut-points and how to use them in GGIR

**NOTE: If you are viewing this page via CRAN note that the main GGIR
documentation has been migrated to the [GGIR GitHub
pages](https://wadpac.github.io/GGIR/).**

## Considerations

The physical activity research field has used so called cut-points to
segment accelerometer time series based on level of intensity. In this
vignette we have compiled a list of published cut-points with
instructions on how to use them with GGIR. Please note that GGIR refers
to cut-points as thresholds, but we are referring to the same thing: A
value or a set of values to help split levels of movement intensity. As
newer cut-points are frequently published the list below may not be up
to date. **Please let us know if you are aware of any published
cut-points that we missed!**

### Cut-points expressed in gravitational units (this vignette)

This vignette focuses on cut-points for metrics that attempt to quantify
average acceleration per epoch in gravitational units. The strength of
these metrics is that their values are not affected by sampling rate and
epoch length improving comparability across studies.

### Cut-points NOT expressed in gravitational units (not in this vignette)

However, GGIR also facilitates some metrics whose values are not
expressed in gravitational units that were historically used. For
example, the metric as described by Neishabouri (see GGIR argument
`do.neishabouricounts`) which reflects the indicator of accumulated body
movement over time, referred to as counts, calculated by the ActiLife
software from the ActiGraph accelerometer brand. Cut-points for counts
corresponding to the ActiGraph brand have been recurrently proposed in
the literature, for example, see this [systematic
review](https://doi.org/10.1007%2Fs40279-017-0716-0) with a
stratification by age group. Note that cut-points for ActiGraph counts
proposed before the introduction of multiday raw data collection are
most likely hardware-based calculations which may not perfectly align
with ActiGraph software-based (Actilife) calculations of counts that
Neishabouri described. As a result, older cut-points may need to be used
with caution.

The cut-points you find in the literature for ActiGraph counts cannot be
applied to Neishabouri counts directly because both are epoch length
specific. The cut-points from the literature need to be corrected by a
conversion factor. The conversion factor is calculated as the epoch
length in the new study (e.g. 5 seconds) divided by the epoch length in
the original study (e.g. 60 seconds). Note that no correction for
differences in sampling rate is needed because Neishabouri counts
already account for this via down-sampling.

If we would want to use cut-point “100 counts per minute” from the
literature on 5 second epoch data, the GGIR function call would look
like this:

    GGIR([...],
         mode = 1:5,
         windowsizes = c(5, 900, 3600),
         do.neishabouricounts = TRUE,
         acc.metric = "NeishabouriCount_y",
         threshold.in = 100 * (5/60),
         [...])

As a side note - Although counts are epoch length specific it is
possible to convert them into gravitational units by dividing the count
value by the sampling rate as used by the count algorithm (30 Hertz for
ActiGraph counts) multiplied by the number of seconds in the time window
for which the count value was derived. For example, 100 counts per
minute equates to an average acceleration per minute of 100 / (30 x 60)
= 0.055 *g*, and 1950 counts per minute equals 1.0833 *g*. Note that the
absolute acceleration values from different acceleration metrics are not
comparable. However, expressing data in *g*-units can at least ease
evaluating the validity of metrics relative to criterion acceleration
and removes the non-informative connection to sampling rate (van Hees et
al. 2013 <https://doi.org/10.1371/journal.pone.0061691>). As this
calculation shows, count-units and *g*-unit metrics only differ by a
constant, by which there is no practical difference in their ability to
explain variance in human behaviour. Similarly, if you would like to
convert an average daily acceleration of 0.05 *g* back to counts per day
then you would have to multiply by (30 Hertz x 60 seconds x 1440
minutes), which gives 129600 counts per day.

## Relevant arguments to use cut-points in GGIR

The argument `mvpathreshold` is used in **part 2** to quantify the time
accumulated over a user-specified threshold over which the
moderate-to-vigorous intensity is expected to occur. The `mvpathreshold`
is applied over all the metrics extracted in part 1 with the arguments
*do.metric* (e.g., `do.enmo`, `do.mad`, `do.neishabouricounts`).

In **part 5**, `threshold.lig`, `threshold.mod`, and `threshold.vig` are
used to indicate the thresholds to separate inactivity from light, light
from moderate, and moderate from vigorous, respectively.These thresholds
are applied over the metric defined with `acc.metric` (default =
"ENMO"). Here a summary table for the parameters definition to calculate
some of the acceleration metrics that has been previously used for the
calibration of cut-points and how to define them to be used in the
physical activity intensity classification with cut-points.

[TABLE]

## Summary of published cut-points

### Cut-points for preschoolers

[TABLE]

\*These publications used acceleration metrics that sum their values per
epoch rather than average them per epoch like GGIR does. So, to use
their cut-point in GGIR, we provide a scaled version of the cut-points
presented in the paper as: `(CutPointFromPaper_in_gsecs/85.7) * 1000`.
Note that sample frequency of 87.5 as reported in the publication was
incorrect and based on correspondence with authors we replaced this by
85.7.

### Cut-points for children/adolescents

[TABLE]

\*These publications used acceleration metrics that sum their values per
epoch rather than average them per epoch like GGIR does. So, to use
their cut-point in GGIR, we provide a scaled version of the cut-points
presented in the paper as:
`(CutPointFromPaper_in_gmins/(sampleRateFromPaper * EpochLengthInSecondsPaper)) * 1000`
\*\* This publication used acceleration metrics that expressed their
cut-points in *g* units. So, to use their cut-point in GGIR, we provide
a cut-point multiplied by 1000.

### Cut-points for adults

[TABLE]

\*These publications used acceleration metrics that sum their values per
epoch rather than average them per epoch like GGIR does. So, to use
their cut-point in GGIR, we provide a scaled version of the cut-points
presented in the paper as:
`(CutPointFromPaper_in_gmins/(sampleRateFromPaper * EpochLengthInSecondsPaper)) * 1000`
^(†) In this
[publication](https://doi.org/10.1371%2Fjournal.pone.0109913), there are
cut-point based on data sampled at 30 Hz and 100 Hz. When scaling the
cut-points as specified in (\*), the resulting thresholds are virtually
the same (the ones presented in this table).

### Cut-points for older adults

[TABLE]

\*Cut-points derived from applying the Youden index on ROC curves.  
\*\* Cut-points derived from increasing Sensitivity over Specificity for
light and vice versa for moderate on ROC curves (see
[paper](https://doi.org/10.1080/02640414.2018.1555904) for more
details).  
^(†) These publications used acceleration metrics that sum their values
per epoch rather than average them per epoch like GGIR does. So, to use
their cut-point in GGIR, we provide a scaled version of the cut-points
presented in the paper as:
`(CutPointFromPaper_in_gmins/(sampleRateFromPaper * EpochLengthInSecondsPaper)) * 1000`
^(‡) More cut-points excluding data on aided walking and washing up
activities can be found in the
[publication](https://doi.org/10.1186/s13102-020-00196-7).

## Notes on cut-point validity

**Sensor calibration**

In all of the studies above, excluding Hildebrand et al. 2016, no effort
was made to calibrate the acceleration sensors relative to gravitational
acceleration prior to cut-point development. Theoretically this can be
expected to cause a bias in the cut-point estimates proportional to the
calibration error in each device, especially for cut-points based on
acceleration metrics which rely on the assumption of accurate
calibration such as metrics: ENMO, EN, ENMOa, and by that also metric
SVMgs used by studies such as Esliger 2011, Phillips 2013, and Dibben
2020.

**Idle sleep mode and ActiGraph**

As discussed in the [main package
vignette](https://cran.r-project.org/package=GGIR/vignettes/GGIR.html),
studies using the ActiGraph sensor often forget to clarify whether idle
sleep mode was used and if so, how it was accounted for in the data
processing.

**How about all the criticism towards cut-point methods?**

For a more elaborate reflection on the limitations of cut-points and a
motivation why cut-points still have value in GGIR see:
<https://www.accelting.com/why-does-ggir-facilitate-cut-points/>

## References

- Aittasalo 2015: <https://doi.org/10.1186/s13102-015-0010-0>
- Bammann 2021: <https://doi.org/10.1371/journal.pone.0252615>
- Dibben 2020: <https://doi.org/10.1186/s13102-020-00196-7>
- Dillon 2016: <https://doi.org/10.1371%2Fjournal.pone.0109913>
- Esliger 2011: <https://doi.org/10.1249/mss.0b013e31820513be>
- Fraysse 2020: <https://doi.org/10.3389%2Ffspor.2020.579278>
- Hildebrand 2014: <https://doi.org/10.1249/mss.0000000000000289>
- Hildebrand 2016: <https://doi.org/10.1111/sms.12795>
- Mielke 2023: <https://doi.org/10.1111/sms.14416>
- Migueles 2021: <https://doi.org/10.3390%2Fs21103326>
- Phillips 2013: <https://doi.org/10.1016/j.jsams.2012.05.013>
- Sanders 2018: <https://doi.org/10.1080/02640414.2018.1555904>
- Schaefer 2014: <https://doi.org/10.1249%2FMSS.0000000000000150>
- Roscoe 2017: <https://doi.org/10.1007/s00431-017-2948-2>
- Vähä-Ypyä 2015: <https://doi.org/10.1371/journal.pone.0134813>

# Fragmentation metrics from time series.

The function is used by
[g.part5](https://wadpac.github.io/GGIR/reference/g.part5.md) to derive
time series fragmentation metrics. The function assumes that NA values
and nonwear time is accounted for before the data enters the function.

## Usage

``` r
g.fragmentation(frag.metrics = c("mean", "TP", "Gini", "power",
        "CoV", "NFragPM", "all"), LEVELS = c(), Lnames=c(), xmin=1, mode = "day")
```

## Arguments

- frag.metrics:

  Character with fragmentation metric to exract. Can be "mean", "TP",
  "Gini", "power", or "CoV", "NFragPM", or all the above metrics with
  "all". See details.

- LEVELS:

  Numeric vector of behavioural level classes derived with
  [identify_levels](https://wadpac.github.io/GGIR/reference/identify_levels.md)

- Lnames:

  Character vector with names of classes used in LEVELS, see details.

- xmin:

  Numeric scalar to indicate the minimum recordable fragment length. In
  [g.part5](https://wadpac.github.io/GGIR/reference/g.part5.md) this is
  derived from the epoch length.

- mode:

  Character to indicate whether input data is daytime ("day") or sleep
  period time ("spt").

## Value

List with Character object showing how decimals are separated

- TP_PA2IN:

  Transition probability physical activity to inactivity

.

- TP_IN2PA:

  Transition probability physical inactivity to activity

- Nfrag_IN2LIPA:

  Number of inacitivty fragments succeeded by LIPA (light physical
  activity)

- TP_IN2LIPA:

  Transition probability physical inactivity to LIPA

- Nfrag_IN2MVPA:

  Number of inacitivty fragments succeeded by MVPA (moderate or vigorous
  physical activity)

- TP_IN2MVPA:

  Transition probability physical inactivity to MVPA

- Nfrag_MVPA:

  Number of MVPA fragments

- Nfrag_LIPA:

  Number of LIPA fragments

- mean_dur_MVPA:

  mean MVPA fragment duration

- mean_dur_LIPA:

  mean LIPA fragment duration

- Nfrag_IN:

  Number of inactivity fragments

- Nfrag_PA:

  Number of activity fragments

- mean_dur_IN:

  mean duration inactivity fragments

- mean_dur_PA:

  mean duration activity fragments

- Gini_dur_IN:

  Gini index corresponding to inactivity fragment durations

- Gini_dur_PA:

  Gini index corresponding to activity fragment durations

- CoV_dur_IN:

  Coefficient of Variance corresponding to inactivity fragment durations

- CoV_dur_PA:

  Coefficient of Variance corresponding to activity fragment durations

- alpha_dur_IN:

  Alpha of the fitted power distribution through inactivity fragment
  durations

- alpha_dur_PA:

  Alpha of the fitted power distribution through activity fragment
  durations

- x0.5_dur_IN:

  x0.5 corresponding to alpha_dur_IN

- x0.5_dur_PA:

  x0.5 corresponding to alpha_dur_PA

- W0.5_dur_IN:

  W0.5 corresponding to alpha_dur_IN

- W0.5_dur_PA:

  W0.5 corresponding to alpha_dur_PA

- NFragPM_IN:

  Number of IN fragments per minutes in IN

- NFragPM_PA:

  Number of PA fragments per minutes in PA

- SD_dur_IN:

  Standard deviation in the duration of inactivity fragments

- SD_dur_PA:

  Standard deviation in the duration of physical activity fragments

## Details

See package vignette for description of fragmentation metrics. In short,
abbreviation "TP" refers to transition probality metrics, abbreviation
"CoV" refers to Coefficient of Variance, and metric "NFragPM" refers to
the Number of fragments per minute.

Regarding the Lnames argument. The class names included in this are
categorised as follows:

- Inactive, if name includes the character strings "day_IN_unbt" or
  "day_IN_bts"

- LIPA, if name includes the character strings "day_LIG_unbt" or
  "day_LIG_bts"

- MVPA, if name includes the character strings "day_MOD_unbt",
  "day_VIG_unbt", or "day_MVPA_bts"

## Author

Vincent T van Hees \<v.vanhees@accelting.com\>

## Examples

``` r
if (FALSE) { # \dontrun{
    x = c(6, 5, 6, 7, 6, 6, 7, 6, 6, 5, 6, 6, 6, 5, 7, 6, 6, 5, 5, 5, 6, 7, 6,
        6, 6, 6, 7, 6, 5, 5, 5, 5, 5, 6, 6, 6, 5, 6, 6, 6, 6, 6, 6, 6, 6, 6,
        7, 7, 6, 5, 6, 5, 6, 5, rep(12, 11), 5, 6, 6, 6, 5, 6, rep(9, 14), 6,
        5, 7, 7, 6, 7, 7, 7, 6, 6, 6, 5, 6, 5, 5, 5, 6, 5, 5, 5, 5, 5, 5, 5)
  Lnames = c("spt_sleep", "spt_wake_IN", "spt_wake_LIG", "spt_wake_MOD",
            "spt_wake_VIG", "day_IN_unbt", "day_LIG_unbt", "day_MOD_unbt",
            "day_VIG_unbt", "day_MVPA_bts_10", "day_IN_bts_30",
             "day_IN_bts_10_30", "day_LIG_bts_10")
  out = g.fragmentation(frag.metrics = "all",
                        LEVELS = x,
                        Lnames=Lnames)} # }
```

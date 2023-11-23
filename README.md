---
editor_options: 
  markdown: 
    wrap: 72
---

![](vignettes/GGIR-MASTERLOGO-RGB.png)

![GitHub Actions
R-CMD-check](https://github.com/wadpac/GGIR/workflows/R-CMD-check/badge.svg)
[![codecov](https://codecov.io/gh/wadpac/GGIR/branch/master/graph/badge.svg)](https://app.codecov.io/gh/wadpac/GGIR)
[![](https://cranlogs.r-pkg.org/badges/last-month/GGIR)](https://cran.r-project.org/package=GGIR)

[**GGIR**](https://CRAN.R-project.org/package=GGIR) stands out as an
R-package designed for the processing of multi-day **raw accelerometer
data**, specifically tailored for applications in physical activity and
sleep research. The name GGIR was decided as a short and easy to
remember acronym for ***G**ENEActive and **G**ENEA data **I**n **R***,
which was the original purpose of GGIR. However, later the functionality
expanded to other sensor brands, and the abbreviation has lost its
functional meaning.

The term ***raw*** refers to data being expressed in m/s<sup>2</sup> or
gravitational acceleration, as opposed to the previous generation
accelerometers which stored data in brand-specific units. Although GGIR
is mainly designed to be applied over ***multi-day recordings***, it can
also be used on shorter recordings with only a few hours of data, yet
most of the functionalities available in the package are only applied
when the recordings last several days.

The signal processing pipeline includes automatic calibration, detection
of sustained abnormally high values, detection of non-wear, and
calculation of average magnitude of dynamic acceleration based on a
variety of metrics. Then, GGIR uses this information to describe the
data per recording, per day of measurement, and (optionally) per segment
of a day of measurement, including estimates of physical activity,
inactivity, and sleep. The output of GGIR comes in a variety of data
sets containing these estimates in csv files, as well as several
visualizations to assist with the check of the data quality, the
performance of the sleep-related algorithms, and summaries of the
estimates per file.

## Citing GGIR

We published an overview paper of GGIR in 2019
[link](https://doi.org/10.1123/jmpb.2018-0063) which tried to describe most of the functionalities available in the package at that time.

A correct citation of research software is important to make your
research reproducible and to acknowledge the effort that goes into the
development of open-source software.

To do so, please report the GGIR version you used in the text.
Additionally, please also cite:

1.  Migueles JH, Rowlands AV, et al. GGIR: A Research Community--Driven
    Open Source R Package for Generating Physical Activity and Sleep
    Outcomes From Multi-Day Raw Accelerometer Data. Journal for the
    Measurement of Physical Behaviour. 2(3) 2019. doi:
    10.1123/jmpb.2018-0063.

If your work depends on the quantification of **physical activity** then
also cite:

2.  van Hees VT, Gorzelniak L, et al. Separating Movement and Gravity
    Components in an Acceleration Signal and Implications for the
    Assessment of Human Daily Physical Activity. PLoS ONE 8(4) 2013.
    [link](https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0061691)
3.  Sabia S, van Hees VT, Shipley MJ, Trenell MI, Hagger-Johnson G,
    Elbaz A, Kivimaki M, Singh-Manoux A. Association between
    questionnaire- and accelerometer-assessed physical activity: the
    role of sociodemographic factors. Am J Epidemiol. 2014 Mar
    15;179(6):781-90. doi: 10.1093/aje/kwt330. Epub 2014 Feb 4. PMID:
    24500862 [link](https://pubmed.ncbi.nlm.nih.gov/24500862/)

If you used the **auto-calibration functionality** then also cite:

4.  van Hees VT, Fang Z, et al. Auto-calibration of accelerometer data
    for free-living physical activity assessment using local gravity and
    temperature: an evaluation on four continents. J Appl Physiol 2014.
    [link](https://doi.org/10.1152/japplphysiol.00421.2014)

If you used the **sleep detection** then also cite:

5.  van Hees VT, Sabia S, et al. A novel, open access method to assess
    sleep duration using a wrist-worn accelerometer, PLoS ONE, 2015
    [link](https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0142533)

If you used the **sleep detection without relying on sleep diary** then
also cite:

6.  van Hees VT, Sabia S, et al. Estimating sleep parameters using an
    accelerometer without sleep diary. Scientific Reports 2018. doi:
    10.1038/s41598-018-31266-z.
    [link](https://www.nature.com/articles/s41598-018-31266-z)

If you used the **sleep regularity index** then also cite:

7.  Andrew J. K. Phillips, William M. Clerx, et al. Irregular sleep/wake
    patterns are associated with poorer academic performance and delayed
    circadian and sleep/wake timing. Scientific Reports. 2017 June 12
    [link](https://www.nature.com/articles/s41598-017-03171-4).

## Images usage

The copyright of the GGIR logo as contained in the file
vignettes/GGIR-MASTERLOGO-RGB.png lies with Accelting (Almere, The
Netherlands), please contact
[v.vanhees\@accelting.com](mailto:v.vanhees@accelting.com){.email} to
ask for permission to use this logo.

All other images in this repository are released under the Creative
Commons Attribution 4.0 International (CC BY 4.0) license.

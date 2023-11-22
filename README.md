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

[**GGIR**](https://CRAN.R-project.org/package=GGIR) is an R-package to
process multi-day raw accelerometer data for physical activity and sleep
research. The term ***raw*** refers to data being expressed in
m/s<sup>2</sup> or gravitational acceleration as opposed to the previous
generation accelerometers which stored data in accelerometer brand
specific units. The signal processing includes automatic calibration,
detection of sustained abnormally high values, detection of non-wear and
calculation of average magnitude of dynamic acceleration based on a
variety of metrics. Next, GGIR uses this information to describe the
data per recording, per day of measurement, and (optionally) per segment
of a day of measurement, including estimates of physical activity,
inactivity and sleep. We published an overview paper of GGIR in 2019
[link](https://doi.org/10.1123/jmpb.2018-0063).

## Images usaged

The copyright of the GGIR logo as contained in the file
vignettes/GGIR-MASTERLOGO-RGB.png lies with Accelting (Almere, The
Netherlands), please contact
[v.vanhees\@accelting.com](mailto:v.vanhees@accelting.com){.email} to
ask for permission to use this logo.

All other images in this repository are released under the Creative
Commons Attribution 4.0 International (CC BY 4.0) license.

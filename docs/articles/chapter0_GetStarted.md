# Get started: the GGIR R package

If you arrive here with the expectation to find a quick instruction to
run and use GGIR for your research then we have to disappoint you. The
number of GGIR functionalities is vast and mastering them requires a
time investment. In this section we will only provide you with a brief
introduction on how GGIR is used. However, to fully benefit from all the
functionality GGIR offers we recommend that you read the chapters on
this website.

The first step to get started is to install R, Rstudio, and GGIR as
discussed
[here](https://wadpac.github.io/GGIR/articles/chapter0_Installation.html).

If you have never used R before, we strongly recommend that you follow a
R introduction course. In
[this](https://www.accelting.com/short-r-tutorial/) blog post we have
attempted to provide a short tutorial. Nonetheless, the more effort you
make to master the basics of R the easier it will be for you to master
GGIR.

Next, make sure you have at least one accelerometer file available on
your computer. If you do not have an accelerometer file, feel free to
use the files from the GGIR training
[link](https://www.dropbox.com/s/rsh8b2sc7v113xg/accelerometer_data.zip?dl=0).
This is a zipped folder of 369MB with 4 accelerometer files and a sleep
log.

## Run GGIR for the first time

1.  First, you will need to place your file(s) in a folder on your
    computer. Make sure that this folder only contains accelerometer
    files.

2.  Use the following command to run GGIR.

    - datadir refers to the directory where you have located the
      accelerometer files.

    - outputdir refers to the directory where you want to store GGIR’s
      output.

``` r
library(GGIR) 
GGIR(datadir="C:/mystudy/mydata", outputdir="D:/myresults")
```

After some minutes, you should be able to see how your output directory
gets populated with files, reports, and visualizations.

This command will let GGIR run with all its default settings
(parameters), so the analysis is not tailored yet to the study design
and your research question. The documentation chapters you will find on
this website will guide you through this.

## Related links

- [Install R and
  GGIR](https://wadpac.github.io/GGIR/articles/chapter0_Installation.html)
- [Get
  support](https://wadpac.github.io/GGIR/articles/chapter0_Support.html)
- [Suitable file formats for
  GGIR](https://wadpac.github.io/GGIR/articles/chapter2_Pipeline.html#input-files)

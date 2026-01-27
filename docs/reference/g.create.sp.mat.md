# Converts sleep period information. Not intended for direct use

Function to convert data into sleep period matrix part of g.part4.R. Not
intended for direct use by package user

## Usage

``` r
g.create.sp.mat(nsp,spo,sleepdet.t,daysleep=FALSE)
```

## Arguments

- nsp:

  Integer indicating the number of sleep periods

- spo:

  Empty matrix with overview of sleep periods, 5 columns and as along as
  nps

- sleepdet.t:

  Part of detected sleep from g.sib.det for one night and one sleep
  definition

- daysleep:

  Boolean to indicator whether this person woke up after noon
  (daysleeper)

## Value

- spo matrix with start and end of each sleep period

- calendardate date corresponding to the day on which the night started

- item wdayname weekdayname

## Author

Vincent T van Hees \<v.vanhees@accelting.com\>

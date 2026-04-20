# Package index

## All functions

- [`ABI()`](https://wadpac.github.io/GGIR/reference/ABI.md) : Activity
  balance index (ABI)
- [`apply_cosinor_IS_IV_Analyses()`](https://wadpac.github.io/GGIR/reference/apply_cosinor_IS_IV_Analyses.md)
  : Apply Cosinor Analyses to time series
- [`create_test_acc_csv()`](https://wadpac.github.io/GGIR/reference/create_test_acc_csv.md)
  : Creates csv data file for testing purposes
- [`create_test_sleeplog_csv()`](https://wadpac.github.io/GGIR/reference/create_test_sleeplog_csv.md)
  : Creates csv sleeplog file for testing purposes
- [`data.calibrate`](https://wadpac.github.io/GGIR/reference/data.calibrate.md)
  : Example output from g.calibrate
- [`data.getmeta`](https://wadpac.github.io/GGIR/reference/data.getmeta.md)
  : Example output from g.getmeta
- [`data.inspectfile`](https://wadpac.github.io/GGIR/reference/data.inspectfile.md)
  : Example output from g.inspectfile
- [`data.metalong`](https://wadpac.github.io/GGIR/reference/data.metalong.md)
  : Metalong object as part of part 1 milestone data
- [`data.ts`](https://wadpac.github.io/GGIR/reference/data.ts.md) : Time
  series data.frame stored by part 5
- [`DFA()`](https://wadpac.github.io/GGIR/reference/DFA.md) : Detrended
  Fluctuation Analysis
- [`g.calibrate()`](https://wadpac.github.io/GGIR/reference/g.calibrate.md)
  : function to estimate calibration error and make recommendation for
  addressing it
- [`g.getbout()`](https://wadpac.github.io/GGIR/reference/g.getbout.md)
  : function to calculate bouts from vector of binary classes
- [`g.getmeta()`](https://wadpac.github.io/GGIR/reference/g.getmeta.md)
  : Function to extract meta-data (features) from data in accelerometer
  file
- [`g.imputeTimegaps()`](https://wadpac.github.io/GGIR/reference/g.imputeTimegaps.md)
  : Impute gaps in three axis raw accelerometer data
- [`g.inspectfile()`](https://wadpac.github.io/GGIR/reference/g.inspectfile.md)
  : function to inspect accelerometer file for brand, sample frequency
  and header
- [`g.loadlog()`](https://wadpac.github.io/GGIR/reference/g.loadlog.md)
  : Load and clean sleeplog information
- [`g.part1()`](https://wadpac.github.io/GGIR/reference/g.part1.md) :
  function to load and pre-process acceleration files
- [`g.part2()`](https://wadpac.github.io/GGIR/reference/g.part2.md) :
  function to analyse and summarize pre-processed output from g.part1
- [`g.part3()`](https://wadpac.github.io/GGIR/reference/g.part3.md) :
  Detection of sustained inactivity periods as needed for sleep
  detection in g.part4.
- [`g.part4()`](https://wadpac.github.io/GGIR/reference/g.part4.md) :
  Labels detected sustained inactivity periods by g.part3 as either part
  of the Sleep Period Time window or not
- [`g.part5()`](https://wadpac.github.io/GGIR/reference/g.part5.md) :
  Merge output from physical activity and sleep analysis into one report
- [`g.part6()`](https://wadpac.github.io/GGIR/reference/g.part6.md) :
  Perform temporal pattern analyses
- [`g.plot5()`](https://wadpac.github.io/GGIR/reference/g.plot5.md) :
  Generate user-friendly visual report. The first part of the report
  summarizes important daily metrics in bar plot format. The second part
  of the report shows the raw data and annotations in 24-hr periods.
  Angle-z is shown with sleep annotations during the SPT (sleep period
  time) window. ENMO is shown with daytime inactivity and PA (physical
  activity) annotations in the lower section of each 24-hr plot. The PA
  annotations are based on a 10 minute bout metric and 80 of a 10 minute
  bout of MVPA. Vigorous PA is a short window of time above
  threshold.vig that is part of a bout of MVPA. Light PA is a short
  window of time above threshold.lig that is part of a bout of light PA.
- [`g.report.part2()`](https://wadpac.github.io/GGIR/reference/g.report.part2.md)
  : Generate report from milestone data produced by g.part2
- [`g.report.part4()`](https://wadpac.github.io/GGIR/reference/g.report.part4.md)
  : Generate report from milestone data produced by g.part4
- [`g.report.part5()`](https://wadpac.github.io/GGIR/reference/g.report.part5.md)
  : Generate report from milestone data produced by g.part5
- [`g.report.part5_dictionary()`](https://wadpac.github.io/GGIR/reference/g.report.part5_dictionary.md)
  : Generate data dictionary for reports from milestone data produced by
  g.part5
- [`g.report.part6()`](https://wadpac.github.io/GGIR/reference/g.report.part6.md)
  : Generate report from milestone data produced by g.part6
- [`g.shell.GGIR()`](https://wadpac.github.io/GGIR/reference/g.shell.GGIR.md)
  : Wrapper function around function GGIR
- [`GGIR-package`](https://wadpac.github.io/GGIR/reference/GGIR-package.md)
  : A package to process multi-day raw accelerometer data
- [`GGIR()`](https://wadpac.github.io/GGIR/reference/GGIR.md) : Shell
  function for analysing an accelerometer dataset.
- [`inspect_binFile_brand()`](https://wadpac.github.io/GGIR/reference/inspect_binFile_brand.md)
  : Identify the Device Brand from a Binary File
- [`is.ISO8601()`](https://wadpac.github.io/GGIR/reference/is.ISO8601.md)
  : Check whether character timestamp is in iso8601 format.
- [`iso8601chartime2POSIX()`](https://wadpac.github.io/GGIR/reference/iso8601chartime2POSIX.md)
  : Convert iso8601 timestamps to POSIX timestamp
- [`load_params()`](https://wadpac.github.io/GGIR/reference/load_params.md)
  : Load default parameters
- [`part6AlignIndividuals()`](https://wadpac.github.io/GGIR/reference/part6AlignIndividuals.md)
  : part6AlignIndividuals
- [`part6PairwiseAggregation()`](https://wadpac.github.io/GGIR/reference/part6PairwiseAggregation.md)
  : part6PairwiseAggregation
- [`POSIXtime2iso8601()`](https://wadpac.github.io/GGIR/reference/POSIXtime2iso8601.md)
  : Convert POSIX to iso8601 timestamp
- [`read.myacc.csv()`](https://wadpac.github.io/GGIR/reference/read.myacc.csv.md)
  : Read custom csv files with accelerometer data
- [`SSP()`](https://wadpac.github.io/GGIR/reference/SSP.md) : Estimated
  self-similarity parameter
- [`visualReport()`](https://wadpac.github.io/GGIR/reference/visualReport.md)
  : Generate visualisation of time series produced by part 5.

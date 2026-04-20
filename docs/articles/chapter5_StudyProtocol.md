# 5. Accounting for Study Protocol

When you use an accelerometer in a study, you are likely to give your
study participants specific instructions on when they should start
wearing the accelerometer, for how many days, and whether or not they
are expected to take the accelerometer off during specific activity
types or parts of the day. Further, it may be that you turned on the
accelerometers hours or even days before you gave it to the participant
or stopped it hours or days after you received it back. Your knowledge
about all these aspects of your study protocol can be used by GGIR to
mask certain periods of time in the recording. This is important because
this information is not necessarily obvious from the recorded data. For
instance, when a recording is started and dispatched to the participant
via mail, the time during which the devices are in transit and not worn
may be impossible to distinguish from a participant wearing the
accelerometer and commuting.

## Selecting/Masking the data

It is important that GGIR masks all data outside the time window for
which the participant was instructed to wear the accelerometer. Study
protocols differ in duration and expected wear period, which is why GGIR
offers a variety of ways to account for the study protocol.

The main parameter to do this is `data_masking_strategy`. It requires a
numeric value indicating one of the following strategies:

- **data_masking_strategy = 1** to indicate that a specific number of
  hours should be masked from the start and/or the end of the recording,
  specified with parameters `hrs.del.start` and `hrs.del.end`,
  respectively.

- **data_masking_strategy = 2** to indicate that only the data between
  the first and the last midnight in the recording should be considered.

- **data_masking_strategy = 3** to indicate that only the most active X
  24-h blocks starting any time in the day should be used, where X is
  specified by parameter `ndayswindow`. Note that this can be combined
  with the aforementioned parameters `hrs.del.start` and `hrs.del.end`,
  which will trim this window at the start and end of the recording.

- **data_masking_strategy = 4** to indicate that only the data after the
  first midnight should be considered.

- **data_masking_strategy = 5** is similar to data_masking_strategy = 3,
  yet it selects X complete calendar days, where X is specified with
  parameter `ndayswindow`.

Additionally, you can set the maximum duration the accelerometer is to
be worn after recording starts. Use parameter `maxdur` to specify the
duration in the number of 24 hour blocks or parameter
`max_calendar_days`for the number of calendar days.

## Key parameters

- `data_masking_strategy`

- `hrs.del.start`

- `hrs.del.end`

- `ndayswindow`

- `maxdur`

- `max_calendar_days`

## Related output

| (Part of) variable name | Description | Report(s) |
|----|----|----|
| data exclusion strategy | A log of the decision made when calling g.impute: value=1 mean ignore specific hours; value=2 mean ignore all data before the first midnight and after the last midnight | part2_summary.csv |
| n hours ignored at start of meas | Number of hours ignored at the end of the measurement (if data_masking_strategy = 1) or at the end of the ndayswindow (if data_masking_strategy = 3 or 5). A log of decision made in part2.R | part2_summary.csv |
| n hours ignored at end of meas | Number of hours ignored at the start of the measurement (if data_masking_strategy = 1) or at the start of the ndayswindow (if data_masking_strategy = 3 or 5) A log of decision made in part2.R | part2_summary.csv |
| n days of measurement after which all data is ignored | Number of days of measurement after which all data is ignored (if data_masking_strategy = 1, 3 or 5) A log of decision made in part2.R | part2_summary.csv |

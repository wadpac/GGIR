# Calibration performance review

## Scope

The implementation reviewed here is committed in `ea6066e5` ("Reduce
calibration computation and allocation overhead"). The benchmark baseline
is `d72bf1b7`, which already contains the earlier `vapply(..., sd)` improvement.
The runtime changes remain inside
calibration; they do not modify GGIRread, g.getmeta(), the public interface,
dependencies or C++ code.

Commit `afa4f5c4` separately updates the downstream hip sleep-window regression
expectations affected by the earlier rolling-median tail fix. The updated
sleep-window and rolling-median tests were verified using the committed
calibration baseline, independently of this larger refactor.

All timings below are measurements from this investigation on one recording,
not a continuing benchmark of HEAD. Re-run
[benchmark_g_calibrate.R](benchmark_g_calibrate.R) after further changes;
the saved baseline command below continues to select `d72bf1b7` explicitly.

Measurements used GGIR 3.3-9, GGIRread 1.2.0 and R 4.6.1 on Windows. The
real input was `37727_0000000001_23-02-2018_MariaClara.cwa` (172 MB).

## Findings and implementation

The earlier full-calibration profile attributed approximately 59% of samples
to reading, 14% to garbage collection, 9% to epoch SDs, 8% to epoch means,
and 4% to weighted least squares. Nested shares overlap.

The larger R changes target allocations and repeated preparation:

- Keep raw columns in a data frame instead of assembling and trimming full
  matrices containing acceleration, time and temperature. Append leftovers
  column by column, and extract only the vectors needed for complete hours.
- Release raw columns and EN vectors before memory accounting and before
  loading the next block.
- For epoch means, select cumulative-sum boundaries before prepending zero.
  This avoids a full-sized concatenation while preserving the original
  cumulative sums, rounding, cancellation and propagation of missing values.
- Start the feature buffer with 8,640 epochs (one day) and retain the existing
  expansion mechanism. The former raw-sample-based estimate allocated more
  feature rows as sampling frequency decreased, although each row always
  represented 10 seconds. Prepare only rows collected so far when fitting.
- Convert the small fitting input to a matrix once. Apply calibration per
  axis using the same addition and reciprocal-division order, avoiding
  repeated data-frame conversion and `scale()`/`sweep()` intermediates.

The same samples, epoch SD calculation, duplicate/stationary-epoch selection,
`lm.wfit()` solver, weights, convergence tolerance, iteration limit and
acceptance/stopping rules are retained. A column-mean or alternative variance
formula was deliberately avoided because it can change exact comparisons.

## Reproducible cached-block benchmark

Use the supplied development script from the package root. It requires
`pkgload`, a CWA file and a baseline source file defining `g.calibrate()`.
For example, in PowerShell:

```powershell
git show d72bf1b7:R/g.calibrate.R | Set-Content "$env:TEMP/g-calibrate-baseline.R"
Rscript --vanilla dev/benchmark_g_calibrate.R "path/to/recording.cwa" "$env:TEMP/g-calibrate-baseline.R"
```

The script reads the first block once, outside timing, then gives both
versions exactly that block. It marks the block as final and uses
`minloadcrit = 0` so fitting can proceed. Both functions are byte-compiled
and warmed. It validates complete calibration outputs, excluding `bsc_qc`,
then alternates order over seven trials, collecting garbage outside timing.
It saves timings, result objects, allocation profiles and session information
to a temporary directory, or an optional third output-directory argument.

Final measurements on 6,658,370 real samples:

| Measurement | Baseline | Candidate |
| --- | ---: | ---: |
| Median cached calibration time | 1.08 s | 0.66 s |
| Total R allocations in a separate profiled call | 2,266 MiB | 1,095 MiB |

This is approximately **39% less computation time** and **52% fewer allocated
bytes** on this block. Allocations are cumulative R allocations, not peak
resident memory. An earlier run gave 1.23/0.76 s, supporting the local timing
improvement. These measurements exclude the substantial cost of file reading.

## Full-file checks and timing limits

A four-run comparison of the raw-column, mean and fitting changes used the
default calibration parameters and real reader. It recorded reading time
separately, without Rprof, and alternated old/new then new/old:

| Implementation | Total seconds | Reading seconds | Other seconds |
| --- | ---: | ---: | ---: |
| Baseline, first | 23.63 | 13.93 | 9.70 |
| Candidate, first | 22.23 | 14.23 | 8.00 |
| Candidate, second | 28.39 | 18.37 | 10.02 |
| Baseline, second | 43.65 | 28.87 | 14.78 |

This comparison preceded the final feature-buffer refinement. All four
returned exactly identical scientific results and read the same packet
ranges. Non-reading time fell in both pairs, but reading and machine timing
varied substantially. The cached benchmark is the stronger evidence for
the magnitude of the local improvement; these full-file timings do not
support claiming a fixed overall percentage.

The final version was additionally run through `GGIR(mode = 1)` with default
metrics and calibration, `do.parallel = FALSE`, and `do.report = NULL`.
It completed in 66.61 s. Calibration, inspection, short-epoch metrics and
long-epoch summaries were exactly identical to the preceding implementation's
saved output. This was a correctness check, not a matched timing comparison.

Resource-log timestamps and memory values intentionally differ. The adaptive
memory policy is unchanged; lower live memory can avoid chunk-size reduction
on workloads that previously exceeded its threshold.

## Regression coverage

The expanded calibration tests pass against both baseline and candidate:

- Exact means, EN, temperatures, SDs and epoch selection at 3, 30, 75 and
  100 Hz, with and without temperature correction.
- Constant epochs, values on both sides of the SD cutoff and a missing sample.
- Partial hours carried across mixed matrix/data-frame blocks.
- Accepted-fit early stopping and ignored high/constant temperatures.
- CWA and ActiGraph CSV zero removal, and insufficient stationary data.
- Feature-buffer expansion for a 25-hour input.

Known calibration coefficients are also recovered within the existing
iterative fit's accuracy. Full-file and cached comparisons independently
check exact equality of the complete returned scientific calibration object.

## Remaining opportunities

Reading now represents an even larger fraction of total time. Within
calibration, changing epoch mean/variance formulae, the fitting solver or
calibration stopping rules would require separate numerical or methodological
validation. Reusing a file read across calibration and metric extraction
could avoid more work but would involve the wider part 1 pipeline and memory
management, so it is outside this change.

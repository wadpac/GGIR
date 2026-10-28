# Part 1 performance review

## Commit status

This is the initial investigation, measured before the subsequent calibration
refactor. The changes are now committed:

- `d72bf1b7`: rolling-median fixes/direct `runmed()` path and calibration
  `vapply(..., sd)` improvement.
- `afa4f5c4`: downstream hip sleep-window test expectations updated after the
  median tail fix removed a spurious 15-second final-day SPT fragment.
- `ea6066e5`: further calibration allocation and computation improvements.

See [the calibration review](g-calibrate-performance-review.md) for the later
measurements and reproducible benchmark. The profile percentages and full-run
timings below describe the initial code, not the current committed version;
profile again before using them to rank future work.

## Scope

Profiled `GGIR(mode = 1)` on
`37727_0000000001_23-02-2018_MariaClara.cwa` (172 MB), using the local
GGIR sources (3.3-9), installed GGIRread 1.2.0 with the accelerated reader,
and R 4.6.1 on Windows. Used default metrics and calibration,
`do.parallel = FALSE`, `overwrite = TRUE`, and `do.report = NULL`.
Outputs were written to a temporary directory. No C++ changes.

## Where time goes

The initial `Rprof(interval = 0.01, gc.profiling = TRUE)` profile gives these
approximate inclusive shares. Nested rows overlap and must not be added.
Windows sampling times are useful for relative attribution, not wall time.

| Function/activity | Share of samples |
| --- | ---: |
| `g.getmeta()` including reading | 56% |
| `g.calibrate()` including reading | 43% |
| `GGIRread::readAxivity()` across both stages | 48% |
| Reader calendar/timestamp decoding | 29% |
| Garbage collection across all stages | 13% |
| `g.applymetrics()` | 14% |
| Rolling-median `process_axes()` | 9% |
| `scale()` | 6% |
| `detect_nonwear_clipping()` | 3% |

The reader is still the largest single component, particularly calendar
conversion. Within GGIR, rolling-median extraction offers a small, isolated
first improvement that also addresses a reported correctness problem.

## Implemented change

`g.applymetrics()` now calls `stats::runmed(..., endrule = "constant")`
directly for sufficiently long, complete downsampled axes. `zoo::rollmedian()`
already uses `runmed()` for this case; the direct call avoids constructing
zoo objects, indices and separate padding. Missing/short inputs retain a
zoo fallback. Existing treatment of internal missing medians is retained.

Three related correctness fixes accompany this:

- Trim the repeated **median**, rather than substituting raw acceleration,
  when the final downsampling step is partial. This includes 75 Hz blocks.
- Extend the tail with the last complete window median.
- Extend only padded edges, preserving real zero medians near block edges.

Downsampling and window-size conventions are unchanged. In particular,
75 Hz still uses every seventh sample and a 51-point median; revising the
effective window duration would be a separate methodological change.

These fixes intentionally affect angle, rolling-median acceleration and
deviation metrics. Historical outputs are not expected to be identical.

## Measurements and checks

For a simulated 12-hour, 100 Hz block with default ENMO and anglez metrics,
five alternating trials gave median times of 0.86 s originally, 0.88 s with
only John's two fixes, and 0.70 s with this change: about 19% faster than the
original, or 20% faster than the correctness-only version. The new output
matched the correctness-only version for this simulated input. Garbage
collection was run before each timed call, outside the timing.

Full-file wall times were 58.21 s and 60.63 s originally, versus 56.44 s and
99.94 s after the change. These measurements do **not** establish an overall
speedup. The slow repeat also spent substantially longer in unchanged
calibration/reader code (reader sampled time rose from 17.71 to 33.97 s),
so whole-run comparisons need a more stable measurement environment.
The isolated metric benchmark supports a local improvement only.

The full-file outputs contained 82,800 short epochs. ENMO, timestamps,
long-window summaries, file inspection and calibration results matched
(excluding calibration memory-log timestamps). Four anglez epochs changed
at chunk edges due to the corrected padding.

Regression tests compare all nine affected metrics against explicit window
medians at 10, 30, 75 and 100 Hz, with and without an extra final sample.
They cover angle-only requests, varying tails, genuine zeros and missing
data. Existing part 1 metrics, multi-format reader and nonwear tests also pass.

## Next steps, separately

1. Reduce full-block allocations in `g.getmeta()` calibration transforms.
   `scale()`/`sweep()` create large intermediate arrays; per-axis arithmetic
   is worth benchmarking, including temperature compensation. Preserve
   operation order and validate all output metrics.
2. Review explicit collection frequency: `g.getmeta()` collects before
   conversion and after each block, and `updateBlocksize()` also calls
   `gc()`. Collection is visible in the profile, but removing calls without
   measuring peak memory could make large files less reliable.
3. Calibration epoch SD overhead has been addressed with `vapply(..., sd)`
   in `d72bf1b7`. Further calibration buffer, raw-column and fitting improvements
   are implemented in `ea6066e5`; see the calibration review. Any further SD
   formula change would need checks for numerical cancellation, missing values
   and classification close to the stationary-epoch threshold.
4. If returning to GGIRread, examine calendar conversion in the reader's R
   callback. It dominates a sizeable fraction of the remaining reader time.
   Treat timezone/DST behavior as a separate correctness concern.

Items 1, 2 and 4 remain candidates for subsequent measured changes; item 3
records completed work and the numerical constraints on further changes.

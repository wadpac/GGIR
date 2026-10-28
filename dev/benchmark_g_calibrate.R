# Run from the package root:
# Rscript --vanilla dev/benchmark_g_calibrate.R <file.cwa> <baseline.R> [outputdir]
# baseline.R must define the version of g.calibrate() to compare with this tree.
# File reading is outside timing; both versions receive the same real CWA block.
args = commandArgs(trailingOnly = TRUE)
if (length(args) < 2) {
  stop("Supply a CWA file and a baseline R source defining g.calibrate().")
}
pkgload::load_all(".", quiet = TRUE)
outputdir = if (length(args) >= 3) args[3] else tempfile("ggir-calibrate-benchmark-")
dir.create(outputdir, recursive = TRUE, showWarnings = FALSE)

namespace = environment(GGIR::g.calibrate)
baseline_env = new.env(parent = namespace)
sys.source(args[2], envir = baseline_env)
baseline = baseline_env$g.calibrate
environment(baseline) = namespace
candidate = GGIR::g.calibrate

params = extract_params()
inspection = g.inspectfile(args[1], params_rawdata = params$params_rawdata,
                           params_general = params$params_general)
if (inspection$monc != MONITOR$AXIVITY || inspection$dformc != FORMAT$CWA) {
  stop("This benchmark expects an Axivity CWA file.")
}
block = g.readaccfile(
  filename = args[1], blocknumber = 1,
  blocksize = round(12 * 3600 * inspection$sf / 80 * params$params_rawdata$chunksize),
  filequality = list(filetooshort = FALSE, filecorrupt = FALSE,
                     filedoesnotholdday = FALSE),
  ws = 3600, inspectfileobject = inspection,
  params_rawdata = params$params_rawdata, params_general = params$params_general)
block$isLastBlock = TRUE
reader = function(...) block
bind_reader = function(fun) {
  environment(fun) = list2env(list(g.readaccfile = reader), parent = namespace)
  compiler::cmpfun(fun)
}
implementations = list(baseline = bind_reader(baseline), candidate = bind_reader(candidate))
run = function(fun) {
  result = fun(args[1], inspectfileobject = inspection, minloadcrit = 0, verbose = FALSE)
  result$bsc_qc = NULL # Resource log: timestamps and memory use intentionally differ.
  result
}

# Warm both implementations and validate the entire scientific result.
reference = run(implementations$baseline)
actual = run(implementations$candidate)
if (!identical(reference, actual)) {
  print(all.equal(reference, actual))
  stop("Calibration results differ.")
}
saveRDS(list(reference = reference, candidate = actual), file.path(outputdir, "results.rds"))

timings = data.frame()
for (trial in seq_len(7)) {
  order = if (trial %% 2) names(implementations) else rev(names(implementations))
  for (name in order) {
    gc()
    elapsed = system.time(run(implementations[[name]]), gcFirst = FALSE)[["elapsed"]]
    timings = rbind(timings, data.frame(trial = trial, implementation = name, elapsed = elapsed))
  }
}
write.csv(timings, file.path(outputdir, "timings.csv"), row.names = FALSE)
print(aggregate(elapsed ~ implementation, timings, median))

if (capabilities("profmem")) {
  allocations = data.frame()
  for (name in names(implementations)) {
    gc()
    path = file.path(outputdir, paste0(name, "-allocations.out"))
    Rprofmem(path)
    tryCatch(run(implementations[[name]]), finally = Rprofmem(NULL))
    lines = readLines(path)
    sizes = as.numeric(sub(" .*", "", lines[grepl("^[0-9]+ :", lines)]))
    allocations = rbind(allocations, data.frame(implementation = name,
                                               allocated_mb = sum(sizes) / 1024^2))
  }
  write.csv(allocations, file.path(outputdir, "allocations.csv"), row.names = FALSE)
  print(allocations) # Total R allocations, not peak resident memory.
}
capture.output(sessionInfo(), file = file.path(outputdir, "session-info.txt"))
cat("Cached samples:", nrow(block$P$data), "\nResults:", normalizePath(outputdir), "\n")

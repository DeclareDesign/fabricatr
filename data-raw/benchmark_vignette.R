# The source of the nine-row speed table in vignettes/fabricatr2.0.Rmd, of the
# "63 seconds against 17 seconds" figure in the paragraph below it, and of
# README.md's "two to four times faster to build". Rbuildignored.
#
# Run: Rscript data-raw/benchmark_vignette.R
#
# Three things about this script are load-bearing.
#
# First, every row runs in its own R process. estimatr's harness found cells
# whose time depended on how much the heap had grown before they ran, a
# five-fold difference for one of them, and a benchmark whose value depends on
# which benchmark ran before it is not a measurement. fabricatr's rows allocate
# less, but the rule costs nothing to keep and the failure it prevents is
# invisible when it happens.
#
# Second, both versions run the same expressions, defined once in ROWS below.
# The nine designs use only spellings that 1.0.2 and 2.0 both accept, which is
# why `potential_outcomes()` appears unnamed: naming it builds a data frame
# column in 1.x, so `Y = potential_outcomes(...)` yields `Y.Y_Z_0` there and
# `Y_Z_0` here, and the two versions would be timing different work.
#
# Third, the closing checks cover README.md as well as the vignette. The README
# sentence is a measured number living outside the document this script was
# written for, which is exactly the copy that goes stale unnoticed.
#
# Every row is run RUNS times, in RUNS separate processes, and the smallest of
# those medians is the one reported. A laptop is never idle: installing the
# 1.0.2 library above sets Spotlight indexing and XProtect scanning the new
# files, and a run taken while they work reads 25% high in both columns at
# once. The ratios survive that, the milliseconds do not, and the vignette
# quotes both. Repeating the row and keeping the quietest reading is what makes
# the absolute figures mean anything.

S <- Sys.getenv("FABRICATR_BENCH_DIR")
if (!nzchar(S)) {
  S <- file.path(tempdir(), "fabricatr_bench")
  Sys.setenv(FABRICATR_BENCH_DIR = S)
}
# The 1.0.2 build is identical every run, so it is cached outside tempdir().
LIB102 <- file.path(tools::R_user_dir("fabricatr", "cache"), "lib102")
RUNS <- 3L
dir.create(S, showWarnings = FALSE, recursive = TRUE)

# Row definitions ----
# label, the design, and replications. Replications fall where one call is slow.
flat <- function(n) {
  bquote(fabricate(N = .(n), x = rnorm(N), y = 0.5 * x + rnorm(N),
                   z = rbinom(N, 1, 0.5)))
}
ROWS <- list(
  flat_100 = list(
    "Flat, `N = 100`, three columns", flat(100), 1000),
  flat_5000 = list(
    "Flat, `N = 5,000`, three columns", flat(5000), 1000),
  flat_200k = list(
    "Flat, `N = 200,000`, three columns", flat(200000), 100),
  chain_500 = list(
    "Ten chained columns, `N = 500`",
    quote(fabricate(N = 500, x1 = rnorm(N), x2 = x1 + rnorm(N),
                    x3 = x2 + rnorm(N), x4 = x3 + rnorm(N), x5 = x4 + rnorm(N),
                    x6 = x5 + rnorm(N), x7 = x6 + rnorm(N), x8 = x7 + rnorm(N),
                    x9 = x8 + rnorm(N), x10 = x9 + rnorm(N))), 1000),
  two_level = list(
    "Two levels, 50 villages x 20 citizens",
    quote(fabricate(
      villages = add_level(N = 50, wealth = rnorm(N)),
      citizens = nest_level(N = 20, income = wealth + rnorm(N)))), 1000),
  three_small = list(
    "Three levels, 20 x 10 x 5",
    quote(fabricate(
      regions = add_level(N = 20, rq = rnorm(N)),
      villages = nest_level(N = 10, wealth = rq + rnorm(N)),
      citizens = nest_level(N = 5, income = wealth + rnorm(N)))), 1000),
  three_big = list(
    "Three levels, 50 x 20 x 20 (20,000 rows)",
    quote(fabricate(
      regions = add_level(N = 50, rq = rnorm(N)),
      villages = nest_level(N = 20, wealth = rq + rnorm(N)),
      citizens = nest_level(N = 20, income = wealth + rnorm(N)))), 200),
  icc = list(
    "Clustered draw with target ICC, 100 x 20",
    quote(fabricate(
      clusters = add_level(N = 100),
      citizens = nest_level(N = 20, y = draw_normal_icc(mean = 0,
                                                        clusters = clusters,
                                                        ICC = 0.5)))), 1000),
  po = list(
    "Potential outcomes plus reveal, `N = 1,000`",
    quote(fabricate(N = 1000, U = rnorm(N), Z = rbinom(N, 1, 0.5),
                    potential_outcomes(Y ~ 0.5 * Z + U),
                    Y_obs = reveal_outcomes(Y ~ Z))), 500)
)
ROW_KEYS <- names(ROWS)
# The three rows README.md's sentence is about, and the three it is not.
LEVEL_KEYS <- c("two_level", "three_small", "three_big")
# README.md says "about twice" of these two and "closer to three times" of the
# 20,000-row design, so they are checked apart.
SMALL_LEVEL_KEYS <- c("two_level", "three_small")
FLAT_KEYS <- c("flat_100", "flat_5000", "flat_200k")

# Worker: one row, one process ----
args <- commandArgs(TRUE)
if (length(args) == 3L) {
  libp <- args[1]; key <- args[2]; run_id <- args[3]
  if (nzchar(libp)) .libPaths(c(libp, .libPaths()))
  suppressMessages({library(fabricatr); library(microbenchmark)})
  ver <- as.character(packageVersion("fabricatr"))
  # .libPaths() order decides which build answers, so the row records the
  # version it actually loaded rather than the one the caller meant to ask for.
  stopifnot(if (nzchar(libp)) ver == "1.0.2" else ver >= "2")
  row <- ROWS[[key]]
  set.seed(20260910)
  # microbenchmark resolves its expression in its own calling frame, so the
  # thunk has to be a local of that frame rather than a global.
  time_it <- function(f, reps) microbenchmark(f(), times = reps)
  run <- function() eval(row[[2]], globalenv())
  d <- run()
  t <- time_it(run, row[[3]])$time / 1e6
  saveRDS(list(label = row[[1]], med = median(t), nrow = nrow(d), ncol = ncol(d)),
          file.path(S, sprintf("row_%s_%s_%s.rds",
                               if (ver >= "2") "200" else "102", key, run_id)))
  quit(save = "no")
}

# Orchestrator ----
if (!dir.exists(file.path(LIB102, "fabricatr"))) {
  dir.create(LIB102, showWarnings = FALSE, recursive = TRUE)
  message("installing fabricatr 1.0.2 into ", LIB102, " (CRAN source)")
  install.packages("fabricatr", lib = LIB102, repos = "https://cloud.r-project.org",
                   type = "source", INSTALL_opts = "--no-docs")
}

me <- normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)))
for (run in seq_len(RUNS)) {
  for (lib in c("", LIB102)) {
    for (k in ROW_KEYS) {
      message("run ", run, " ", if (nzchar(lib)) "1.0.2 " else "2.0.0 ", k)
      system2(file.path(R.home("bin"), "Rscript"),
              c(shQuote(me), shQuote(lib), shQuote(k), shQuote(run)),
              stdout = NULL, stderr = NULL)
    }
  }
}

# Table ----
# The quietest of the RUNS readings, which is the one least contaminated by
# whatever else the machine was doing.
get <- function(v, k) {
  runs <- lapply(seq_len(RUNS), function(r) {
    readRDS(file.path(S, sprintf("row_%s_%s_%s.rds", v, k, r)))
  })
  runs[[which.min(vapply(runs, function(x) x$med, 0))]]
}
old <- lapply(stats::setNames(ROW_KEYS, ROW_KEYS), function(k) get("102", k))
new <- lapply(stats::setNames(ROW_KEYS, ROW_KEYS), function(k) get("200", k))
gain <- vapply(ROW_KEYS, function(k) old[[k]]$med / new[[k]]$med, 0)

cat("\n| What is being built | fabricatr 1.0.2 | fabricatr 2.0 | Speedup |\n")
cat("|---|---:|---:|---:|\n")
for (k in ROW_KEYS) {
  cat(sprintf("| %s | %.2f ms | %.2f ms | %.1fx |\n",
              old[[k]]$label, old[[k]]$med, new[[k]]$med, gain[[k]]))
}

# The prose figure below the table is this row's median times 10,000 builds.
secs <- function(k, v) round(v[[k]]$med * 10000 / 1000)
cat(sprintf("\n10,000 builds of `three_big`: %d seconds under 1.0.2, %d seconds here\n",
            secs("three_big", old), secs("three_big", new)))

# The sentence above the table names the machine and three versions, so it is
# built from what this run actually used and compared to what the file says.
chip <- if (Sys.info()[["sysname"]] == "Darwin") {
  system2("sysctl", c("-n", "machdep.cpu.brand_string"), stdout = TRUE)
} else NA_character_
r_short <- paste(R.version$major, R.version$minor, sep = ".")
env_line <- sprintf(
  "Median time per call, measured with microbenchmark on an %s under R %s, fabricatr 1.0.2, dplyr %s.",
  chip, r_short, as.character(packageVersion("dplyr")))
cat("\n", env_line, "\n", sep = "")

# Checks ----
# Every row identical in shape under both versions, which is the weakest form
# of "the same work was timed" this script can afford at benchmark speed.
shape_ok <- vapply(ROW_KEYS, function(k) {
  identical(old[[k]]$nrow, new[[k]]$nrow) && identical(old[[k]]$ncol, new[[k]]$ncol)
}, TRUE)

# Each claim is read out of the file that carries it, so a row renamed in the
# vignette and not here, or a README sentence that quietly drifts from the
# table it summarises, fails in this script rather than in a reader's head.
vig <- readLines("vignettes/fabricatr2.0.Rmd")
readme <- readLines("README.md")
env_said <- grep("^Median time per call", vig, value = TRUE)
tbl_said <- grep("^[|] (Flat|Ten|Two|Three|Clustered|Potential)", vig, value = TRUE)
cell <- function(lines, i) {
  trimws(vapply(strsplit(lines, "|", fixed = TRUE), `[`, "", i))
}
labels_said <- cell(tbl_said, 2L)
said <- ROW_KEYS[match(labels_said, vapply(ROWS, `[[`, "", 1L))]
num <- function(i, suffix) {
  stats::setNames(as.numeric(sub(suffix, "", cell(tbl_said, i))), said)
}
gain_said <- num(5L, "x$")
old_said <- num(3L, " ms$")
new_said <- num(4L, " ms$")

# The table's own numbers, not only the prose around them. A tolerance is
# unavoidable: three runs of a row on a laptop spread about 5%, and a second
# machine would spread more. It is set wide enough not to cry wolf and far
# tighter than the staleness it exists to catch, which was 60% and had stood
# for three weeks.
TOL <- 0.25
drift <- pmax(abs(old_said / vapply(ROW_KEYS, function(k) old[[k]]$med, 0) - 1),
              abs(new_said / vapply(ROW_KEYS, function(k) new[[k]]$med, 0) - 1))
if (any(drift > TOL)) {
  cat("\nthe vignette's table has drifted from the machine:\n")
  for (k in ROW_KEYS[drift > TOL]) {
    cat(sprintf("  %-12s says %.2f / %.2f ms, is %.2f / %.2f ms\n",
                k, old_said[[k]], new_said[[k]], old[[k]]$med, new[[k]]$med))
  }
}

# The table rounds to one decimal and the claims are checked against these, so
# a row that prints "2.0x" and fails a ">= 2" check is legible here.
cat("\nunrounded gains:\n")
for (k in ROW_KEYS) cat(sprintf("  %-12s %.3fx\n", k, gain[[k]]))

stopifnot(
  "rows differ in shape between versions; the two are not timing the same work" =
    all(shape_ok),
  "the vignette's table is not the nine rows this script measures, in order" =
    identical(labels_said, unname(vapply(ROWS, `[[`, "", 1L))),
  "the sentence above the vignette's table does not describe this run" =
    length(env_said) == 1L && identical(env_said, env_line),
  "README.md no longer carries the sentence this script checks" =
    any(grepl("Levels are about twice as fast to build", readme, fixed = TRUE)),
  "the vignette says no row is slower under 2.0" =
    min(gain) > 1,
  "the vignette's table no longer matches the machine it says it was measured on" =
    all(drift <= TOL),
  "README.md says levels are about twice as fast to build" =
    all(gain[SMALL_LEVEL_KEYS] >= 1.9) && all(gain[SMALL_LEVEL_KEYS] <= 2.4),
  "README.md says the 20,000-row design is closer to three times" =
    gain[["three_big"]] >= 2.4 && gain[["three_big"]] <= 3.2,
  "the vignette says the gains concentrate in hierarchical data" =
    min(gain[LEVEL_KEYS]) > max(gain[FLAT_KEYS]),
  "the vignette says flat N = 200,000 is the row that does not move" =
    which.min(gain) == match("flat_200k", ROW_KEYS) && gain[["flat_200k"]] < 1.25
)
cat("\nthe speed claims in the vignette and in README.md hold\n")

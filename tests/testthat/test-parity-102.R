# Parity with fabricatr 1.0.2 over the deterministic recode family.
#
# GENERATED FILE. Do not edit by hand. Regenerate with
#   Rscript claude_control/tools/pkg_diff/parity_fixture.R \
#     ~/git_projects/.cran_reference_lib
# after installing the package under test. The case list and the exception
# list live in that script.
#
# CRAN machines have no 1.0.2 installed, so the reference side is recorded
# rather than computed. Nothing here is recomputed on another platform: `x`
# is a fixed draw from R's own RNG, the breaks are written out, and the
# signature compares integer category codes rather than the doubles they
# were cut from.
#
# The rest of this suite asserts internal consistency, and both defects found
# on 2026-09-20 were invisible to every assertion in it: the infinite-break
# offset in draw_ordered() and the draw_likert() endpoint change. Both move
# this signature, which is why it pins the codes and the full table and not
# just the shape.

parity_cases <- lapply(c(
  interior_1      = "draw_ordered(x, breaks = 0)",
  interior_3      = "draw_ordered(x, breaks = c(-1, 0, 1))",
  interior_5      = "draw_ordered(x, breaks = c(-2, -1, 0, 1, 2))",
  inf_lower_only  = "draw_ordered(x, breaks = c(-Inf, 0, 1))",
  inf_upper_only  = "draw_ordered(x, breaks = c(-1, 0, Inf))",
  inf_both        = "draw_ordered(x, breaks = c(-Inf, 0, Inf))",
  quantile_8      = "draw_ordered(x, breaks = qnorm(seq(0, 1, length.out = 8)))",
  quantile_4      = "draw_ordered(x, breaks = qnorm(seq(0, 1, length.out = 4)))",
  strict_interior = "draw_ordered(x, breaks = c(-1, 0, 1), strict = TRUE)",
  strict_inf      = "draw_ordered(x, breaks = c(-Inf, 0, Inf), strict = TRUE)",
  dup_breaks      = "draw_ordered(x, breaks = c(-1, 0, 0, 1))",
  breaks_above_x  = "draw_ordered(x, breaks = c(10, 20))",
  breaks_below_x  = "draw_ordered(x, breaks = c(-20, -10))",
  x_has_na        = "draw_ordered(xn, breaks = c(-1, 0, 1))",
  x_constant      = "draw_ordered(rep(0.5, 20), breaks = c(-1, 0, 1))",
  likert_5        = "draw_likert(x, min = -3, max = 3, bins = 5)",
  likert_breaks   = "draw_likert(x, breaks = c(-1, 0, 1))",
  likert_inf      = "draw_likert(x, breaks = c(-Inf, 0, Inf))",
  splitq_2        = "split_quantile(x, type = 2)",
  splitq_5        = "split_quantile(x, type = 5)",
  drawq_3         = "draw_quantile(x, type = 3)"
), function(s) parse(text = s)[[1]])
parity_signature <- function(value) {
  if (is.character(value) && length(value) == 1L) return(value)
  paste0("class=", paste(class(value), collapse = ","),
         " codes=", paste(utils::head(as.integer(value), 12), collapse = ","),
         " tab=", paste(table(as.integer(value), useNA = "no"), collapse = "/"),
         " nlev=", if (is.factor(value)) nlevels(value) else NA)
}

parity_signatures <- function() {
  set.seed(343)
  x <- rnorm(200)
  xn <- x
  xn[c(5, 50)] <- NA
  vapply(parity_cases, function(expr) {
    value <- tryCatch(eval(expr), error = function(e) paste0("ERROR: ", conditionMessage(e)),
                      warning = function(w) paste0("WARNING: ", conditionMessage(w)))
    parity_signature(value)
  }, character(1))
}

# Recorded under fabricatr 1.0.2.
expected_102 <- c(
  interior_1      = "class=integer codes=1,2,2,1,1,1,1,1,1,2,1,2 tab=96/104 nlev=NA",
  interior_3      = "class=integer codes=1,3,3,2,2,1,2,1,1,4,1,3 tab=27/69/78/26 nlev=NA",
  interior_5      = "class=integer codes=2,4,4,3,3,2,3,2,2,5,2,4 tab=2/25/69/78/21/5 nlev=NA",
  inf_lower_only  = "class=integer codes=1,2,2,1,1,1,1,1,1,3,1,2 tab=96/78/26 nlev=NA",
  inf_upper_only  = "class=integer codes=0,2,2,1,1,0,1,0,0,2,0,2 tab=27/69/104 nlev=NA",
  inf_both        = "class=integer codes=1,2,2,1,1,1,1,1,1,2,1,2 tab=96/104 nlev=NA",
  quantile_8      = "class=integer codes=1,5,4,2,3,1,2,1,1,7,1,5 tab=23/25/35/28/31/34/24 nlev=NA",
  quantile_4      = "class=integer codes=1,3,2,1,2,1,1,1,1,3,1,2 tab=58/74/68 nlev=NA",
  strict_interior = "class=integer codes=NA,2,2,1,1,NA,1,NA,NA,NA,NA,2 tab=69/78 nlev=NA",
  strict_inf      = "class=integer codes=1,2,2,1,1,1,1,1,1,2,1,2 tab=96/104 nlev=NA",
  dup_breaks      = "class=integer codes=1,4,4,2,2,1,2,1,1,5,1,4 tab=27/69/78/26 nlev=NA",
  breaks_above_x  = "class=integer codes=1,1,1,1,1,1,1,1,1,1,1,1 tab=200 nlev=NA",
  breaks_below_x  = "class=integer codes=3,3,3,3,3,3,3,3,3,3,3,3 tab=200 nlev=NA",
  x_has_na        = "class=integer codes=1,3,3,2,NA,1,2,1,1,4,1,3 tab=27/68/78/25 nlev=NA",
  x_constant      = "class=integer codes=3,3,3,3,3,3,3,3,3,3,3,3 tab=20 nlev=NA",
  likert_5        = "class=numeric codes=2,3,3,2,3,2,2,2,2,4,2,3 tab=3/43/99/47/7 nlev=NA",
  likert_breaks   = "class=numeric codes=NA,2,2,1,1,NA,1,NA,NA,NA,NA,2 tab=69/78 nlev=NA",
  likert_inf      = "class=numeric codes=1,2,2,1,1,1,1,1,1,2,1,2 tab=96/104 nlev=NA",
  splitq_2        = "class=factor codes=1,2,2,1,1,1,1,1,1,2,1,2 tab=100/100 nlev=2",
  splitq_5        = "class=factor codes=1,4,3,1,2,1,1,1,1,5,1,3 tab=40/40/40/40/40 nlev=5",
  drawq_3         = "ERROR: `N` must be provided to `draw_quantile()` and must be a single positive number."
)

# The breaks with 1.0.2 that NEWS.md documents, and what 2.0 does instead.
documented_breaks <- c(
  inf_upper_only = "1.0.2 returns 0-based codes for a trailing Inf; 2.0 numbers from 1 whatever breaks looks like",
  likert_5       = "1.0.2 returns NA for a value outside [min, max]; 2.0 puts it in the outermost bin",
  likert_breaks  = "1.0.2 reads a manual breaks vector as the full bin boundary set; 2.0 reads interior cut-points",
  likert_inf     = "draw_likert() returns integer codes where 1.0.2 returned doubles",
  drawq_3        = "the argument-validation message names the argument and the function"
)

expected_200 <- c(
  inf_upper_only = "class=integer codes=1,3,3,2,2,1,2,1,1,3,1,3 tab=27/69/104 nlev=NA",
  likert_5       = "class=integer codes=2,3,3,2,3,2,2,2,2,4,2,3 tab=4/43/99/47/7 nlev=NA",
  likert_breaks  = "class=integer codes=1,3,3,2,2,1,2,1,1,4,1,3 tab=27/69/78/26 nlev=NA",
  likert_inf     = "class=integer codes=1,2,2,1,1,1,1,1,1,2,1,2 tab=96/104 nlev=NA",
  drawq_3        = "ERROR: `N` must be a single positive integer."
)

test_that("the deterministic recode family matches fabricatr 1.0.2", {
  sigs <- parity_signatures()
  kept <- setdiff(names(sigs), names(documented_breaks))
  expect_identical(sigs[kept], expected_102[kept])
})

test_that("each documented break with 1.0.2 is still exactly that break", {
  # The exception list must not outlive the break it excuses: a case that
  # comes back into agreement with 1.0.2 has to leave the list, and one that
  # diverges further has to be read before it is recorded.
  sigs <- parity_signatures()
  broken <- names(documented_breaks)
  expect_identical(sigs[broken], expected_200[broken])
  expect_false(any(sigs[broken] == expected_102[broken]))
})

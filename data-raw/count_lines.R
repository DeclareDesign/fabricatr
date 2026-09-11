# Re-measures the four line counts quoted in the first paragraph of
# vignettes/fabricatr2.0.Rmd and fails if any has drifted.
#
# The counting rule, which is the part that matters: both packages are counted
# the same way, from the R sources of each, with blank lines and lines whose
# first non-space character is `#` removed for the code count, and lines
# beginning `#'` counted for the roxygen count. estimatr shipped a comparison
# of 1.x's total lines against 2.0's code-only lines twice before anyone ran
# the numbers; counting one side differently from the other is the whole trap.
#
# 1.0.2 is read out of the `main` branch of this repo rather than from a
# tarball, so this needs a git checkout and no network.
#
# Usage: Rscript data-raw/count_lines.R

count_file <- function(lines) {
  code <- lines[!grepl("^\\s*(#|$)", lines)]
  roxy <- lines[grepl("^\\s*#'", lines)]
  c(total = length(lines), code = length(code), roxygen = length(roxy))
}

count_ref <- function(paths, read) {
  Reduce(`+`, lapply(paths, function(p) count_file(read(p))))
}

# 2.0, the working tree
here <- list.files("R", pattern = "[.][Rr]$", full.names = TRUE)
new <- count_ref(here, readLines)

# 1.0.2, from main
main_files <- system2("git", c("ls-tree", "--name-only", "main", "R/"),
                      stdout = TRUE)
old <- count_ref(main_files, function(p) {
  system2("git", c("show", paste0("main:", p)), stdout = TRUE)
})

n_files_new <- length(here)
n_files_old <- length(main_files)

cat("fabricatr 1.0.2 (main): ", n_files_old, " files, ", old[["total"]],
    " lines, ", old[["code"]], " code, ", old[["roxygen"]], " roxygen\n", sep = "")
cat("fabricatr 2.0 (working tree): ", n_files_new, " files, ", new[["total"]],
    " lines, ", new[["code"]], " code, ", new[["roxygen"]], " roxygen\n", sep = "")

# Neither package compiles anything; the vignette says so.
stopifnot(
  "2.0 has grown a src/ directory; the vignette says neither package compiles anything" =
    !dir.exists("src"),
  "1.0.2 has a src/ directory; the vignette says neither package compiles anything" =
    length(system2("git", c("ls-tree", "--name-only", "main", "src/"), stdout = TRUE)) == 0
)

# The figures quoted in vignettes/fabricatr2.0.Rmd, first paragraph.
claimed <- c(files_new = 9, files_old = 16,
             code_new = 1013, code_old = 1609,
             roxygen_new = 882, roxygen_old = 751)
found <- c(files_new = n_files_new, files_old = n_files_old,
           code_new = new[["code"]], code_old = old[["code"]],
           roxygen_new = new[["roxygen"]], roxygen_old = old[["roxygen"]])

drift <- claimed != found
if (any(drift)) {
  stop("vignettes/fabricatr2.0.Rmd is stale. Quoted vs measured:\n",
       paste0("  ", names(claimed)[drift], ": says ", claimed[drift],
              ", is ", found[drift], collapse = "\n"),
       call. = FALSE)
}
cat("\nAll six figures in the vignette's first paragraph are current.\n")

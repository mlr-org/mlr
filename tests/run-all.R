library(testthat)
# we only check 'base' tests for CRAN, but all for TRAVIS (which takes looong)
# make sure YOU run ALL!
if (identical(Sys.getenv("TRAVIS"), "true")) {
  test_check("mlr")
  categories = list.files("testthat", pattern = "^test_")
  p = "^test(_[[:alnum:]]+_).+"
  stopifnot(all(grepl(p, categories)))
  categories = unique(sub("^test(_[[:alnum:]]+_).+", "\\1", categories))

  for (category in categories) {
    cat(sprintf("### Testing category '%s'", category), file = stdout)
    test_check("mlr", filter = category)
  }
} else {
  test_check("mlr", filter = "base")
}

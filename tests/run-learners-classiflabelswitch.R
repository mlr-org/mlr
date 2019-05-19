library(testthat)
if (identical(Sys.getenv("TRAVIS"), "True") ||
    identical(Sys.getenv("APPVEYOR"), "True") ||
    identical(Sys.getenv("R_EXPENSIVE_TEST_OK"), "true") ||
    identical(Sys.getenv("NOT_CRAN"), "true")) {

  if (getRversion() > "3.5.3") {
    # set old seed
    suppressWarnings(RNGversion("3.5.0"))
    # restore standard seed when done (so that we are back to defaults for the next tests)
    on.exit(suppressWarnings(RNGversion("3.6.0")))
  }
  set.seed(getOption("mlr.debug.seed"))

  test_check("mlr", "_learners_classiflabelswitch")
}

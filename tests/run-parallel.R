library(testthat)
# FIXME: I am not sure if enabling this leads to travis 'hanging'. we currently must test it locally
# if (identical(Sys.getenv("TRAVIS"), "true") || identical(Sys.getenv("R_EXPENSIVE_TEST_OK"), "true")) {
if (
    identical(Sys.getenv("R_EXPENSIVE_TEST_OK"), "true") # || identical(Sys.getenv("CIRCLECI"), "true")
    ) {

  if (getRversion() > "3.5.3") {
    # set old seed
    suppressWarnings(RNGversion("3.5.0"))
    # restore standard seed when done (so that we are back to defaults for the next tests)
    on.exit(suppressWarnings(RNGversion("3.6.0")))
  }
  set.seed(getOption("mlr.debug.seed"))

  test_check("mlr", "_parallel_")
}

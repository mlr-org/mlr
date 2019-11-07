library(testthat)

  # if (getRversion() > "3.5.3") {
  #   # set old seed
  #   suppressWarnings(RNGversion("3.5.0"))
  #   # restore standard seed when done (so that we are back to defaults for the next tests)
  #   on.exit(suppressWarnings(RNGversion("3.6.0")))
  # }
  set.seed(getOption("mlr.debug.seed"))

  test_check("mlr", filter = "base_")

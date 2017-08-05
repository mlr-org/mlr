library(testthat)
if (nchar(Sys.getenv("NO_RLINT")) == 0) {
  test_check("mlr", filter = "lint")
}

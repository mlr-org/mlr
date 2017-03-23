library("lintr")
context("lint")

test_that("lint check", {
  # linters are defined in help_lint.R
  expect_lint_free(linters = linters)
})


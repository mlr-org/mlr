context("fda_feature_selection")

test_that("fda_feature_selection", {
  p = generateFilterValuesData(fuelsubset.task, method = "information.gain")
  assertList(p, len = 2L)
}
)

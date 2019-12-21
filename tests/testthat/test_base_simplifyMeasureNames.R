test_that("simplifyMeasureNames", {
  # setup some measures get ids and aggegated names
  meas = list(mmce, acc, ber)
  meas.aggr = vcapply(meas, measureAggrName)
  meas.ids = extractSubList(meas, "id")
  # some dummy-strings not representing measures
  no.meas = c("abc", "def")
  # join aggr.names and dummy entries together
  xs = c(meas.aggr, no.meas)
  # test that aggr names get clipped and dummies are unchanged
  expected = c(meas.ids, no.meas)
  expect_equal(expected, simplifyMeasureNames(xs))

  # check measure ids are ignored too
  xs = c("acc", "no measure")
  expect_equal(xs, simplifyMeasureNames(xs))

  # check inputs of length 0
  xs = character(0L)
  expect_equal(xs, simplifyMeasureNames(xs))
})

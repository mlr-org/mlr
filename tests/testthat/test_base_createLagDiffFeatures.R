context("createLagDiffFeatures")

test_that("createLagDiffFeatures", {
  fcregr.xts.lag = createLagDiffFeatures(fcregr.xts,
                                         lag = 1:2L,
                                         difference.lag = 1L,
                                         difference = 2L)
  expect_equal(nrow(fcregr.xts),nrow(fcregr.xts.lag))
  fcregr.xts.lag.na = createLagDiffFeatures(fcregr.xts,
                                            lag = 1:2L,
                                            difference.lag = 1L,
                                            difference = 2L,
                                            na.pad = FALSE)
  expect_equal(nrow(fcregr.xts.lag.na), 296L)
  lag.task = createLagDiffFeatures(fcregr.task,
                                   lag = 1:2L,
                                   difference.lag = 1L,
                                   difference = 2L)
  # Have to convert to data frame
  expect_equal(lag.task$env$data, as.data.frame(fcregr.xts.lag))

  fcregr.xts.lag.seasonal = createLagDiffFeatures(fcregr.xts,
                                                  lag = 1:2L,
                                                  seasonal.lag = 1L,
                                                  frequency = 7L)
  expect_equal(colnames(fcregr.xts.lag.seasonal), c("test_data", "test_data_lag1_diff0",
                                                    "test_data_lag2_diff0", "test_data_lag7_diff0"))
})

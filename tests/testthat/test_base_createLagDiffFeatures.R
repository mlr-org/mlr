context("createLagDiffFeatures")

test_that("createLagDiffFeatures", {
  # Check for padding
  fcregr.df.lag = createLagDiffFeatures(fcregr.df[, 1, drop = FALSE],
    lag = 1:2L,
    difference.lag = 1L,
    difference = 2L,
    na.pad = TRUE,
    date.col = fcregr.df[, 2, drop = TRUE])
  expect_equal(nrow(fcregr.df), nrow(fcregr.df.lag))
  fcregr.df.lag.na = createLagDiffFeatures(fcregr.df[, 1, drop = FALSE],
    lag = 1:2L,
    difference.lag = 1L,
    difference = 2L,
    date.col = fcregr.df[, 2, drop = TRUE],
    target = "test_data")
  expect_equal(nrow(fcregr.df.lag.na), 298L)
  lag.task = createLagDiffFeatures(fcregr.task,
    lag = 1:2L,
    difference.lag = 1L,
    difference = 2L,
    na.pad = FALSE)
  # Have to convert to data frame
  expect_equal(getTaskData(lag.task), fcregr.df.lag.na)

  fcregr.df.lag.seasonal = createLagDiffFeatures(fcregr.df[, 1, drop = FALSE],
    lag = 1:2L,
    seasonal.lag = 1L,
    frequency = 7L,
    na.pad = TRUE,
    date.col = fcregr.df[, 2, drop = TRUE],
    target = "test_data")
  expect_equal(colnames(fcregr.df.lag.seasonal), c("test_data", "test_data_lag_1",
    "test_data_lag_2", "test_data_.lag._7", "yday_dt"))
})

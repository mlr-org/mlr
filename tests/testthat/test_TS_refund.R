context("TS_FPCA")

test_that("TS_FPCA", {
  gp = global_var4TS_gp
  taskTs = makeTimeSeriesClassifTask(data = gp, target = "X1", positive = "1")
  refData = getTaskData(taskTs, target.extra = TRUE)
  expect_true((nrow(refData$data) == nrow(gp)))
  expect_true((ncol(refData$data) == ncol(gp) - 1))
  f_df = getTSFPCAFeatures(data = refData$data, target = "X1", have.target = TRUE, include.target = FALSE)
  expect_true((nrow(f_df) == nrow(gp) ))
})

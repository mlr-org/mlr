context("FDA_FPCA")

test_that("FDA_FPCA", {
  gp = getTaskData(gunpoint.task, target.extra = FALSE)
  taskTs = makeFDAClassifTask(data = gp, target = "X1", positive = "1")
  refData = getTaskData(taskTs, target.extra = TRUE)
  expect_true((nrow(refData$data) == nrow(gp)))
  expect_true((ncol(refData$data) == ncol(gp) - 1))
  f_df = getFDAFPCAFeatures(data = refData$data, target = "X1", have.target = TRUE, include.target = FALSE)
  expect_true((nrow(f_df$feat) == nrow(gp) ))
})

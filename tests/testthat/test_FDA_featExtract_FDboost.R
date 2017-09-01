context("FDA_extractFeat_FDboost")

test_that("FDA_extractFeat_FDboost", {
  gp = data.frame(v1 = 1:5, v2 = 2:6, v3 = 3:7, v4 = 4:8, X1 = as.factor(c(-1,1,1,-1, 1)))
  taskTs = makeFDAClassifTask(data = gp, target = "X1", positive = "1")
  refData = getTaskData(taskTs, target.extra = TRUE)

  expect_true((nrow(refData$data) == nrow(gp)))
  expect_true((ncol(refData$data) == ncol(gp) - 1))

  f_df = getFDAFDboostFeatures(data = refData$data, target = "X1", have.target = TRUE,
    include.target = FALSE, bsignal.knots = 10L, bsignal.df = 3L)
  expect_true((nrow(f_df$feat) == nrow(gp) ))

})

context("extractFpcaFeatures")

test_that("extractFpcaFeatures works", {
  set.seed(getOption("mlr.debug.seed"))
  gp = getTaskData(gunpoint.task, subse = 1:20, target.extra = TRUE, functionals.as = "matrix")
  fpca.df = extractFpcaFeatures(data = gp$data, target = "X1", cols = "fd")
  expect_true((nrow(gp$data) == nrow(fpca.df)))
  expect_true((ncol(fpca.df) == 6L))
  expect_match(names(fpca.df), regexp = "[Fpca]")

  set.seed(getOption("mlr.debug.seed"))
  fpca.df2 = data.frame(refund::fpca.sc(Y = as.matrix(gp$data$fd))$scores)
  expect_true((nrow(gp$data) == nrow(fpca.df2)))
  expect_true((ncol(fpca.df2) == 7L))
  expect_equivalent(fpca.df, fpca.df2)

  set.seed(getOption("mlr.debug.seed"))
  gp = getTaskData(gunpoint.task, subset = 1:20, target.extra = TRUE, functionals.as = "matrix")
  fpca.df = extractFpcaFeatures(data = gp$data, target = "X1", cols = "fd", npc = 12L)
  expect_true((nrow(gp$data) == nrow(fpca.df)))
  expect_true((ncol(fpca.df) == 12L))
  expect_match(names(fpca.df), regexp = "[Fpca]")
})

context("conf matrix")

test_that("conf matrix", {
  rdesc = makeResampleDesc("CV", iters=3)
  r = resample(makeLearner("classif.rpart"), multiclass.task, rdesc)
  m = getConfMatrix(r$pred, relative = FALSE)
  expect_true(is.matrix(m) && nrow(m) == 4 && ncol(m) == 4)
	m = getConfMatrix(r$pred, relative = TRUE)
  expect_true(is.matrix(m) && nrow(m) == 4 && ncol(m) == 4)
})


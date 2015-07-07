context("tuneThreshold")

test_that("tuneThreshold", {

  # binary classes, 1 th
  lrn = makeLearner("classif.lda", predict.type="prob")
  m = train(lrn, binaryclass.task)
  p = predict(m, binaryclass.task)
  tr = tuneThreshold(p)
  expect_true(length(tr$th) == 1 && tr$th >= 0 && tr$th <= 1)
  expect_true(tr$perf >= 0 && tr$perf < 0.3)

  # multiclass
  m = train(lrn, multiclass.task)
  p = predict(m, multiclass.task)
  tr = tuneThreshold(p, mmce, control=list(maxit=5L))
  expect_true(length(tr$th) == 3 && all(tr$th >= 0) && all(tr$th <= 1))
  expect_true(tr$perf >= 0 && tr$perf < 0.1)
})



context("setThreshold")

test_that("setThreshold for fdaclassif", {
  lrn = makeLearner("classif.fdaknn", predict.type = "prob")
  mod = train(lrn, fda.binary.gp.task.small)

  # predict probabilities and compute performance
  pred = predict(mod, newdata = getTaskData(fda.binary.gp.task.small, keep.functionals = TRUE))
  p = performance(pred, measures = mmce)
  expect_identical(p[["mmce"]], 0.025)

  # adjust threshold and predict probabilities again
  threshold = c("1" = 0, "2" = 1)
  pred = setThreshold(pred, threshold = threshold)
  p = performance(pred, measures = mmce)
  expect_identical(p[["mmce"]], 0.5)

})

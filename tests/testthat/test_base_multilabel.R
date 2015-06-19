context("multilabel")

test_that("multilabel", {
  lrn = makeLearner("multilabel.rFerns")

  # train predict eval
  mod = train(lrn, multilabel.task)
  pred = predict(mod, multilabel.task)
  p = performance(pred)
  expect_true(!is.na(p))
  # with newdata df
  pred = predict(mod, newdata = multilabel.df)
  p = performance(pred)
  expect_true(!is.na(p))
  # resample
  r = holdout(lrn, multilabel.task)
  expect_true(!is.na(r$aggr))
  # probs
  lrn = setPredictType(lrn, "prob")
  mod = train(lrn, multilabel.task)
  pred = predict(mod, multilabel.task)
  print(str(pred$data))
})




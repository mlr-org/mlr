context("multilabel")

test_that("multilabel", {
  lrn = makeLearner("multilabel.rFerns")

  # train predict eval
  mod = train(lrn, multilabel.task)
  pred = predict(mod, multilabel.task)
  p = performance(pred)
  expect_true(!is.na(p))

  r = holdout(lrn, multilabel.task)
  expect_true(!is.na(r$aggr))

})




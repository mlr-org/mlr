context("regr_kmforrester")

test_that("regr_kmforrester", {
  library(DiceKriging)

  x.seq = c(1, 2, 3)
  dd = data.frame(x = x.seq, y = x.seq^2 + rnorm(length(x.seq)))

  rt = makeRegrTask(data = dd, target = "y")

  learner = makeLearner("regr.kmforrester", predict.type = "se")
  mod = train(learner = learner, task = rt)

  # check if standard error vanishes at the design points
  preds = predict(mod, newdata = data.frame(x = x.seq))
  expect_true(all(preds$se == 0))
})

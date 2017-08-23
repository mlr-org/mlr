context("FDA_multiResBoost")
test_that("FDA_multiResBoost", {
  p = 1000  # number of instances
  n  = 200 # length of each time serie instance
  Ts = replicate(p, rnorm(n))
  multiResBoost(X = Ts, y = as.matrix(replicate(1, rnorm(n))), M = 10,  res.level = 3L, shift = 0.5)
  # y = sample(c(-1, 1), replace = TRUE, n)
})

test_that("FDA_multiResBoostLearner", {
  lrn = makeLearner("fdaregr.multiResBoost", max_iter = 10,  res.level = 3L, shift = 0.5)
  mod1f = train(learner = lrn, task = fuelsubset.task)
  mdata = getTaskData(fuelsubset.task, target.extra = TRUE)
  mod1f = predict(object = mod1f, newdata = mdata$data)
})


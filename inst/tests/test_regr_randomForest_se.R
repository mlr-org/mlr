context("random_forest_standard_error")

test_that("standard error", {
  data(BostonHousing)

  n = nrow(BostonHousing)
  train.set.idx = sample(1:n, floor(n * 0.4))
  test.set.idx = setdiff(1:n, train.set.idx)
  train.set = BostonHousing[train.set.idx,]
  test.set = BostonHousing[test.set.idx,]
  one.point = BostonHousing[1, ]

  se.methods = c("bootstrap", "jackknife", "noisy.bootstrap")

  for (method in se.methods) {
    task = makeRegrTask(data=train.set, target = "medv")
    learner = makeLearner("regr.randomForest",
      predict.type="se",
      ntree = 20,
      ntree.for.se = 5L,
      se.method = method,
      nr.of.bootstrap.samples = 5L,
      keep.inbag = TRUE)
    model = train(learner, task)
    preds = predict(model, newdata=test.set)
    se.preds = preds$data$se
    expect_true(all(se.preds >= 0))
    preds = predict(model, newdata = one.point)
    se.preds = preds$data$se
    expect_true(all(se.preds >= 0))
  }
})


context("ConstantClassWrapper")

test_that("ConstantClassWrapper predicts with response", {
  lrn1 = makeLearner("classif.rpart")
  lrn2 = makeConstantClassWrapper(lrn1)

  # multiple classes present
  m1 = train(lrn1, multiclass.task, subset = multiclass.train.inds)
  m2 = train(lrn2, multiclass.task, subset = multiclass.train.inds)
  expect_false(inherits(m2, "FailureModel"))

  p1 = predict(m1, task = multiclass.task, subset = multiclass.test.inds)
  p2 = predict(m2, task = multiclass.task, subset = multiclass.test.inds)
  p1$time = 0
  p2$time = 0
  expect_equal(p1, p2)

  # one class present
  train.inds = 1:20
  try({
    suppressAll({
      train(lrn1, multiclass.task, subset = train.inds)
    })
    fail("Data has more than one class.")
  }, silent = TRUE)
  m2 = train(lrn2, multiclass.task, subset = train.inds)
  expect_false(inherits(m2, "FailureModel"))

  p2 = predict(m2, task = multiclass.task, subset = multiclass.test.inds)
  have = getPredictionResponse(p2)
  want = rep.int(unique(multiclass.df[train.inds, multiclass.target]), length(multiclass.test.inds))
  expect_equal(have, want)
})

test_that("ConstantClassWrapper predicts with frac", {
  lrn1 = makeLearner("classif.rpart")
  lrn2 = makeConstantClassWrapper(lrn1, frac = 0.1)

  train.inds = 1:51
  m1 = train(lrn1, multiclass.task, subset = multiclass.train.inds)
  m2 = train(lrn2, multiclass.task, subset = train.inds)
  expect_false(inherits(m2, "FailureModel"))

  p1 = predict(m1, task = multiclass.task, subset = multiclass.test.inds)
  p2 = predict(m2, task = multiclass.task, subset = multiclass.test.inds)
  expect_false(all(getPredictionResponse(p1) == getPredictionResponse(p2)))

  have = getPredictionResponse(p2)
  want = rep.int(multiclass.df[1, multiclass.target], length(multiclass.test.inds))
  expect_equal(have, want)
})

test_that("ConstantClassWrapper predicts with probs", {
  lrn1 = makeLearner("classif.rpart", predict.type = "prob")
  lrn2 = makeConstantClassWrapper(lrn1)

  # multiple classes present
  m1 = train(lrn1, multiclass.task, subset = multiclass.train.inds)
  m2 = train(lrn2, multiclass.task, subset = multiclass.train.inds)
  expect_false(inherits(m2, "FailureModel"))

  p1 = predict(m1, task = multiclass.task, subset = multiclass.test.inds)
  p2 = predict(m2, task = multiclass.task, subset = multiclass.test.inds)
  p1$time = 0
  p2$time = 0
  expect_equal(p1, p2)

  # one class present
  train.inds = 1:20
  try({
    suppressAll({
      train(lrn1, multiclass.task, subset = train.inds)
    })
    fail("Data has more than one class.")
  }, silent = TRUE)
  m2 = train(lrn2, multiclass.task, subset = train.inds)
  expect_false(inherits(m2, "FailureModel"))

  p2 = predict(m2, task = multiclass.task, subset = multiclass.test.inds)
  have = getPredictionResponse(p2)
  want = rep.int(unique(multiclass.df[train.inds, multiclass.target]), length(multiclass.test.inds))
  expect_equal(have, want)

  probs = getPredictionProbabilities(p2)
  sapply(names(probs), function(col) {
    prob = ifelse(col == unique(multiclass.df[train.inds, multiclass.target]), 1, 0)
    expect_true(all(probs[col] == prob))
  })
})

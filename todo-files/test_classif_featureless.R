context("classif_featureless")

test_that("classif_featureless", {
  # check content of learner model
  checkLearnerModel = function(mod, probs) {
    lmod = getLearnerModel(mod)
    expect_named(lmod, c("method", "probs"))
    expect_equal(lmod$method, m)
    expect_equal(as.numeric(lmod$probs), probs)
  }

  # multiclass with inequal class sizes
  df = data.frame(
    y = as.factor(c(1, 2, 3, 3, 3)),
    x = rep(1, 5)
  )
  lvls = levels(df$y)
  method = c("majority", "sample-prior")
  task = makeClassifTask(data = df, target = "y")
  # create dest data
  n = 10
  test = data.frame(rep(1, n))

  # determine probs and majority class manually
  probs = c(0.2, 0.2, 0.6)
  majority.class = 3

  for (m in method) {
    lrn = makeLearner("classif.featureless", method = m)
    mod = train(lrn, task)
    # test content of learner model
    checkLearnerModel(mod, probs)

    # create predictions using predict
    p = predict(mod, newdata = test)

    # create predictions "manually"
    if (m == "sample-prior") {
      # here we have to sample
      set.seed(getOption("mlr.debug.seed"))
      pred = factor(sample(lvls, size = n, prob = probs, replace = TRUE))
    } else {
      # here we know that 3 is majority class
      pred = factor(rep(majority.class, n), levels = lvls)
    }
    # test if predictions are the same as the manually created ones
    expect_equal(getPredictionResponse(p), pred)

    # test that printer works correctly
    expect_output(print(lrn), "featureless")
    expect_output(print(lrn), m)

    lrn = setPredictType(lrn, predict.type = "prob")
    mod = train(lrn, task)
    p = predict(mod, newdata = test)
    expect_equal(as.numeric(unique(getPredictionProbabilities(p))), probs)
  }

  # binaryclass with qual class sizes
  df = data.frame(
    y = as.factor(c(1, 2, 1, 2)),
    x = rep(1, 4)
  )
  lvls = levels(df$y)
  method = c("majority", "sample-prior")
  task = makeClassifTask(data = df, target = "y")

  # determine probs manually
  probs = c(0.5, 0.5)

  for (m in method) {
    lrn = makeLearner("classif.featureless", method = m)
    mod = train(lrn, task)
    # test content of learner model
    checkLearnerModel(mod, probs)

    # create predictions using predict
    p = predict(mod, newdata = test)

    # create predictions "manually"
    set.seed(getOption("mlr.debug.seed"))
    if (m == "sample-prior") {
      # here we have to sample
      pred = factor(sample(lvls, size = n, prob = probs, replace = TRUE))
    } else {
      # here we don't know the majority class and sample it randomly
      pred = factor(rep(getMaxIndex(probs, ties.method = "random"), n), levels = lvls)
    }
    # test if predictions are the same as the manually created ones
    expect_equal(getPredictionResponse(p), pred)

    # test that printer works correctly
    expect_output(print(lrn), "featureless")
    expect_output(print(lrn), m)

    # test predict.type prob
    lrn = setPredictType(lrn, predict.type = "prob")
    mod = train(lrn, task)
    p = predict(mod, newdata = test)
    expect_equal(getPredictionProbabilities(p), rep(0.5, n))
  }
})

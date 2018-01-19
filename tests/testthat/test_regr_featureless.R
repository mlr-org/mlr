context("regr_featureless")

test_that("regr_featureless", {
  df = data.frame(
    y = c(1, 2, 3, 3, 3),
    x = rep(1, 5)
  )
  method = c("mean", "median")
  task = makeRegrTask(data = df, target = "y")

  # compute predictions manually
  expected.response = list(
    median = 3,
    mean = (1 + 2 + 3 + 3 + 3) / 5
  )

  for (m in method) {
    lrn = makeLearner("regr.featureless", method = m)
    mod = train(lrn, task)
    # test content of learner model
    expect_equal(getLearnerModel(mod)$response, expected.response[[m]])
    # test prediction works properly
    n = 10
    test = data.frame(rep(1, n))
    p = predict(mod, newdata = test)
    expect_equal(getPredictionResponse(p), rep(expected.response[[m]], n))
    # test that printer works correctly
    expect_output(print(lrn), "featureless")
    expect_output(print(lrn), m)
  }
})

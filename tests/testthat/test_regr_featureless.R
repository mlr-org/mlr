context("regr_featureless")

test_that("regr_featureless", {
  df = data.frame(
    y = c(1, 2, 3, 3, 3),
    x = rep(1, 5)
  )
  method = c("mean", "median")
  expected.response = list(
    median = 3,
    mean = (1 + 2 + 3 + 3 + 3)/5
  )

  task = makeRegrTask(data = df, target = "y")

  # test content of learner model
  for (m in method) {
    lrn = makeLearner("regr.featureless", method = m)
    mod = train(lrn, task)
    expect_equal(getLearnerModel(mod)$response, expected.response[[m]])
  }

  # test that printers work correctly
  expect_output(print(lrn), "featureless")
})

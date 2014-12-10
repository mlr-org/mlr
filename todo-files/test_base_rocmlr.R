context("roc from mlr")

test_that("roc coords work", {

  makePred = function(truth, prob) {
    truth = as.factor(truth)
    prob = data.frame(b = prob, a = 1 - prob)
    data = data.frame(x = rnorm(length(truth)), y = truth)
    task = makeClassifTask(data = data, target = "y", positive = "b")
    makePrediction(task$task.desc, NULL, id = "foo", predict.type = "prob",
      truth = truth, y = prob, time = 0)
  }

  p = makePred(c("a", "b", "a"), c(0.7, 0.3, 0.1))
  rc = getROCCoords(p)

  expect_equal(rc$data, as.data.frame(matrix(c(
    1.0, 0.0, 0.0,
    0.7, 0.0, 0.5,
    0.3, 1.0, 0.5,
    0.1, 1.0, 1.0,
    0.0, 1.0, 1.0
  ), byrow = TRUE, ncol = 3L)), check.names = FALSE)

  p = makePred(c("a", "b", "b", "a"), c(0.7, 0.3, 0.3, 0.1))
  rc = getROCCoords(p)

  expect_equal(rc$data, as.data.frame(matrix(c(
    1.0, 0.0, 0.0,
    0.7, 0.0, 0.5,
    0.3, 1.0, 0.5,
    0.1, 1.0, 1.0,
    0.0, 1.0, 1.0
  ), byrow = TRUE, ncol = 3L)), check.names = FALSE)

  p = makePred(c("b", "a", "b", "b", "a", "b"), c(0.7, 0.7, 0.3, 0.3, 0.1, 0.1))
  rc = getROCCoords(p)

  expect_equal(rc$data, as.data.frame(matrix(c(
    1.0, 0.0, 0.0,
    0.7, 1/4, 0.5,
    0.3, 3/4, 0.5,
    0.1, 1.0, 1.0,
    0.0, 1.0, 1.0
  ), byrow = TRUE, ncol = 3L)), check.names = FALSE)
})



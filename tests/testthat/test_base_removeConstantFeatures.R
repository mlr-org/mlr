context("removeConstantFeatures")

test_that("removeConstantFeatures", {
  data = data.frame(
    a = c(1L, 2L),
    b = as.factor(1:2),
    c = c("a", "b"),
    d = as.factor(c(TRUE, FALSE)),
    e = c(NA, 1),
    f = c(1, NA),
    g = c(1, 1),
    n = c(0, 1 - 0.7 - 0.3),
    m = as.double(c(NA, NA)), # only missings are supported?
    target = as.factor(1:2)
  )
  data = data[c(rep(1, 9), 2), ]
  data$safe = seq_row(data)
  task = makeClassifTask("test", data = data, target = "target")

  res1 = getTaskData(removeConstantFeatures(task, perc = 0.1, dont.rm = "g"))
  res2 = removeConstantFeatures(getTaskData(task), perc = 0.1, dont.rm = c("g", "target"))
  expect_equal(colnames(res1), c("g", "target", "safe"))
  expect_equal(res1, res2)

  res = getTaskData(removeConstantFeatures(task, na.ignore = TRUE))
  expect_equal(colnames(res), c("a", "b", "c", "d", "target", "safe"))

  res = getTaskData(removeConstantFeatures(task, tol = 0, na.ignore = TRUE))
  expect_true(setequal(colnames(res), c("a", "b", "c", "d", "target", "safe", "n")))

  res = getTaskData(removeConstantFeatures(task, na.ignore = FALSE))
  expect_true(setequal(names(res), c("a", "b", "c", "d", "e", "f", "target", "safe")))

  res = getTaskData(removeConstantFeatures(task, na.ignore = FALSE, perc = 0.2))
  expect_equal(colnames(res), c("target", "safe"))


  data = dropNamed(data, c("e", "f", "m"))
  data$target = 1
  data$noise = rnorm(nrow(data))
  lrn = makeLearner("regr.lm")
  lrn = makeRemoveConstantFeaturesWrapper(lrn, perc = 0.1)
  task = makeRegrTask(data = data, target = "target")
  model = train(lrn, task)
  mod = getLearnerModel(model)$learner.model
  expect_true(setequal(names(coef(mod)), c("(Intercept)", "noise", "safe")))
})

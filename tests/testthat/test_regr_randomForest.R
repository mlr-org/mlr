context("regr_randomForest")

test_that("regr_randomForest", {
  requirePackagesOrSkip("randomForest", default.method = "load")

  parset.list = list(
    list(ntree = 5, mtry = 2),
    list(ntree = 5, mtry = 4),
    list(ntree = 5, proximity = TRUE, oob.prox = TRUE),
    list(ntree = 5, nPerm = 3)
  )

  old.predicts.list = list()

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    pars = list(formula = regr.formula, data = regr.train)
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(randomForest::randomForest, pars)
    p = predict(m, newdata = regr.test, type = "response")
    old.predicts.list[[i]] = p
  }

  testSimpleParsets("regr.randomForest", regr.df, regr.target,
    regr.train.inds, old.predicts.list, parset.list)

  tt = randomForest::randomForest

  testCVParsets("regr.randomForest", regr.df, regr.target, tune.train = tt,
    parset.list = parset.list)
})


test_that("fix factors work", {
  data(iris)
  n = nrow(iris)
  data = iris
  train = sample(1:n, floor(n * 0.9))
  test = setdiff(1:n, train)

  task = makeRegrTask(data = data[train, ], target = "Sepal.Length")
  learner = makeLearner("regr.randomForest", fix.factors.prediction = TRUE)
  model = train(learner, task)
  newdata = data[head(test, 1L), ]
  newdata$Species = droplevels(newdata$Species)
  expect_is(predict(model, newdata = newdata), "Prediction")
})

test_that("different se.methods work", {
  se.methods = c("bootstrap", "jackknife", "sd")
  preds = setNames(vector("list", length(se.methods)), se.methods)
  for (se.method in se.methods) {
    keep.inbag = se.method == "jackknife"
    par.vals = list(se.method = se.method, ntree = 10L, keep.inbag = keep.inbag)
    if (se.method == "bootstrap") {
      par.vals = c(par.vals, list(se.ntree = 5L, se.boot = 3L))
    }
    learner = makeLearner("regr.randomForest", predict.type = "se",
      par.vals = par.vals)
    model = train(learner, task = bh.task, subset = 1:500)

    preds[[se.method]] = predict(model, task = bh.task)
    expect_true(is.numeric(preds[[se.method]]$data$se))
    expect_true(all(preds[[se.method]]$data$se >= 0))

    # test if it works with one row
    pred.one = predict(model, task = bh.task, subset = 501)
    expect_true(is.numeric(pred.one$data$se))
    expect_true(all(pred.one$data$se >= 0))
  }
  # mean prediction should be unaffected from the se.method
  expect_equal(preds$bootstrap$data$response, preds$sd$data$response)
  expect_equal(preds$sd$data$response, preds$jackknife$data$response)
})


test_that("dplyr data.frames work", {
  data("mpg", package = "ggplot2")
  mpg$model = NULL
  for (cname in colnames(mpg)[sapply(mpg, is.character)]) {
    mpg[[cname]] = as.factor(mpg[[cname]])
  }
  expect_warning((task.mpg = makeRegrTask(data = mpg, target = "cty")),
    "Provided data is not a pure data.frame but from class")
  lrn = makeLearner("regr.randomForest", ntree = 2)
  train(lrn, task.mpg)
})

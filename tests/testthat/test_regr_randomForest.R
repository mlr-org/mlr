context("regr_randomForest")

test_that("regr_randomForest", {
  requirePackagesOrSkip("randomForest", default.method = "load")

  parset.list = list(
    list(),
    list(ntree = 5, mtry = 2),
    list(ntree = 5, mtry = 4)
  )

  old.predicts.list = list()

  for (i in 1:length(parset.list)) {
    parset = parset.list[[i]]
    pars = list(formula = regr.formula, data = regr.train)
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(randomForest::randomForest, pars)
    set.seed(getOption("mlr.debug.seed"))
    p = predict(m, newdata = regr.test, type = "response")
    old.predicts.list[[i]] = p
  }

  testSimpleParsets("regr.randomForest", regr.df, regr.target,
    regr.train.inds, old.predicts.list, parset.list)

  tt = randomForest::randomForest

  testCVParsets("regr.randomForest", regr.df, regr.target, tune.train = tt, parset.list = parset.list)
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
  se.methods = c("bootstrap", "jackknife")
  for (se.method in se.methods) {
    keep.inbag = se.method %in% c("jackknife")
    learner = makeLearner("regr.randomForest", predict.type = "se", se.method = se.method, ntree = 10L, keep.inbag = keep.inbag)
    model = train(learner, task = regr.task, subset = regr.train.inds)

    pred.all = predict(model, task = regr.task, subset = 1)
    expect_true(is.numeric(pred.all$data$se))
    expect_true(all(pred.all$data$se >= 0))

    pred.one = predict(model, task = regr.task, subset = 1)
    expect_true(is.numeric(pred.one$data$se))
    expect_true(all(pred.one$data$se >= 0))
  }
})


test_that("dplyr data.frames work", {
  data("mpg", package = "ggplot2")
  mpg$model = NULL
  for (cname in colnames(mpg)[sapply(mpg, is.character)])
    mpg[[cname]] = as.factor(mpg[[cname]])
  expect_warning((task_mpg = makeRegrTask(data = mpg, target = "cty")), "Provided data is not a pure data.frame but from class")
  lrn = makeLearner("regr.randomForest", ntree = 2)
  train(lrn, task_mpg)
})

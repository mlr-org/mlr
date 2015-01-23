context("regr_randomForest")

test_that("regr_randomForest", {
  requirePackages("randomForest", default.method = "load")
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
  learner = makeLearner("regr.randomForest", fix.factors = TRUE)
  model = train(learner, task)
  newdata = data[head(test, 1L), ]
  newdata$Species = droplevels(newdata$Species)
  expect_is(predict(model, newdata = newdata), "Prediction")
})

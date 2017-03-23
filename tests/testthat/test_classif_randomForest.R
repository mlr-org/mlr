context("classif_randomForest")

test_that("classif_randomForest", {
  requirePackagesOrSkip("randomForest", default.method = "load")
  parset.list = list(
    list(),
    list(ntree = 50,  mtry = 2),
    list(ntree = 50, mtry = 4),
    list(ntree = 200, mtry = 2),
    list(ntree = 2000, mtry = 4, proximity = TRUE, oob.prox = TRUE)
  )

  old.predicts.list = list()
  old.probs.list = list()

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    pars = list(formula = multiclass.formula, data = multiclass.train)
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(randomForest::randomForest, pars)
    set.seed(getOption("mlr.debug.seed"))
    p = predict(m, newdata = multiclass.test, type = "response")
    set.seed(getOption("mlr.debug.seed"))
    p2 = predict(m, newdata = multiclass.test, type = "prob")
    old.predicts.list[[i]] = p
    old.probs.list[[i]] = p2
  }

  testSimpleParsets("classif.randomForest", multiclass.df, multiclass.target,
    multiclass.train.inds, old.predicts.list, parset.list)
  testProbParsets("classif.randomForest", multiclass.df, multiclass.target,
    multiclass.train.inds, old.probs.list, parset.list)

  tt = randomForest::randomForest

  testCVParsets("classif.randomForest", multiclass.df, multiclass.target, tune.train = tt, parset.list = parset.list)

  # FIXME test RF with one constant feature
  #data = multiclass.df
  #data = data[, c(1,5)]
  #data[, 1] = 1
  #task = makeClassifTask(data=data, target=multiclass.target)
  #m = train(makeLearner("classif.randomForest"), task)
  #p = predict(m, task=task)
})

test_that("fix factors work", {
  data(iris)
  n = nrow(iris)
  data = iris
  train = sample(1:n, floor(n * 0.9))
  test = setdiff(1:n, train)

  data$x = factor(sample(letters[1:3], n, replace = TRUE))
  task = makeClassifTask(data = data[train, ], target = "Species")
  learner = makeLearner("classif.randomForest", fix.factors.prediction = TRUE)
  model = train(learner, task)
  newdata = data[head(test, 1L), ]
  newdata$Species = droplevels(newdata$Species)
  expect_is(predict(model, newdata = newdata), "Prediction")
})

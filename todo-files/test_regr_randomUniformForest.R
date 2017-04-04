context("regr_randomUniformForest")

test_that("regr_randomUniformForest", {
  skip_on_travis() # FIXME: I dont know why this breaks on travis
  requirePackages("randomUniformForest", default.method = "load")

  parset.list = list(
    list(ntree = 5, mtry = 4)
  )

  tsk.train = makeRegrTask(data = regr.train, target = regr.target)
  tsk.test = makeRegrTask(data = regr.test, target = regr.target)

  for (i in seq_along(parset.list)) {
    parset = c(list(formula = regr.formula, data = regr.train, OOB = FALSE,
    importance = FALSE, unsupervised = FALSE, threads = 1L), parset.list[[i]])
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(randomUniformForest::randomUniformForest, parset)
    old.predicts = predict(m, regr.test)

    lrn = do.call("makeLearner", c("regr.randomUniformForest", parset.list[[i]]))
    set.seed(getOption("mlr.debug.seed"))
    trained.mod = train(lrn, tsk.train)
    new.predicts = predict(trained.mod, tsk.test)$data$response

    #randomUniformForest is such randomized that using the same seed will produce different results on
    #the same data, see vignette("randomUniformForestsOverview") on page 22.

    expect_true(length(old.predicts) == length(new.predicts))
  }
})

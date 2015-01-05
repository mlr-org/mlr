context("classif_randomUniformForest")

test_that("classif_randomUniformForest", {
  skip("breaks on travis")
  library(randomUniformForest)

  parset.list = list(
    list(),
    list(ntree = 5, mtry = 4)
  )

  for (i in 1:length(parset.list)) {
    parset = c(list(formula = binaryclass.formula, data = binaryclass.train, OOB = FALSE,
    importance = FALSE, unsupervised = FALSE, threads = 1L), parset.list[[i]])
    set.seed(getOption("mlr.debug.seed"))
    capture.output({m = do.call(randomUniformForest, parset)})
    old.predicts = predict(m, binaryclass.test)

    lrn = do.call("makeLearner", c("classif.randomUniformForest", parset.list[[i]]))
    set.seed(getOption("mlr.debug.seed"))
    trained.mod = train(lrn, binaryclass.task, binaryclass.train.inds)
    new.predicts = predict(trained.mod, binaryclass.task, subset = binaryclass.test.inds)$data$response

    #randomUniformForest is such randomized that using the same seed will produce different results on
    #the same data, see vignette("randomUniformForestsOverview") on page 22.

    expect_true(length(old.predicts) == length(new.predicts))
  }
})

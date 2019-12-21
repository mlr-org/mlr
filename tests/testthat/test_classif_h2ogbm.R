context("classif_h2ogbm")

test_that("classif_h2ogbm", {
  skip_on_ci()
  requirePackages("h2o", default.method = "load")
  h2o::h2o.init()

  parset.list = list(
    list(),
    list(ntrees = 5L),
    list(ntrees = 5L, min_rows = 5L),
    list(ntrees = 5L, nbins = 2L)
  )
  old.probs.list = list()

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    parset = c(parset, list(x = colnames(binaryclass.train[, -binaryclass.class.col]),
      y = binaryclass.target,
      training_frame = h2o::as.h2o(binaryclass.train)))
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(h2o::h2o.gbm, parset)
    p = predict(m, newdata = h2o::as.h2o(binaryclass.test))
    old.probs.list[[i]] = as.data.frame(p)[, 2L]
  }

  testProbParsets("classif.h2o.gbm", binaryclass.df, binaryclass.target,
    binaryclass.train.inds, old.probs.list, parset.list)
})

test_that("class names are integers and probabilities predicted (#1787)", {
  skip_on_ci()
  df = data.frame(matrix(runif(100, 0, 1), 100, 9))
  classx = factor(sample(c(0, 1), 100, replace = TRUE))
  df = cbind(classx, df)

  classif.task = makeClassifTask(id = "example", data = df, target = "classx")
  gb.lrn = makeLearner("classif.h2o.gbm", predict.type = "prob")
  rdesc = makeResampleDesc("CV", iters = 3, stratify = TRUE)
  rin = makeResampleInstance(rdesc, task = classif.task)
  r = resample(gb.lrn, classif.task, rin)
  expect_false(is.null(r$pred))
})

test_that("feature importances are returned", {
  skip_on_ci()
  iris2 = iris[iris$Species %in% c("versicolor", "virginica"), ]
  iris2$Species = droplevels(iris2$Species)
  task = makeClassifTask(data = iris2, target = "Species")

  lrn = makeLearner("classif.h2o.gbm")
  mod = train(lrn, task)
  feat.imp = getFeatureImportance(mod)$res
  feat.imp.h2o = h2o::h2o.varimp(getLearnerModel(mod))[, c("variable", "relative_importance")]
  # Convert to data.frame with same structure for equality check
  feat.imp.h2o = data.frame(as.list(xtabs(relative_importance ~ variable,
    data = feat.imp.h2o)))[names(feat.imp)]

  expect_equal(feat.imp, feat.imp.h2o)
})

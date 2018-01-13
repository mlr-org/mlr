context("PreprocWrapperCaret")

test_that("basic PreprocWrapperCaret works", {
  lrn1 = makeLearner("classif.rpart")
  lrn2 = makePreprocWrapperCaret(lrn1)
  m = train(lrn2, multiclass.task, subset = multiclass.train.inds)
  p = predict(m, subsetTask(multiclass.task, subset = multiclass.test.inds))
  perf = performance(p, mmce)
  expect_true(perf < 0.1)

  lrn3 = makePreprocWrapperCaret(lrn1, ppc.BoxCox = TRUE, ppc.YeoJohnson = FALSE, ppc.pca = TRUE,
    ppc.pcaComp = 2, ppc.scale = TRUE, ppc.center = TRUE)
  m = train(lrn3, multiclass.task, subset = multiclass.train.inds)
  ctrl = m$learner.model$control
  p = predict(m, subsetTask(multiclass.task, subset = multiclass.test.inds))
  perf2 = performance(p, mmce)
  expect_true(perf2 > perf)

  mod = caret::preProcess(x = multiclass.df[multiclass.train.inds, 1:4],
    method = c("BoxCox", "pca", "scale", "center"), pcaComp = 2)
  mod$method = mod$method[order(names(mod$method))]
  ctrl$method = ctrl$method[order(names(ctrl$method))]
  mod$call = NULL
  ctrl$call = NULL
  expect_equal(mod, ctrl)
})

test_that("PreprocWrapperCaret supports missing values", {
  lrn1 = makeLearner("classif.svm")
  lrn2 = makePreprocWrapperCaret(lrn1, ppc.knnImpute = TRUE)
  expect_true(hasLearnerProperties(lrn2, props = "missings"))
})

test_that("PreprocessWrapperCaret supports nzv,zv and corr method with different methodparams as default", {
  lrn1 = makeLearner("classif.rpart")
  mod2 = caret::preProcess(x = multiclass.df[multiclass.train.inds, 1:4], method = c("nzv", "zv", "corr"),
    freqCut = 100 / 5, uniqeCut = 9.5, cutoff = 0.8)
  lrn4 = makePreprocWrapperCaret(lrn1, ppc.center = FALSE, ppc.scale = FALSE, ppc.na.remove = FALSE,
    ppc.nzv = TRUE, ppc.zv = TRUE, ppc.corr = TRUE, ppc.freqCut = 100 / 5,
    ppc.uniqeCut = 9.5, cutoff = 0.8)
  m4 = train(lrn4, multiclass.task, subset = multiclass.train.inds)
  ctrl4 = m4$learner.model$control
  mod2$method = mod2$method[order(names(mod2$method))]
  ctrl4$method = ctrl4$method[order(names(ctrl4$method))]
  mod2$call = NULL
  ctrl4$call = NULL
  expect_equal(mod2, ctrl4)
})

test_that("PreprocessWrapperCaret creates the same output as preProcess for methods nzv, zv", {
  lrn1 = makeLearner("classif.rpart")
  df = multiclass.df[multiclass.train.inds, ]
  # Include fake column which should be removed
  df$Fake = c(rep(1, nrow(df) - 1), 0)
  mod2 = caret::preProcess(x = df, method = "nzv", freqCut = 100 / 5, uniqeCut = 9.5)
  out2 = predict(mod2, df)

  carettask = makeClassifTask(id = "multiclass.iris", data = df, target = "Species")
  lrn4 = makePreprocWrapperCaret(lrn1, ppc.center = FALSE, ppc.scale = FALSE, ppc.na.remove = FALSE,
    ppc.nzv = TRUE, ppc.zv = TRUE, ppc.freqCut = 100 / 5, ppc.uniqeCut = 9.5)
  out.transformed.mlr = lrn4$train(data = df, target = "Species", args = lrn4$par.vals)$data
  expect_equal(out2, out.transformed.mlr)
})

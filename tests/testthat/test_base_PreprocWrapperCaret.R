context("PreprocWrapperCaret")

test_that("basic PreprocWrapperCaret works", {
  lrn1 = makeLearner("classif.rpart")
  lrn2 = makePreprocWrapperCaret(lrn1)
  m = train(lrn2, multiclass.task, subset = multiclass.train.inds)
  p = predict(m, subsetTask(multiclass.task, subset = multiclass.test.inds))
  perf = performance(p, mmce)
  expect_true(perf < 0.1)

  lrn3 = makePreprocWrapperCaret(lrn1, ppc.BoxCox = TRUE, ppc.YeoJohnson = FALSE, ppc.pca = TRUE, ppc.pcaComp = 2, ppc.scale = TRUE, ppc.center = TRUE)
  m = train(lrn3, multiclass.task, subset = multiclass.train.inds)
  ctrl = m$learner.model$control
  p = predict(m, subsetTask(multiclass.task, subset = multiclass.test.inds))
  perf = performance(p, mmce)
  expect_true(perf < 0.1)


#   mod = caret::preProcess(x = multiclass.df[multiclass.train.inds,1:4], method = c("BoxCox", "pca", "scale", "center"), pcaComp = 2)
#   FIXME: fails and looks pretty different
#   expect_equal(mod, ctrl)
})

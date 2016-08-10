context("FeatSelWrapper")

test_that("FeatSelWrapper", {
  outer = makeResampleDesc("CV", iters = 2L)
  inner = makeResampleDesc("Holdout")

  lrn1 = makeLearner("classif.rpart")
  ctrl = makeFeatSelControlRandom(maxit = 3)
  lrn2 = makeFeatSelWrapper(lrn1, resampling = inner, control = ctrl)

  r = resample(lrn2, multiclass.task, outer, extract = function(model) {
    getFeatSelResult(model)
  })
  expect_true(!is.na(r$aggr[[1]]))
  feats = extractSubList(r$extract, "x", simplify = FALSE)
  expect_true(is.list(feats) && length(feats) == 2L && all(sapply(feats, is.character)))
  perfs = extractSubList(r$extract, "y")
  expect_true(is.numeric(perfs) && length(perfs) == 2L && !any(is.na(perfs)))
})

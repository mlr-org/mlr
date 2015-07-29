context("FeatSelWrapper")

test_that("FeatSelWrapper", {
  outer = makeResampleDesc("CV", iters = 2L)
  inner = makeResampleDesc("Holdout")

  lrn1 = makeLearner("classif.rpart")
  ctrl = makeFeatSelControlRandom(maxit = 3)
  lrn2 = makeFeatSelWrapper(lrn1, resampling = inner, control = ctrl)

  r = resample(lrn2, multiclass.task, outer)
  expect_true(!is.na(r$aggr[[1]]))
  feats = extractSubList(r$extract, list(1, "x"), simplify = FALSE)
  expect_true(is.list(feats) && length(feats) == 2L && all(sapply(feats, is.character)))
  perfs = extractSubList(r$extract, list(1, "y"))
  expect_true(is.numeric(perfs) && length(perfs) == 2L && !any(is.na(perfs)))
  expect_class(r$extract[[1]][[1]], "FeatSelResult")
})


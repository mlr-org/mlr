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
  expect_true(is.list(feats) && length(feats) == 2L &&
    all(sapply(feats, is.character)))
  perfs = extractSubList(r$extract, "y")
  expect_true(is.numeric(perfs) && length(perfs) == 2L && !any(is.na(perfs)))
})

test_that("FeatSelWrapper works with custom bits", {
  bns = c("b1", "b2")
  btf = function(x, task) {
    fns = getTaskFeatureNames(task)
    Reduce(c, list(fns[1:2], fns[3:4])[as.logical(x)], init = character(0))
  }

  lrn1 = makeLearner("classif.rpart")
  ctrl = makeFeatSelControlRandom(maxit = 3)
  lrn2 = makeFeatSelWrapper(lrn1, resampling = makeResampleDesc("Holdout"),
    control = ctrl, bit.names = bns, bits.to.features = btf)

  r = resample(lrn2, multiclass.task, cv2, extract = function(model) {
    getFeatSelResult(model)
  })
  expect_true(!is.na(r$aggr[[1]]))
  feats = extractSubList(r$extract, "x", simplify = FALSE)
  expect_true(is.list(feats) && length(feats) == 2L &&
    all(sapply(feats, is.character)))
  bit.names = extractSubList(r$extract, "x.bit.names", simplify = FALSE)
  expect_true(is.list(bit.names) && length(bit.names) == 2L &&
    all(sapply(feats, is.character)))
})

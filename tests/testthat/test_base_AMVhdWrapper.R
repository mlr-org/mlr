context("oneclass_AMVhdWrapper")

test_that("AMVhdWrapper", {
  # creates data
  set.seed(1234)
  sigma = matrix(0, 9, 9)
  diag(sigma) = c(4, 5, 8, 3, 2, 6, 9, 3, 1)
  normal = MASS::mvrnorm(n = 1000, rep(0, 9), sigma)
  colnames(normal) = paste0("V", 1:9)
  normal = as.data.frame(normal)
  normal$normal = TRUE

  anomaly = matrix(sample(size = 50 * 9, x = 20:100, replace = TRUE), 50, 9)
  colnames(anomaly) = paste0("V", 1:9)
  anomaly = as.data.frame(anomaly)
  anomaly$normal = FALSE
  data = rbind(normal, anomaly)
  data = na.omit(data)

  # create train and test sets
  inds.split = chunk(seq_len(nrow(data)), shuffle = TRUE, props = c(0.6, 0.4))
  train.inds = inds.split[[1]]
  test.inds = inds.split[[2]]

  task = makeOneClassTask(data = data, target = "normal", positive = "TRUE", negative = "FALSE")
  # base learner
  lrn = makeLearner("oneclass.svm", predict.type = "prob")


  # wrapped learner, with 3 feature subsample for each of the 10 iteration
  feats = 3
  iters = 10
  lrn.amww = makeAMVhdWrapper(lrn, amv.iters = iters, amv.feats = feats)

  # wrapped model
  mod.amww = train(lrn.amww, task, subset = train.inds)
  expect_is(mod.amww, "AMVhdModel")
  expect_is(mod.amww, "HomogeneousEnsembleModel")
  expect_is(mod.amww, "BaseWrapperModel")
  expect_is(mod.amww, "WrappedModel")
  expect_true(!inherits(mod.amww, "FailureModel"))


  # list all submodels, first list element is the full model
  submod = getLearnerModel(mod.amww, more.unwrap = FALSE)
  expect_true(is.list(submod) && length(submod) == (lrn.amww$par.vals$amv.iters + 1))
  expect_true(inherits(submod[[1L]], "WrappedModel"))
  expect_true(length(submod[[1L]]$features) == (ncol(data) - 1))
  expect_true(length(submod[[2L]]$features) == feats)
  expect_equal(unique(sapply(extractSubList(submod, "subset", simplify = FALSE), length)), length(train.inds))
  expect_equal(unique(sapply(extractSubList(submod, "features", simplify = FALSE), length)), c(9L, feats))

  submod.unwrap = getLearnerModel(mod.amww, more.unwrap = TRUE)
  expect_true(is.list(submod.unwrap) && length(submod.unwrap) == (lrn.amww$par.vals$amv.iters + 1))
  expect_true(inherits(submod.unwrap[[1L]], "svm"))

  # wrapped prediction
  pred.amww = predict(mod.amww, task, subset = test.inds)
  # t contains the prediction with subsampled features
  t = attr(pred.amww, "AMVhdSubpredict")

  expect_is(pred.amww, "PredictionAMVhd")
  expect_equal(names(attributes(pred.amww)), c("names", "class", "AMVhdSubpredict"))
  expect_equal(length(t), 10)
  expect_equal(pred.amww$n.subfeat, 9)
  expect_equal(t[[1]]$n.subfeat, 3)
})


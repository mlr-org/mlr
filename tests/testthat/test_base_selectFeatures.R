context("selectFeatures")
# check some basic stuff here, the rest is done in test_featsel_

test_that("selectFeatures", {
  rdesc = makeResampleDesc("Holdout")
  lrn = makeLearner("classif.lda")

  ctrl = makeFeatSelControlRandom(maxit = 2L)
  fr = selectFeatures(lrn, task = multiclass.task, resampling = rdesc,
    control = ctrl, show.info = FALSE,
    measures = getDefaultMeasure(multiclass.task))
  expect_equal(getOptPathLength(fr$opt.path), 2L)
  expect_equal(nrow(as.data.frame(fr$opt.path)), 2L)
  expect_equal(ncol(as.data.frame(fr$opt.path)), 9L)

  # test printing
  expect_output(print(ctrl), "Imputation value: <worst>")
  ctrl$impute.val = 10
  expect_output(print(ctrl), "Imputation value: 10")
  expect_output(print(fr), "mmce.test.mean")

  # check empty model
  ctrl = makeFeatSelControlSequential(method = "sfs", alpha = 10)
  fr = selectFeatures(lrn, task = multiclass.task, resampling = rdesc,
    control = ctrl, show.info = FALSE)
  expect_equal(fr$x, character(0))

  # check bits.to.features
  bns = c("b1", "b2")
  btf = function(x, task) {
    fns = getTaskFeatureNames(task)
    Reduce(c, list(fns[1:2], fns[3:4])[as.logical(x)], init = character(0))
  }
  ctrl = makeFeatSelControlRandom(maxit = 2L)
  fr = selectFeatures(lrn, task = multiclass.task, resampling = rdesc, bit.names = bns,
    bits.to.features = btf, control = ctrl, show.info = FALSE)
  df = as.data.frame(fr$opt.path)
  expect_true(setequal(colnames(df), c("b1", "b2", "mmce.test.mean", "dob", "eol",
    "exec.time", "error.message")))
  expect_equal(nrow(df), 2L)

  expect_error(selectFeatures(lrn, task = multiclass.task, resampling = rdesc,
    bit.names = bns, control = ctrl, show.info = FALSE), "you also have to set bits.to.features")

  expect_error(selectFeatures(lrn, task = multiclass.task, resampling = rdesc,
    bit.names = bns, bits.to.features = function(x, task) binaryToFeatures(x, getTaskFeatureNames(task)), control = ctrl, show.info = FALSE), "Must have length")

  expect_error(selectFeatures(lrn, task = multiclass.task, resampling = rdesc,
    bits.to.features = function(x, task) c("test1", "test2"), control = ctrl, show.info = FALSE), "\\(test1,test2\\) that are not in the task.")

  expect_error(selectFeatures(lrn, task = multiclass.task, resampling = rdesc,
    bits.to.features = function(x, task) NULL, control = ctrl, show.info = FALSE), "but an object of type NULL")
})

test_that("threshold tuning with feature selection", {
  rdesc = makeResampleDesc("Holdout")
  lrn = makeLearner("classif.rpart", predict.type = "prob")
  ctrl = makeFeatSelControlRandom(maxit = 2L, tune.threshold = TRUE,
    tune.threshold.args = list(nsub = 2L))
  fr = selectFeatures(lrn, task = binaryclass.task, resampling = rdesc,
    control = ctrl, show.info = FALSE, measures = getDefaultMeasure(binaryclass.task))
  df = as.data.frame(fr$opt.path)
  expect_true(is.numeric(df$threshold) && !any(is.na(df$threshold)))
})

test_that("show info works in selectFeatures", {
  rdesc = makeResampleDesc("Holdout", split = 0.05, stratify = TRUE)
  ctrl = makeFeatSelControlRandom(maxit = 1L)
  expect_message({
    z = selectFeatures("classif.rpart", task = iris.task, resampling = rdesc,
      control = ctrl, show.info = TRUE)
  }, "1: [01].*([0-9]+ bits)")
  expect_message({
    z = selectFeatures("classif.rpart", task = iris.task, resampling = rdesc,
      control = ctrl, show.info = TRUE)
  }, "mmce.test.mean=0.[0-9]+")
})

# we had a bug here when an empty model was created and isFailureModel could not be called on it, cf. #284
test_that("selectFeatures/sfs works with wrapper", {
  ctrl = makeFeatSelControlSequential(method = "sfs")
  lrn1 = makeLearner("classif.LiblineaRL2LogReg")
  lrn2 = makeWeightedClassesWrapper(lrn1, wcw.param = "wi")
  task = subsetTask(binaryclass.task, features = getTaskFeatureNames(binaryclass.task)[1:2])
  rdesc = makeResampleDesc("Holdout", split = 0.8, stratify = TRUE)
  sel = selectFeatures(lrn2, task, rdesc, control = ctrl)
  expect_class(sel, "FeatSelResult")
})

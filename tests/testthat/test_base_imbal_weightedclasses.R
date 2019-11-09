context("weightedclasses")

test_that("WeightedClassesWrapper, binary", {
  pos = getTaskDesc(binaryclass.task)$positive
  f = function(lrn, w) {
    lrn1 = makeLearner(lrn)
    lrn2 = makeWeightedClassesWrapper(lrn1, wcw.weight = w)
    m = train(lrn2, binaryclass.task)
    p = predict(m, binaryclass.task)
    return(calculateConfusionMatrix(p)$result)
  }

  learners = paste("classif", c("ksvm", "LiblineaRL1L2SVC", "LiblineaRL2L1SVC",
    "LiblineaRL2SVC", "LiblineaRL1LogReg", "LiblineaRL2LogReg", "LiblineaRMultiClassSVC",
    "randomForest", "svm"), sep = ".")
  x = lapply(learners, function(lrn) {
    cm1 = f(lrn, 0.001)
    cm2 = f(lrn, 1)
    cm3 = f(lrn, 1000)
    expect_true(all(cm1[, pos] <= cm2[, pos]))
    expect_true(all(cm2[, pos] <= cm3[, pos]))
  })

  # check what happens, if no weights are provided
  expect_error(f("classif.lda", 0.01))
})

test_that("WeightedClassesWrapper, multiclass", {
  levs = getTaskClassLevels(multiclass.task)
  f = function(lrn, w) {
    lrn1 = makeLearner(lrn)
    lrn2 = makeWeightedClassesWrapper(lrn1, wcw.weight = w)
    m = train(lrn2, multiclass.task)
    p = predict(m, multiclass.task)
    return(calculateConfusionMatrix(p)$result)
  }

  learners = paste("classif", c("ksvm", "LiblineaRL1L2SVC", "LiblineaRL2L1SVC",
    "LiblineaRL2SVC", "LiblineaRL1LogReg", "LiblineaRL2LogReg", "LiblineaRMultiClassSVC",
    "randomForest", "svm"), sep = ".")
  x = lapply(learners, function(lrn) {

    classes = getTaskFactorLevels(multiclass.task)[[multiclass.target]]
    cm1 = f(lrn, setNames(object = c(10000, 1, 1), classes))
    cm2 = f(lrn, setNames(object = c(1, 10000, 1), classes))
    cm3 = f(lrn, setNames(object = c(1, 1, 10000), classes))
    expect_true(all(cm1[, levs[1]] >= cm2[, levs[1]]))
    expect_true(all(cm1[, levs[1]] >= cm3[, levs[1]]))
    expect_true(all(cm2[, levs[2]] >= cm1[, levs[2]]))
    expect_true(all(cm2[, levs[2]] >= cm3[, levs[2]]))
    expect_true(all(cm3[, levs[3]] >= cm1[, levs[3]]))
    expect_true(all(cm3[, levs[3]] >= cm2[, levs[3]]))
  })

  # check what happens, if no weights are provided
  expect_error(f("classif.lda", setNames(object = c(1, 10000, 1), classes)))
})

context("getClassWeightParam")

test_that("getClassWeightParam", {
  f = function(lrn) {
    lrn1 = makeLearner(lrn)
    expect_is(getClassWeightParam(lrn), "LearnerParam")
    expect_is(getClassWeightParam(lrn1), "LearnerParam")
  }

  learners = paste("classif", c("ksvm", "LiblineaRL1L2SVC", "LiblineaRL2L1SVC",
    "LiblineaRL2SVC", "LiblineaRL1LogReg", "LiblineaRL2LogReg", "LiblineaRMultiClassSVC",
    "randomForest", "svm"), sep = ".")
  x = lapply(learners, f)

  # some special cases
  lrn = makeLearner("classif.ksvm")
  ps = lrn$par.set$pars[[lrn$class.weights.param]]

  # wrapped learner
  lrn.wrap = makeBaggingWrapper(lrn)
  expect_equal(ps, getClassWeightParam(lrn))

  # model multiplexer with at least 1 learner without class.weight prop
  mod.mult = makeModelMultiplexer(list(lrn, makeLearner("classif.rpart")))

  expect_error(getClassWeightParam(mod.mult), "please specify one of the base learners: classif.ksvm, classif.rpart")
  expect_error(getClassWeightParam(mod.mult, "classif.fu"), "classif.fu is not a base learner. Available base learners are: classif.ksvm, classif.rpart")
  expect_equal(getClassWeightParam(mod.mult, "classif.ksvm"), ps)
})

context("WeightedClassesWrapper")

test_that("WeightedClassesWrapper, binary",  {
  pos = binaryclass.task$task.desc$positive
  f = function(lrn, w) {
    lrn1 = makeLearner(lrn)
    lrn2 = makeWeightedClassesWrapper(lrn1, wcw.weight = w)
    m = train(lrn2, binaryclass.task)
    p = predict(m, binaryclass.task)
    cm = getConfMatrix(p)
  }

  learners = listLearners(binaryclass.task, "class.weights")
  x = lapply(learners, function(lrn) {
    cm1 = f(lrn, 0.001)
    cm2 = f(lrn, 1)
    cm3 = f(lrn, 1000)
    expect_true(all(cm1[, pos] <= cm2[, pos]))
    expect_true(all(cm2[, pos] <= cm3[, pos]))
  })
})

test_that("WeightedClassesWrapper, multiclass",  {
  pos = multiclass.task$task.desc$positive
  f = function(lrn, w) {
    lrn1 = makeLearner(lrn)
    param = lrn1$class.weights.param
    lrn2 = makeWeightedClassesWrapper(lrn1, wcw.weight = w)
    m = train(lrn2, multiclass.task)
    p = predict(m, multiclass.task)
    cm = getConfMatrix(p)
  }

  learners = listLearners(multiclass.task, "class.weights")
  x = lapply(learners, function(lrn) {
    classes = getTaskFactorLevels(multiclass.task)[[multiclass.target]]
    n = length(classes)
    cm1 = f(lrn, setNames(object = c(10000, 1, 1), classes))
    cm2 = f(lrn, setNames(object = c(1, 10000, 1), classes))
    cm3 = f(lrn, setNames(object = c(1, 1, 10000), classes))
    expect_true(all(cm1[, 1] >= cm2[, 1]))
    expect_true(all(cm1[, 1] >= cm3[, 1]))
    expect_true(all(cm2[, 2] >= cm1[, 2]))
    expect_true(all(cm2[, 2] >= cm3[, 2]))
    expect_true(all(cm3[, 3] >= cm1[, 3]))
    expect_true(all(cm3[, 3] >= cm2[, 3]))
  })
})

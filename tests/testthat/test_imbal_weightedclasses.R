context("WeightedClassesWrapper")

test_that("WeightedClassesWrapper, binary",  {
  pos = binaryclass.task$task.desc$positive
  f = function(w) {
    lrn1 = makeLearner("classif.rpart")
    lrn2 = makeWeightedClassesWrapper(lrn1, wcw.weight = w)
    m = train(lrn2, binaryclass.task)
    p = predict(m, binaryclass.task)
    cm = getConfMatrix(p)
  }
  cm1 = f(0.001)
  cm2 = f(1)
  cm3 = f(1000)
  expect_true(all(cm1[, pos] < cm2[, pos]))
  expect_true(all(cm2[, pos] < cm3[, pos]))
})

test_that("WeightedClassesWrapper, multiclass",  {
  levs = multiclass.task$task.desc$class.levels
  f = function(w) {
    lrn1 = makeLearner("classif.multinom")
    lrn2 = makeWeightedClassesWrapper(lrn1, wcw.weight = w)
    m = train(lrn2, multiclass.task)
    p = predict(m, multiclass.task)
    cm = getConfMatrix(p)
  }
  cm0 = f(c(1, 1, 1))
  cm1 = f(c(100000, 1, 1))
  cm2 = f(c(1, 100000, 1))
  cm3 = f(c(1, 1, 100000))
  expect_true(all(cm0[, levs[1]] <= cm1[, levs[1]]))
  expect_true(all(cm0[, levs[2]] <= cm2[, levs[2]]))
  expect_true(all(cm0[, levs[3]] <= cm3[, levs[3]]))
  expect_equal(sum(cm1[1:3, levs[1L]]), 150L)
  expect_equal(sum(cm2[1:3, levs[2L]]), 150L)
  expect_equal(sum(cm3[1:3, levs[3L]]), 150L)
})



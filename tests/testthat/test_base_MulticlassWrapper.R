context("MulticlassWrapper")

test_that("MulticlassWrapper", {
  # cmatrix function
  ownCmatrix = function(task) {
    cm.onevsrest = function(task) {
      n = length(getTaskClassLevels(task))
      cm = matrix(-1, n, n)
      diag(cm) = 1
      rownames(cm) = getTaskClassLevels(task)
      return(cm)
    }
    cm = cm.onevsrest(task)
    levs = getTaskClassLevels(task)
    if (!setequal(rownames(cm), levs)) {
      stop("Rownames of codematrix must be class levels!")
    }
    if (!all(cm == 1 | cm == -1 | cm == 0)) {
      stop("Codematrix must only contain: -1, 0, +1!")
    }
    cm
  }

  lrn1 = makeLearner("classif.rpart")
  lrn2 = makeLearner("classif.ranger")
  lrn3 = makeBaggingWrapper(learner = lrn1, bw.iters = 2)
  lrn1.w = makeMulticlassWrapper(lrn1)
  lrn2.w = makeMulticlassWrapper(lrn2, mcw.method = "onevsone")
  lrn3.w = makeMulticlassWrapper(lrn3, mcw.method = ownCmatrix)
  m1 = train(lrn1.w, multiclass.task)
  m2 = train(lrn2.w, multiclass.task)
  m3 = train(lrn3.w, multiclass.task)
  expect_false(isFailureModel(m1))
  expect_false(isFailureModel(m2))
  expect_false(isFailureModel(m3))
  rdesc = makeResampleDesc("CV", iters = 2)
  r1 = resample(lrn1.w, multiclass.task, rdesc)
  r2 = resample(lrn2.w, multiclass.task, rdesc)
  r3 = resample(lrn3.w, multiclass.task, rdesc)
  expect_true(r1$aggr[[1L]] < 0.2)
  expect_true(r2$aggr[[1L]] < 0.2)
  expect_true(r3$aggr[[1L]] < 0.2)
})

test_that("MulticlassWrapper works with multiple factor levels (#620)", {
  df = iris
  df$Sepal.Length = factor(df$Sepal.Length)
  classif.task = makeClassifTask(id = "test", data = df, target = "Species")
  base.lrn = makeLearner("classif.rpart")
  w = makeMulticlassWrapper(base.lrn, mcw.method = "onevsrest")
  rdesc = makeResampleDesc("CV", iters = 2L)
  res = benchmark(w, classif.task, rdesc)
  expect_true(all(res$results[[1]]$classif.rpart.multiclass$measures.test$mmce < 1L))
})

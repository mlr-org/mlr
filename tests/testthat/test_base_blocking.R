context("blocking")

test_that("blocking", {

  df = multiclass.df
  b = as.factor(rep(1:30, 5))
  ct = makeClassifTask(target = multiclass.target, data = multiclass.df, blocking = b)
  expect_true(getTaskDesc(ct)$has.blocking)

  res = makeResampleInstance(makeResampleDesc("CV", iters = 3, blocking.cv = TRUE), task = ct)
  for (j in 1:res$desc$iters) {
    train.j = res$train.inds[[j]]
    test.j = res$test.inds[[j]]
    tab = table(b[train.j])
    expect_true(setequal(c(0, 5), unique(as.numeric(tab))))
    tab = table(b[test.j])
    expect_true(setequal(c(0, 5), unique(as.numeric(tab))))
  }

  # test blocking in resample
  lrn = makeLearner("classif.lda")
  mycheck = function(rdesc, p, b) {
    for (j in 1:rdesc$iters) {
      test.j = p$data[p$data$iter == j, "id"]
      tab = table(b[test.j])
      expect_true(setequal(c(0, 5), unique(as.numeric(tab))))
    }
  }

  rdesc = makeResampleDesc("CV", iters = 3, blocking.cv = TRUE)
  p = resample(lrn, ct, rdesc)$pred
  mycheck(rdesc, p, b)

  rdesc = makeResampleDesc("RepCV", folds = 3, reps = 2, blocking.cv = TRUE)
  p = resample(lrn, ct, rdesc)$pred
  mycheck(rdesc, p, b)
})

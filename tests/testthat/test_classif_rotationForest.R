context("classif_rotationForest")

test_that("classif_rotationForest", {
  requirePackagesOrSkip("rotationForest", default.method = "load")

  parset.list = list(
    list(L = 5L, K = 2L),
    list(L = 10L, K = 2L)
  )

  old.predicts.list = list()
  old.probs.list = list()

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    train = binaryclass.train
    target = train[, binaryclass.target]
    target = as.factor(ifelse(target == binaryclass.task$task.desc$positive,
      1, 0))
    train[, binaryclass.target] = NULL
    pars = list(x = train, y = target)
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(rotationForest::rotationForest, pars)
    binaryclass.test[, binaryclass.target] = NULL
    p = predict(m, newdata = binaryclass.test)
    p = as.factor(ifelse(p > 0.5, binaryclass.task$task.desc$positive,
      binaryclass.task$task.desc$negative))
    p2 = predict(m, newdata = binaryclass.test)
    old.predicts.list[[i]] = p
    old.probs.list[[i]] = p2
  }

  testSimpleParsets("classif.rotationForest", binaryclass.df, binaryclass.target,
    binaryclass.train.inds, old.predicts.list, parset.list)
  testProbParsets("classif.rotationForest", binaryclass.df, binaryclass.target,
    binaryclass.train.inds, old.probs.list, parset.list)
})

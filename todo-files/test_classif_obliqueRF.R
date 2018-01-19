context("classif_obliqueRF")

test_that("classif_obliqueRF", {
  requirePackages("obliqueRF", default.method = "load")
  parset.list = list(
    list(),
    list(ntree = 5L,  mtry = 2L),
    list(training_method = "svm")
  )

  old.predicts.list = list()
  old.probs.list = list()

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    train = binaryclass.train
    target = train[, binaryclass.target]
    target = ifelse(target == binaryclass.task$task.desc$positive, 1, 0)
    train[, binaryclass.target] = NULL
    train = as.matrix(train)
    pars = list(x = train, y = target)
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(obliqueRF::obliqueRF, pars)
    binaryclass.test[, binaryclass.target] = NULL
    p = predict(m, newdata = binaryclass.test)
    p = as.factor(p)
    p = as.factor(ifelse(p == 1L, binaryclass.task$task.desc$positive, binaryclass.task$task.desc$negative))
    p2 = predict(m, newdata = binaryclass.test, type = "prob")
    p2 = p2[, colnames(p2) == "1"]
    old.predicts.list[[i]] = p
    old.probs.list[[i]] = p2
  }

  testSimpleParsets("classif.obliqueRF", binaryclass.df, binaryclass.target,
    binaryclass.train.inds, old.predicts.list, parset.list)
  testProbParsets("classif.obliqueRF", binaryclass.df, binaryclass.target,
    binaryclass.train.inds, old.probs.list, parset.list)
})
context("classif_pamr")

test_that("classif_pamr", {
  requirePackagesOrSkip("pamr", default.method = "load")

  parset.list = list(
    list(),
    list(threshold.predict = 2, n.threshold = 40L)
  )

  old.predicts.list = list()
  old.probs.list = list()

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    d = list(x = t(binaryclass.train[, -binaryclass.class.col]),
      y = binaryclass.train[, binaryclass.class.col])
    parset = c(parset, list(data = d))
    if ("threshold.predict" %in% names(parset)) {
      threshold.predict = parset$threshold.predict
      parset$threshold.predict = NULL
    } else {
      threshold.predict = 1
    }
    capture.output({
      m = do.call(pamr::pamr.train, parset)
    })
    newdata = t(binaryclass.test[, -binaryclass.class.col])
    old.predicts.list[[i]] = pamr::pamr.predict(m, newdata,
      threshold = threshold.predict)
    old.probs.list[[i]] = pamr::pamr.predict(m, newdata,
      type = "posterior", threshold = threshold.predict)[, 1L]
  }

  testSimpleParsets("classif.pamr", binaryclass.df, binaryclass.target,
    binaryclass.train.inds, old.predicts.list, parset.list)
  testProbParsets("classif.pamr", binaryclass.df, binaryclass.target,
    binaryclass.train.inds, old.probs.list, parset.list)
})

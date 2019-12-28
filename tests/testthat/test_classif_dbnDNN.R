context("classif_dbnDNN")

test_that("classif_dbnDNN", {
  requirePackagesOrSkip("deepnet", default.method = "load")

  parset.list1 = list(
    list(output = "softmax"),
    list(output = "softmax", hidden = 7)
  )

  parset.list2 = list(
    list(),
    list(hidden = 7)
  )

  old.predicts.list = list()

  for (i in seq_along(parset.list1)) {
    parset = parset.list1[[i]]

    capture.output({
      # neuralnet is not dealing with formula with `.` well
      x = data.matrix(binaryclass.train[, -ncol(binaryclass.train)])
      y = binaryclass.train[, ncol(binaryclass.train)]

      dict = sort(unique(y))
      onehot = matrix(0, length(y), length(dict))
      for (j in seq_along(dict)) {
        ind = which(y == dict[j])
        onehot[ind, j] = 1
      }
      pars = list(x = x, y = onehot)
      pars = c(pars, parset)
      set.seed(getOption("mlr.debug.seed"))
      m = do.call(deepnet::dbn.dnn.train, pars)
      p = deepnet::nn.predict(m, data.matrix(binaryclass.test[, -ncol(binaryclass.test)]))
      colnames(p) = binaryclass.class.levs
      old.predicts.list[[i]] = as.factor(colnames(p)[max.col(p)])
    })
  }

  testSimpleParsets("classif.dbnDNN", binaryclass.df, binaryclass.target,
    binaryclass.train.inds, old.predicts.list, parset.list2)
})

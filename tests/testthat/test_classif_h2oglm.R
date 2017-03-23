context("classif_h2oglm")

test_that("classif_h2oglm", {
  skip_on_travis()
  requirePackages("h2o", default.method = "load")
  h2o::h2o.init()

  parset.list = list(
    list(),
    list(alpha = 1),
    list(alpha = 1, lambda = 0.2)
  )
  old.probs.list = list()

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    parset = c(parset, list(x = colnames(binaryclass.train[, -binaryclass.class.col]),
      y = binaryclass.target, family = "binomial",
      training_frame = h2o::as.h2o(binaryclass.train)))
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(h2o::h2o.glm, parset)
    p  = predict(m, newdata = h2o::as.h2o(binaryclass.test))
    old.probs.list[[i]] = as.data.frame(p)[, 2]
  }

  testProbParsets("classif.h2o.glm", binaryclass.df, binaryclass.target, binaryclass.train.inds, old.probs.list, parset.list)
})

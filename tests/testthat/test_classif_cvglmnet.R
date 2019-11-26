context("classif_cvglmnet")

test_that("classif_cvglmnet", {
  requirePackagesOrSkip("glmnet", default.method = "load")

  parset.list = list(
    list(),
    list(mnlam = 4),
    list(nlambda = 20, nfolds = 5)
  )

  old.predicts.list = list()
  old.probs.list = list()

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    x = binaryclass.train
    y = x[, binaryclass.class.col]
    x[, binaryclass.class.col] = NULL
    pars = list(x = as.matrix(x), y = y, family = "binomial")
    pars = c(pars, parset)
    glmnet::glmnet.control(factory = TRUE)
    ctrl.args = names(formals(glmnet::glmnet.control))
    if (any(names(pars) %in% ctrl.args)) {
      on.exit(glmnet::glmnet.control(factory = TRUE))
      do.call(glmnet::glmnet.control, pars[names(pars) %in% ctrl.args])
      m = do.call(glmnet::cv.glmnet, pars[!names(pars) %in% ctrl.args])
    } else {
      m = do.call(glmnet::cv.glmnet, pars)
    }
    newx = binaryclass.test
    newx[, binaryclass.class.col] = NULL
    p = factor(predict(m, as.matrix(newx), type = "class")[, 1])
    p2 = predict(m, as.matrix(newx), type = "response")[, 1]
    old.predicts.list[[i]] = p
    old.probs.list[[i]] = 1 - p2
  }

  testSimpleParsets("classif.cvglmnet", binaryclass.df, binaryclass.target,
    binaryclass.train.inds, old.predicts.list, parset.list)
  testProbParsets("classif.cvglmnet", binaryclass.df, binaryclass.target,
    binaryclass.train.inds, old.probs.list, parset.list)
})

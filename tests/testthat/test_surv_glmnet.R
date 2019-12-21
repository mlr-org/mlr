context("surv_glmnet")

test_that("surv_glmnet", {
  requirePackagesOrSkip(c("survival", "!glmnet"), default.method = "load")

  parset.list = list(
    list(),
    list(alpha = 0.3, mxit = 80),
    list(alpha = 1, nlambda = 10),
    list(prec = 1e-3)
  )

  old.predicts.list = list()

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    y = as.matrix(surv.train[, surv.target])
    colnames(y) = c("time", "status")
    pars = c(list(y = Surv(time = surv.train[, surv.target[1]],
      event = surv.train[, surv.target[2]]),
    x = as.matrix(surv.train[, -c(1, 2, 7)]), family = "cox"), parset)
    glmnet::glmnet.control(factory = TRUE)
    ctrl.args = names(formals(glmnet::glmnet.control))
    if (any(names(pars) %in% ctrl.args)) {
      on.exit(glmnet::glmnet.control(factory = TRUE))
      do.call(glmnet::glmnet.control, pars[names(pars) %in% ctrl.args])
      m = do.call(glmnet::glmnet, pars[!names(pars) %in% ctrl.args])
    } else {
      m = do.call(glmnet::glmnet, pars)
    }
    p = predict(m, newx = as.matrix(surv.test[, -c(1, 2, 7)]), type = "link",
      s = 0.01)
    old.predicts.list[[i]] = as.numeric(p)
  }

  testSimpleParsets("surv.glmnet", surv.df[, -7], surv.target, surv.train.inds,
    old.predicts.list, parset.list)

  # check that we restored the factory default
  expect_true(glmnet::glmnet.control()$prec < 1e-4) # should be ==1e-5
})

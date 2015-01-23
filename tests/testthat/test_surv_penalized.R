context("surv_penalized")

test_that("surv_penalized", {
  requirePackages("survival", default.method = "load")
  requirePackages("penalized", default.method = "load")
  parset.list = list(
    list()
  )

  old.predicts.list = list()

  for (i in 1:length(parset.list)) {
    pars = c(list(response = surv.formula, data = surv.train[, -7], model = "cox", trace = FALSE), parset.list[[i]])
    set.seed(getOption("mlr.debug.seed"))
    m = do.call(penalized::penalized, pars)

    # contr = contr.none(3)
    # colnames(contr) = levels(surv.test[, "Species"])
    # contrasts(surv.test[, "Species"], how.many = 3) = contr
    p = penalized::survival(penalized::predict(m, penalized = model.matrix(surv.formula, surv.test[, -7])[, -1]), Inf)
    old.predicts.list[[i]] = p
  }

  # FIXME: does not work yet:
  testSimpleParsets("surv.penalized", surv.df[, -7], surv.target, surv.train.inds, old.predicts.list, parset.list)
})

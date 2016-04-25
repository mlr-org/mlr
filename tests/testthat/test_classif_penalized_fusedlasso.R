context("classif_penalized_fusedlasso")

test_that("classif_penalized_fusedlasso", {
  requirePackages("!penalized", default.method = "load")
  parset.list = list(
    list(maxiter = 10L),
    list(lambda1 = 2, maxiter = 10L),
    list(lambda1 = 2, lambda2 = 1, maxiter = 5L),
    list(lambda2 = 2, maxiter = 10L)
  )
  old.probs.list = list()

  for (i in 1:length(parset.list)) {
    parset = parset.list[[i]]
    if (is.null(parset$lambda1))
      parset$lambda1 = 1
    if (is.null(parset$lambda2))
      parset$lambda2 = 1
    pars = list(binaryclass.formula, data = binaryclass.train, trace = FALSE,
      fusedl = TRUE)
    pars = c(pars, parset)
    set.seed(getOption("mlr.debug.seed"))
    capture.output(
      m <- do.call(penalized::penalized, pars)
    )
    old.probs.list[[i]] = 1 - penalized::predict(m, data = binaryclass.test)
  }
  testProbParsets("classif.penalized.fusedlasso", binaryclass.df, binaryclass.target,
    binaryclass.train.inds, old.probs.list, parset.list)

  parset.list = list(
    list(lambda1 = 2, lambda2 = 1, maxiter = 2L),
    list(lambda1 = 1, lambda2 = 2, maxiter = 4L)
  )

  tt = function(formula, data, subset = 1:nrow(data), ...) {
    penalized::penalized(formula, data = data[subset, ],
      fusedl = TRUE, trace = FALSE, ...)
  }

  tp = function(model, newdata, ...) {
    pred = penalized::predict(model, data = newdata,...)
    ifelse(pred > 0.5, binaryclass.class.levs[2L], binaryclass.class.levs[1L])
  }
  
  testCVParsets("classif.penalized.fusedlasso", binaryclass.df, binaryclass.target,
    tune.train = tt, tune.predict = tp, parset.list = parset.list)
})

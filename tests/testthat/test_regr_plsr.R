context("regr_plsr")

test_that("regr_plsr", {
  requirePackages("pls")
	parset.list = list(
      list(),
			list(ncomp = 1),
			list(ncomp = 3, method = "simpls")
	)
	
	old.predicts.list = list()
	
	for (i in 1:length(parset.list)) {
		parset = parset.list[[i]]
		pars = list(regr.formula, data = regr.train)
		pars = c(pars, parset)
		m = do.call(plsr, pars)
		old.predicts.list[[i]] = predict(m, newdata = regr.test, comps = 1:m$ncomp)[,1]
	}
	
	testSimpleParsets("regr.plsr", regr.df, regr.target, regr.train.inds, old.predicts.list, parset.list)
})

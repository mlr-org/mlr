context("classif_blackboost")

test_that("classif_blackboost", {
  library(mboost)
  library(party)
  
	parset.list1 = list(
			list(family=Binomial(), tree_controls=ctree_control(maxdepth=2)),
			list(family=Binomial(), tree_controls=ctree_control(maxdepth=4), control=boost_control(nu=0.03))
	)
	
	parset.list2 = list(
			list(family=Binomial(), maxdepth=2),
			list(family=Binomial(), maxdepth=4, nu=0.03)
	)
	
	old.predicts.list = list()
	old.probs.list = list()
	
	for (i in 1:length(parset.list1)) {
		parset = parset.list1[[i]]
		pars = list(binaryclass.formula, data=binaryclass.train)
		pars = c(pars, parset)
		set.seed(getOption("mlr.debug.seed"))
		m = do.call(blackboost, pars)
		set.seed(getOption("mlr.debug.seed"))
		old.predicts.list[[i]] = predict(m, newdata=binaryclass.test, type="class")
		set.seed(getOption("mlr.debug.seed"))
		old.probs.list[[i]] = predict(m, newdata=binaryclass.test, type="response")[,1]
	}
	
	testSimpleParsets("classif.blackboost", binaryclass.df, binaryclass.target, binaryclass.train.inds, old.predicts.list, parset.list2)
	testProbParsets("classif.blackboost", binaryclass.df, binaryclass.target, binaryclass.train.inds, old.probs.list, parset.list2)	
})




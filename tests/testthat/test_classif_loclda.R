#context("classif_loclda")
#
#test_that("classif_loclda", {
#	require("klaR")
#	m = klaR::loclda(formula=multiclass.formula, data=multiclass.train)
#	p = predict(m, newdata=multiclass.test)
#	
#	testSimple("classif.loclda", multiclass.df, multiclass.target, multiclass.train.inds, p$class)
#	testProb  ("classif.loclda", multiclass.df, multiclass.target, multiclass.train.inds, p$posterior)
#	
#	tt = klar::loclda
#	tp = function(model, newdata) predict(model, newdata)$class
#	
#	testCV("classif.loclda", multiclass.df, multiclass.target, tune.train=tt, tune.predict=tp )
#	
#})

#test.lda = function() {
#	
#	set.seed(getOption("mlr.debug.seed"))
#	m = lda(formula=multiclass.formula, data=multiclass.train)
#	set.seed(getOption("mlr.debug.seed"))
#	p = predict(m, newdata=multiclass.test)
#	
#	testSimple("classif.hda", multiclass.df, multiclass.target, multiclass.train.inds, p$class)
#		
#	tt = "hda"
#	tp = function(model, newdata) predict(model, newdata)$class
#	
#	testCV("classif.hda", multiclass.df, multiclass.target, tune.train=tt, tune.predict=tp )
#	
#	
#}




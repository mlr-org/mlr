context("cluster_XMeans")

test_that("cluster_XMeans", {
    library(RWeka)
	parset.list = list(
			list(),
			list(I=1)
	)
	
	old.predicts.list = list()
	old.probs.list = list()
	
	for (i in 1:length(parset.list)) {
		parset = parset.list[[i]]
		ctrl = do.call(Weka_control, parset)
		set.seed(getOption("mlr.debug.seed"))
		m = XMeans(noclass.train, control=ctrl)
		p = predict(m, newdata=noclass.test)
		old.predicts.list[[i]] = p
	}
	
	testSimpleParsets("cluster.XMeans", noclass.df, noclass.train.inds, old.predicts.list, parset.list)
	testProbParsets  ("cluster.XMeans", noclass.df, noclass.train.inds, old.probs.list, parset.list)
	
	tt = function (formula, data, subset, ...) {
		XMeans(data[subset,], control=Weka_control(...))
	}
	
	tp = function(model, newdata) predict(model, newdata)
	
	testCVParsets("cluster.XMeans", noclass.df, tune.train=tt, tune.predict=tp, parset.list=parset.list)
})

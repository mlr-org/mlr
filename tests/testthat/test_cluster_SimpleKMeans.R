context("cluster_SimpleKMeans")

test_that("cluster_SimpleKMeans", {
  library(RWeka)
	parset.list = list(
			list(),
			list(N=5)
	)
	
	old.predicts.list = list()
	
	for (i in 1:length(parset.list)) {
		parset = parset.list[[i]]
		ctrl = do.call(Weka_control, parset)
		set.seed(getOption("mlr.debug.seed"))
		m = SimpleKMeans(noclass.train, control=ctrl)
		p = predict(m, noclass.test) + 1
		old.predicts.list[[i]] = p
	}
	
	testSimpleParsets("cluster.SimpleKMeans", noclass.df, character(0L), noclass.train.inds, old.predicts.list, parset.list)
})

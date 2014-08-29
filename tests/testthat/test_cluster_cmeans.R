context("cluster_cmeans")

test_that("cluster_cmeans", {
    library(clue)

    centers = matrix(c(1,2,3,4,2,3,4,5), ncol=4)
	parset.list = list(
        list(centers=centers)
	)
	
	old.predicts.list = list()
	
	for (i in 1:length(parset.list)) {
		parset = parset.list[[i]]
		set.seed(getOption("mlr.debug.seed"))
		m = cmeans(noclass.train, centers = centers)
		p = as.integer(cl_predict(m, newdata = noclass.test, type = "class_ids"))
		old.predicts.list[[i]] = p
	}
	
	testSimpleParsets("cluster.cmeans", noclass.df, character(0L), noclass.train.inds, old.predicts.list, parset.list)

    # test fuzzy clustering memberships
    set.seed(getOption("mlr.debug.seed"))
    m = cmeans(noclass.train, centers = centers)
    p = as.matrix(cl_predict(m, newdata = noclass.test, type = "memberships"))

    lrn = makeLearner("cluster.cmeans", predict.type = "prob")
    m = train(lrn, task = makeClusterTask(data = noclass.train))
    pp = as.matrix(predict(m, newdata = noclass.test)$data)

    print(p)
    print(pp)
    #expect_equal(p, pp, tol=1e-3)
})

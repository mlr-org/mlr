context("cluster_kmeans")

test_that("cluster_kmeans", {
  requirePackages("stats", default.method = "load")

  centers = matrix(c(1, 2, 3, 4, 2, 3, 4, 5), ncol = 4)
  parset.list = list(
    list(centers = centers),
    list(centers = 2L)
  )

  old.predicts.list = list()

  for (i in 1:length(parset.list)) {
    parset = parset.list[[i]]
    set.seed(getOption("mlr.debug.seed"))
    m = stats::kmeans(noclass.train, centers = centers)
    p = as.integer(clue::cl_predict(m, newdata = noclass.test, type = "class_ids"))
    old.predicts.list[[i]] = p
  }

  testSimpleParsets("cluster.kmeans", noclass.df, character(0L), noclass.train.inds,
    old.predicts.list, parset.list)
})

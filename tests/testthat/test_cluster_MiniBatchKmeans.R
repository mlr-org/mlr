context("cluster_MiniBatchKmeans")

test_that("cluster_MiniBatchKmeans", {
  requirePackagesOrSkip("ClusterR", default.method = "load")

  parset.list = list(
    list()
  )

  old.predicts.list = list()

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    set.seed(getOption("mlr.debug.seed"))
    m = ClusterR::MiniBatchKmeans(noclass.train, clusters = 2L, batch_size = 10L)
    p = as.integer(ClusterR::predict_MBatchKMeans(data = noclass.test, CENTROIDS = m$centroids, fuzzy = FALSE))
    p[p == 0] = NA
    old.predicts.list[[i]] = p
  }

  testSimpleParsets("cluster.MiniBatchKmeans", noclass.df,
    character(0L), noclass.train.inds, old.predicts.list,
    parset.list)
})

context("cluster_MiniBatchKmeans")

test_that("cluster_MiniBatchKmeans", {
  requirePackagesOrSkip("ClusterR", default.method = "load")

  parset.list = list(
    list(),
    list(clusters = 3L, batch_size = 5L)
  )

  old.predicts.list = list()

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]

    if ("clusters" %in% names(parset)) {
      clst = parset[["clusters"]]
    } else {
      clst = 2L
    }

    if ("batch_size" %in% names(parset)) {
      btch.size = parset[["batch_size"]]
    } else {
      btch.size = 10L
    }

    m = ClusterR::MiniBatchKmeans(noclass.train,
      clusters = clst, batch_size = btch.size)
    p = as.integer(ClusterR::predict_MBatchKMeans(data = noclass.test,
      CENTROIDS = m$centroids, fuzzy = FALSE))
    old.predicts.list[[i]] = p
  }

  testSimpleParsets("cluster.MiniBatchKmeans", noclass.df,
    character(0L), noclass.train.inds, old.predicts.list,
    parset.list)
})

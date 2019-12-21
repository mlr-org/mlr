context("cluster_kkmeans")

test_that("cluster_kkmeans", {
  requirePackagesOrSkip("kernlab", default.method = "load")

  centers = matrix(c(1, 2, 3, 4, 2, 3, 4, 5), ncol = 4)
  parset.list = list(
    list(),
    list(centers = centers)
  )
  old.predicts.list = list()

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    set.seed(getOption("mlr.debug.seed"))
    m = kernlab::kkmeans(as.matrix(noclass.train), centers = centers)
    K = kernlab::kernelf(m)
    c = kernlab::centers(m)
    d.xc = kernlab::kernelMatrix(K, as.matrix(noclass.test), c)
    d.xx = matrix(rep(diag(kernlab::kernelMatrix(K, as.matrix(noclass.test))),
      each = ncol(d.xc)), ncol = ncol(d.xc), byrow = TRUE)
    d.cc = matrix(rep(diag(kernlab::kernelMatrix(K, as.matrix(c))),
      each = nrow(d.xc)), nrow = nrow(d.xc))
    d2 = d.xx + d.cc - 2 * d.xc # this is the squared kernel distance to the centers
    p = apply(d2, 1, function(x) BBmisc::getMinIndex(x, ties.method = "random"))
    old.predicts.list[[i]] = p
  }

  testSimpleParsets("cluster.kkmeans", noclass.df, character(0L),
    noclass.train.inds, old.predicts.list, parset.list)
})

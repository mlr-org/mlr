context("cluster_kkmeans")

test_that("cluster_kkmeans", {
  requirePackagesOrSkip("kernlab", default.method = "load")

  centers = matrix(c(1, 2, 3, 4, 2, 3, 4, 5), ncol = 4)
  parset.list = list(
    list(),
    list(centers = centers)
  )
  old.predicts.list = list()

  for (i in 1:length(parset.list)) {
    parset = parset.list[[i]]
    set.seed(getOption("mlr.debug.seed"))
    m = kernlab::kkmeans(as.matrix(noclass.train), centers = centers)
    K = kernlab::kernelf(m)
    c = kernlab::centers(m)
    Dxc = kernlab::kernelMatrix(K, as.matrix(noclass.test), c)
    Dxx = matrix(rep(diag(kernlab::kernelMatrix(K, as.matrix(noclass.test))), each = ncol(Dxc)), ncol = ncol(Dxc), byrow = TRUE)
    Dcc = matrix(rep(diag(kernlab::kernelMatrix(K, as.matrix(c))), each = nrow(Dxc)), nrow = nrow(Dxc))
    D2 = Dxx + Dcc - 2*Dxc #this is the squared kernel distance to the centers
    p = apply(D2, 1, function(x) BBmisc::getMinIndex(x, ties.method = "random"))
    old.predicts.list[[i]] = p
  }

  testSimpleParsets("cluster.kkmeans", noclass.df, character(0L), noclass.train.inds,
    old.predicts.list, parset.list)
})

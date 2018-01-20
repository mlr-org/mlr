instantiateResampleInstance = function(desc, size, task) {
  UseMethod("instantiateResampleInstance")
}

instantiateResampleInstance.HoldoutDesc = function(desc, size, task = NULL) {
  inds = sample(size, size * desc$split)
  makeResampleInstanceInternal(desc, size, train.inds = list(inds))
}

instantiateResampleInstance.CVDesc = function(desc, size, task = NULL) {
  if (desc$iters > size)
    stopf("Cannot use more folds (%i) than size (%i)!", desc$iters, size)
  test.inds = chunk(seq_len(size), shuffle = TRUE, n.chunks = desc$iters)
  makeResampleInstanceInternal(desc, size, test.inds = test.inds)
}

instantiateResampleInstance.SpCVDesc = function(desc, size, task = NULL) {

  if (is.null(task) | is.null(task$coordinates)) {
    stopf("Please provide a task with suitable coordinates for SpCV.")
  }
  else {
    # subset the coordinates with respect to the observations in the data
    # in a nested cv call we only have a subset of the whole data here (the
    # indices from the training fold of the outer loop)
    task$coordinates = task$coordinates[as.integer(rownames(task$env$data)), ]

    # perform kmeans clustering
    inds = kmeans(task$coordinates, centers = desc$iters)
    inds = factor(inds$cluster)

    # uses resulting factor levels from kmeans clustering to set up a list of
    # length x (x = folds) with row indices of the data referring to which fold
    # each observations is assigned to
    test.inds = lapply(levels(inds), function(x, spl)
      which(spl == x), spl = inds)

    makeResampleInstanceInternal(desc, size, test.inds = test.inds)
  }

}

instantiateResampleInstance.LOODesc = function(desc, size, task = NULL) {
  desc$iters = size
  makeResampleInstanceInternal(desc, size, test.inds = as.list(seq_len(size)))
}

instantiateResampleInstance.SubsampleDesc = function(desc, size, task = NULL) {
  inds = lapply(seq_len(desc$iters), function(x) sample(size, size * desc$split))
  makeResampleInstanceInternal(desc, size, train.inds = inds)
}

instantiateResampleInstance.BootstrapDesc = function(desc, size, task = NULL) {
  inds = lapply(seq_len(desc$iters), function(x) sample(size, size, replace = TRUE))
  makeResampleInstanceInternal(desc, size, train.inds = inds)
}

instantiateResampleInstance.RepCVDesc = function(desc, size, task = NULL) {
  folds = desc$iters / desc$reps
  d = makeResampleDesc("CV", iters = folds)
  i = replicate(desc$reps, makeResampleInstance(d, size = size), simplify = FALSE)
  train.inds = Reduce(c, lapply(i, function(j) j$train.inds))
  test.inds = Reduce(c, lapply(i, function(j) j$test.inds))
  g = as.factor(rep(seq_len(desc$reps), each = folds))
  makeResampleInstanceInternal(desc, size, train.inds = train.inds, test.inds = test.inds, group = g)
}

instantiateResampleInstance.SpRepCVDesc = function(desc, size, task = NULL) {
  folds = desc$iters / desc$reps
  d = makeResampleDesc("SpCV", iters = folds)
  i = replicate(desc$reps, makeResampleInstance(d, task = task), simplify = FALSE)
  train.inds = Reduce(c, lapply(i, function(j) j$train.inds))
  test.inds = Reduce(c, lapply(i, function(j) j$test.inds))
  g = as.factor(rep(seq_len(desc$reps), each = folds))
  makeResampleInstanceInternal(desc, size, train.inds = train.inds, test.inds = test.inds, group = g)
}

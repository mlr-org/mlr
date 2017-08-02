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


# Resample for oneclass-classification. Train data only contains one class, which is the normal class

instantiateResampleInstance.OCHoldoutDesc = function(desc, size, task) {
  label = getTaskTargets(task)
  normal.inds = which(label == task$task.desc$negative) #index of only normal class
  size.normal = length(normal.inds)
  inds = sample(normal.inds, size.normal * desc$split)
  makeResampleInstanceInternal(desc, size, train.inds = list(inds))
}

instantiateResampleInstance.OCCVDesc = function(desc, size, task) {
  if (desc$iters > size)
    stopf("Cannot use more folds (%i) than size (%i)!", desc$iters, size)

  # testset will have normal and anomaly observations
  test.inds = chunk(seq_len(size), shuffle = TRUE, n.chunks = desc$iters)

  label = getTaskTargets(task)
  normal.inds = which(label == task$task.desc$negative) #index of only normal class

  # only allow normal obs in training and shuffle data set of normal observation
  # basically drop anomaly in training
  train.inds = sample(normal.inds)
  train.inds = lapply(test.inds, function(x) setdiff(train.inds, x))

  makeResampleInstanceInternal(desc, size, train.inds = train.inds, test.inds = test.inds)
}


instantiateResampleInstance.OCSubsampleDesc = function(desc, size, task) {
  label = getTaskTargets(task)
  normal.inds = which(label == task$task.desc$negative) #only normal class
  size.normal = length(normal.inds)
  # sample without replacement only from the normal data to create train set
  # all anomaly are going to be in the test set
  inds = lapply(seq_len(desc$iters), function(x) sample(normal.inds, size.normal * desc$split))
  makeResampleInstanceInternal(desc, size, train.inds = inds)
}

instantiateResampleInstance.OCBootstrapDesc = function(desc, size, task) {

  label = getTaskTargets(task)
  normal.inds = which(label == task$task.desc$negative) #only normal class
  size.normal = length(normal.inds)
  # sample with replacement only from the normal data to create train set
  # all anomaly are going to be in the test set
  inds = lapply(seq_len(desc$iters), function(x) sample(normal.inds, size.normal, replace = TRUE))
  makeResampleInstanceInternal(desc, size, train.inds = inds)
}

instantiateResampleInstance.OCRepCVDesc = function(desc, size = NULL, task) {
  folds = desc$iters / desc$reps
  d = makeResampleDesc("OCCV", iters = folds)
  i = replicate(desc$reps, makeResampleInstance(d, task), simplify = FALSE)
  train.inds = Reduce(c, lapply(i, function(j) j$train.inds))
  test.inds = Reduce(c, lapply(i, function(j) j$test.inds))
  g = as.factor(rep(seq_len(desc$reps), each = folds))
  makeResampleInstanceInternal(desc, size, train.inds = train.inds, test.inds = test.inds, group = g)
}

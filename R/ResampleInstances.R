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

  if (is.null(task)) {
    stopf("Please provide a task.")
  }
  if (is.null(task$coordinates)) {
    stopf("Please provide suitable coordinates for SpCV. See ?Task for help.")
  }
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

instantiateResampleInstance.FixedWindowCVDesc = function(desc, size, task = NULL, coords) {
  makeResamplingWindow(desc, size, task, coords, "FixedWindowCV")
}

instantiateResampleInstance.GrowingWindowCVDesc = function(desc, size, task = NULL, coords) {
  makeResamplingWindow(desc, size, task, coords, "GrowingWindowCV")
}

instantiateResampleInstance.BlockingDesc = function(desc, size, task = NULL) {

   test = 0
  if (!is.na(desc$iters) && desc$iters != length(levels(droplevels(task$blocking)))) {
    warningf("iters (%i) is not equal to blocking levels (%i)! Setting iters to length of blocking levels.", desc$iters, length(levels(task$blocking)))
    desc$iters = length(levels(droplevels(task$blocking)))
  } else {
    desc$iters = length(levels(droplevels(task$blocking)))
  }
  desc2 = desc
  attr(desc2, "class") = c("CVDesc", "ResampleDesc")
  levs = levels(task$blocking)
  size2 = length(levels(task$blocking))
  desc2$iters = length(levels(task$blocking))
  desc2$id = "cross-validation"

  # create fake ResampleInstance to initialize "blocking"
  inst = instantiateResampleInstance(desc2, size2, task)
  # now exchange block indices with indices of elements of this block and shuffle

  test.inds = lapply(inst$test.inds, function(i) sample(which(task$blocking %in% levs[i])))

  # Nested resampling: We need to create a list with length(levels) first.
  # Then one fold will be length(0) because we are missing one factor level because we are in the inner level
  # We check for this and remove this fold
  # There is no other way to do this. If we initially set "desc$iters" to length(levels) - 1, test.inds will not be created correctly
  length.test.inds = unlist(lapply(test.inds, function(x) length(x)))
  if (0 %in% length.test.inds) {
    index = match(0, length.test.inds)
    test.inds[[index]] = NULL
  }
  makeResampleInstanceInternal(desc, size, test.inds = test.inds)
}

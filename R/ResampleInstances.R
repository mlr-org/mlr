instantiateResampleInstance = function(desc, size, task) {
  UseMethod("instantiateResampleInstance")
}

instantiateResampleInstance.HoldoutDesc = function(desc, size, task = NULL) {
  inds = sample(size, size * desc$split)
  makeResampleInstanceInternal(desc, size, train.inds = list(inds))
}

instantiateResampleInstance.CVDesc = function(desc, size, task = NULL) {
  # Random sampling CV
  if (!desc$fixed) {
    if (desc$iters > size) {
      stopf("Cannot use more folds (%i) than size (%i)!", desc$iters, size)
    }
    test.inds = chunk(seq_len(size), shuffle = TRUE, n.chunks = desc$iters)
    makeResampleInstanceInternal(desc, size, test.inds = test.inds)
  } else {

    # CV with only predefined indices ("fixed")

    if (is.null(task$blocking)) {
      stopf("To use blocking in resampling, you need to pass a factor variable when creating the task!")
    }

    # In the inner call, the implementation is able to adapt by automatically reducing one level (see line if (0 %in% length.test.inds)).
    # So having always `length(iters) = length(levels(task$blocking)` is the most safe environment for the function to work.
    if (desc$iters != length(levels(task$blocking))) {
      desc$iters = length(levels(task$blocking))
      warningf("Adjusting levels to match number of blocking levels.")
    }
    levs = levels(task$blocking)
    n.levels = length(levs)

    # Why do we need the helper desc? If we would call 'instantiateResampleInstance()' here,
    # we would call the function within itself and will receive an 'error-c-stack-usage-is-too-close-to-the-limit' error
    # So we simply change the class name to mimic a new function..
    attr(desc, "class")[1] = "CVHelperDesc"
    # create fake ResampleInstance
    inst = instantiateResampleInstance(desc, n.levels, task)
    attr(desc, "class")[1] = "CVDesc"

    # now exchange block indices with indices of elements of this block and shuffle
    test.inds = lapply(inst$test.inds, function(i) which(task$blocking %in% levs[i]))

    # Nested resampling: We need to create a list with length(levels) first.
    # Then one fold will be length(0) because we are missing one factor level because we are in the inner level
    # We check for this and remove this fold
    # There is no other way to do this. If we initially set "desc$iters" to length(levels) - 1, test.inds will not be created correctly
    length.test.inds = unlist(lapply(test.inds, function(x) length(x)))
    if (0 %in% length.test.inds) {
      index = match(0, length.test.inds)
      test.inds[[index]] = NULL
      size = length(task$env$data[, 1])
      desc$iters = length(test.inds)
    }
    makeResampleInstanceInternal(desc, size, test.inds = test.inds)
  }
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
  test.inds = lapply(levels(inds), function(x, spl) {
    which(spl == x)
  }, spl = inds)

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
  d = makeResampleDesc("CV", iters = folds, blocking.cv = desc$blocking.cv, fixed = desc$fixed)
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

instantiateResampleInstance.CVHelperDesc = function(desc, size, task = NULL) {
  if (desc$iters > size) {
    stopf("Cannot use more folds (%i) than size (%i)!", desc$iters, size)
  }
  test.inds = chunk(seq_len(size), shuffle = TRUE, n.chunks = desc$iters)
  makeResampleInstanceInternal(desc, size, test.inds = test.inds)
}

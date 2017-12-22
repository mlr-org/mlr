instantiateResampleInstance = function(desc, size, task, coords) {
  UseMethod("instantiateResampleInstance")
}

instantiateResampleInstance.HoldoutDesc = function(desc, size, task = NULL, coords) {
  inds = sample(size, size * desc$split)
  makeResampleInstanceInternal(desc, size, train.inds = list(inds))
}

instantiateResampleInstance.CVDesc = function(desc, size, task = NULL, coords) {
  if (desc$iters > size)
    stopf("Cannot use more folds (%i) than size (%i)!", desc$iters, size)
  test.inds = chunk(seq_len(size), shuffle = TRUE, n.chunks = desc$iters)
  makeResampleInstanceInternal(desc, size, test.inds = test.inds)
}

instantiateResampleInstance.SpCVDesc = function(desc, size, task = NULL, coords = NULL) {
  if (is.null(coords)) {
    coords = data.frame(task$env$data$x, task$env$data$y)
  } else {
    coords = coords
  }
  # perform kmeans clustering
  inds = kmeans(coords, centers = desc$iters)
  inds = factor(inds$cluster)

  # uses resulting factor levels from kmeans clustering to set up a list of
  # length x (x = folds) with row indices of the data referring to which fold
  # each observations is assigned to
  test.inds = lapply(levels(inds), function(x, spl)
    which(spl == x), spl = inds)

  makeResampleInstanceInternal(desc, size, test.inds = test.inds)
}

instantiateResampleInstance.LOODesc = function(desc, size, task = NULL, coords) {
  desc$iters = size
  makeResampleInstanceInternal(desc, size, test.inds = as.list(seq_len(size)))
}

instantiateResampleInstance.SubsampleDesc = function(desc, size, task = NULL, coords) {
  inds = lapply(seq_len(desc$iters), function(x) sample(size, size * desc$split))
  makeResampleInstanceInternal(desc, size, train.inds = inds)
}

instantiateResampleInstance.BootstrapDesc = function(desc, size, task = NULL, coords) {
  inds = lapply(seq_len(desc$iters), function(x) sample(size, size, replace = TRUE))
  makeResampleInstanceInternal(desc, size, train.inds = inds)
}

instantiateResampleInstance.RepCVDesc = function(desc, size, task = NULL, coords) {
  folds = desc$iters / desc$reps
  d = makeResampleDesc("CV", iters = folds)
  i = replicate(desc$reps, makeResampleInstance(d, size = size), simplify = FALSE)
  train.inds = Reduce(c, lapply(i, function(j) j$train.inds))
  test.inds = Reduce(c, lapply(i, function(j) j$test.inds))
  g = as.factor(rep(seq_len(desc$reps), each = folds))
  makeResampleInstanceInternal(desc, size, train.inds = train.inds, test.inds = test.inds, group = g)
}


instantiateResampleInstance.SpRepCVDesc = function(desc, size, task = NULL, coords = NULL) {
  folds = desc$iters / desc$reps
  d = makeResampleDesc("SpCV", iters = folds)
  i = replicate(desc$reps, makeResampleInstance(d, task = task, coords = coords), simplify = FALSE)
  train.inds = Reduce(c, lapply(i, function(j) j$train.inds))
  test.inds = Reduce(c, lapply(i, function(j) j$test.inds))
  g = as.factor(rep(seq_len(desc$reps), each = folds))
  makeResampleInstanceInternal(desc, size, train.inds = train.inds, test.inds = test.inds, group = g)

}

instantiateResampleInstance.FixedCVDesc = function(desc, size, task = NULL, coords) {
  initial.window.abs = floor(desc$initial.window * size)
  assertInt(initial.window.abs, lower = 1)
  if (size - initial.window.abs < desc$horizon)
    stop(catf("The initial window is %i observations while the data is %i observations. \n
      There is not enough data left (%i observations) to create a test set for a %i size horizon",
      initial.window.abs, size, initial.window.abs - size, desc$horizon))
  skip = desc$skip
  stops  = (seq(size))[initial.window.abs:(size - desc$horizon)]
  starts = stops - initial.window.abs + 1
  train.inds = mapply(seq, starts, stops, SIMPLIFY = FALSE)
  test.inds  = mapply(seq, stops + 1, stops + desc$horizon, SIMPLIFY = FALSE)

  thin = function(x, skip = 0) {
    n = length(x)
    x[seq(1, n, by = skip)]
  }

  if (skip > 0) {
    train.inds = thin(train.inds, skip = skip)
    test.inds = thin(test.inds, skip = skip)
  }

  if (length(test.inds) == 0)
    stop("Skip is too large and has removed all resampling instances. Please lower the value of skip.")

  # If the last value if not included, shift everything over by one
  if (test.inds[[length(test.inds)]][desc$horizon] != size) {

    train.inds = lapply(train.inds, function(x) x + 1)
    test.inds = lapply(test.inds, function(x) x + 1)
  }
  desc$iters = length(test.inds)
  makeResampleInstanceInternal(desc, size, train.inds = train.inds, test.inds = test.inds)
}

instantiateResampleInstance.GrowingCVDesc = function(desc, size, task = NULL, coords) {
  initial.window.abs = floor(desc$initial.window * size)
  assertInt(initial.window.abs, lower = 1)
  if (size - initial.window.abs < desc$horizon)
    stop(catf("The initial window is %i observations while the data is %i observations. \n
      There is not enough data left (%i observations) to create a test set for a %i size horizon",
      initial.window.abs, size, initial.window.abs - size, desc$horizon))
  skip = desc$skip
  stops  = (seq(from = 1, to = size))[initial.window.abs:(size - desc$horizon)]
  starts = rep(1, length(stops))
  train.inds = mapply(seq, starts, stops, SIMPLIFY = FALSE)
  test.inds  = mapply(seq, stops + 1, stops + desc$horizon, SIMPLIFY = FALSE)


  thin = function(x, skip = 0) {
    n = length(x)
    x[seq(1, n, by = skip)]
  }

  if (skip > 0) {
    train.inds = thin(train.inds, skip = skip + 1)
    test.inds  = thin(test.inds, skip = skip + 1)
  }

  if (length(test.inds) == 0)
    stop("Skip is too large and has removed all resampling instances. Please lower the value of skip.")

  # If the last value if not included, shift everything over by one
  if (test.inds[[length(test.inds)]][desc$horizon] != size) {

    train.inds = lapply(train.inds, function(x) x + 1)
    test.inds = lapply(test.inds, function(x) x + 1)
  }
  desc$iters = length(test.inds)
  makeResampleInstanceInternal(desc, size, train.inds = train.inds, test.inds = test.inds)
}

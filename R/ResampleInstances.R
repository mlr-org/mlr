instantiateResampleInstance = function(desc, size, task) {
  UseMethod("instantiateResampleInstance")
}

instantiateResampleInstance.HoldoutDesc = function(desc, size, task = NULL) {
  inds = sample(size, size*desc$split)
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
  inds = lapply(seq_len(desc$iters), function(x) sample(size, size*desc$split))
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

instantiateResampleInstance.FixedCVDesc = function(desc, size, task = NULL) {
  initial.window = floor(desc$initial.window * size)
  skip = floor(desc$skip * size)
  stops  = (seq(size))[initial.window:(size - desc$horizon)]
  starts = stops - initial.window + 1
  train.inds = mapply(seq, starts, stops, SIMPLIFY = FALSE)
  test.inds  = mapply(seq, stops + 1, stops + desc$horizon, SIMPLIFY = FALSE)

  thin = function(x, skip = 2) {
    n = length(x)
    x[seq(1, n, by = skip)]
  }

  if(skip > 0) {

    train.inds = thin(train.inds, skip = skip+1)
    test.inds = thin(test.inds, skip = skip+1)
  }
  desc$iters = length(test.inds)

  makeResampleInstanceInternal(desc, size, train.inds = train.inds, test.inds = test.inds )
}

instantiateResampleInstance.GrowingCVDesc = function(desc, size, task = NULL) {
  initial.window = floor(desc$initial.window * size)
  skip = floor(desc$skip * size)
  stops  = (seq(from = 1, to = size))[initial.window:(size - desc$horizon)]
  starts = rep(1, length(stops))
  train.inds = mapply(seq, starts, stops, SIMPLIFY = FALSE)
  test.inds  = mapply(seq, stops + 1, stops + desc$horizon, SIMPLIFY = FALSE)
  thin = function(x, skip = 2) {
    n = length(x)
    x[seq(1, n, by = skip)]
  }

  if(skip > 0) {
    train.inds = thin(train.inds, skip = skip+1)
    test.inds  = thin(test.inds, skip = skip+1)
  }
  desc$iters = length(test.inds)
  makeResampleInstanceInternal(desc, size, train.inds = train.inds, test.inds = test.inds )
}

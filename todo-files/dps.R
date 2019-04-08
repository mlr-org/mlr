load_all()

doDPSSplit = function(x) {
  n = nrow(x)
  # the sets for our splits, both index vectors
  s1 = integer(0L)
  s2 = integer(0L)
  # calc dist matrix, this is inefficient, d.old stays, d.new gets reduced later
  # we dont want a object to be close to itself
  d.old = fields::rdist(x)
  diag(d.old) = Inf
  colnames(d.old) = rownames(d.old) = NULL
  d.new = d.old
  
  # run while he have not distributed all elements to s1 or s2
  while(length(s1) + length(s2) < n) {
    # dists for each obs to its nearest neigh
    min.dists = apply(d.new, 1, min)
    # get first element where min dist occurs and its best neighbor, both indices
    nn1 = which.min(min.dists)
    nn2 = which.min(d.new[nn1, ])
    # mean dists of nn1 and nn2 to sets
    nn1.s1 = mean(d.old[nn1, s1])
    nn2.s1 = mean(d.old[nn2, s1])
    nn1.s2 = mean(d.old[nn1, s2])
    nn2.s2 = mean(d.old[nn2, s2])
    # if (nn1 to s1) and (nn2 to s2) have in sum larger mean dists, we like
    # that distribution, not the other ways round, to to increase coverage
    if (length(s1) == 0L || (nn1.s1 + nn2.s2) >= nn1.s2 + nn2.s1) {
      s1 = c(s1, nn1)
      s2 = c(s2, nn2)
    } else {
      s1 = c(s1, nn2)
      s2 = c(s2, nn1)
    }
    # remove both nn1 and nn2 from dists we work on
    d.new[nn1, ] = Inf
    d.new[nn2, ] = Inf
    d.new[, nn1] = Inf
    d.new[, nn2] = Inf
    
    # odd nr of elements, add last guy to set with larger mean dist
    if (length(s1) + length(s2) == n - 1L) {
      last = setdiff(1:n, c(s1, s2))
      if (mean(d.old[last, s1]) > mean(d.old[last, s2]))
        s1 = c(s1, last)
      else
        s2 = c(s2, last)
    }
  }
  list(s1, s2)
}

doDPSSplits = function(x, k, inds = seq_len(nrow(x))) {
  if (log2(nrow(x)) < k) stop("Too many splits")
  if (k == 1L) {
    s.index = doDPSSplit(x)
    s = lapply(s.index, function(X) inds[X])
  } else {
    s.index = doDPSSplit(x)
    x1 = x[s.index[[1L]],]
    x2 = x[s.index[[2L]],]
    s = lapply(s.index, function(X) inds[X])
    s1 = s[[1L]]
    s2 = s[[2L]]
    
    if(any(is.na(x1)) | any(is.na(x2)))
      stop("There are NAs")
    
    s = c(
      doDPSSplits(x1, k - 1L, s1),
      doDPSSplits(x2, k - 1L, s2)
    )
  }
}

makeResampleDescDPS = function(iters = 10L) {
  iters = asCount(iters, positive = TRUE)
  k = log2(iters)
  if (as.integer(k) != k)
    stopf("'iters' must be a power of 2, but it is %i!", iters)
  makeResampleDescInternal("density preserving sampling", iters = iters)
}


instantiateResampleInstance.DPSDesc = function(desc, task) {
  if (desc$iters > size)
    stopf("Cannot use more folds (%i) than size (%i)!", desc$iters, size)
  test.inds = doDPSSplits()
  makeResampleInstanceInternal(desc, size, test.inds = test.inds)
}

# rdesc = makeResampleDesc("CV")
# rin = makeResampleInstance(rdesc, task = iris.task)

x = read.table("todo-files/dps_iris.csv", sep  = ",")
x[,1] = x[,1] + 1:150
# x[10,] = x[10,] + 1e-15
# x[1,] = x[1,] + 1e-15
# x[11,] = x[11,] + 1e-15

u = doDPSSplits(x, k=8, 1:150)
print(sort(u[[1]]))
print(sort(u[[2]]))

# v = read.table("todo-files/folds.csv", sep  = ",")
# v1 = which(v[,1] == 1L)
# v2 = which(v[,2] == 2L)
# print(v1)

# print(s)
# print(sort(unlist(s)))

# rdesc = makeResampleDesc("DPS", iters = 2L)
# rin = makeResampleInstance(rdesc, task = iris.task)

instantiateResampleInstance.DPSDesc = function(desc, size, task) {
  if (missing(size))
    size = getTaskSize(task)
  if (desc$iters > size)
    stopf("Cannot use more folds (%i) than size (%i)!", desc$iters, size)
  x = getTaskData(task, target.extra=TRUE)$data
  if (!all(sapply(x, is.numeric))) stop("All features must be numeric")
  test.inds = doDPSSplits(x, k = log2(desc$iters))
  makeResampleInstanceInternal(desc, size, test.inds = test.inds)
}

doDPSSplit = function(x) {
  n = nrow(x)
  # the sets for our splits, both index vectors
  s1 = integer(0L)
  s2 = integer(0L)
  # calc dist matrix, this is inefficient, d.old stays, d.new gets reduced later
  # we dont want a object to be close to itself
  d.old = fields::rdist(x)
  diag(d.old) = Inf
  colnames(d.old) = rownames(d.old) = NULL
  d.new = d.old
  
  # run while he have not distributed all elements to s1 or s2
  while(length(s1) + length(s2) < n) {
    # dists for each obs to its nearest neigh
    min.dists = apply(d.new, 1, min)
    # get first element where min dist occurs and its best neighbor, both indices
    nn1 = which.min(min.dists)
    nn2 = which.min(d.new[nn1, ])
    # mean dists of nn1 and nn2 to sets
    nn1.s1 = mean(d.old[nn1, s1])
    nn2.s1 = mean(d.old[nn2, s1])
    nn1.s2 = mean(d.old[nn1, s2])
    nn2.s2 = mean(d.old[nn2, s2])
    # if (nn1 to s1) and (nn2 to s2) have in sum larger mean dists, we like
    # that distribution, not the other ways round, to to increase coverage
    if (length(s1) == 0L || (nn1.s1 + nn2.s2) >= nn1.s2 + nn2.s1) {
      s1 = c(s1, nn1)
      s2 = c(s2, nn2)
    } else {
      s1 = c(s1, nn2)
      s2 = c(s2, nn1)
    }
    # remove both nn1 and nn2 from dists we work on
    d.new[nn1, ] = Inf
    d.new[nn2, ] = Inf
    d.new[, nn1] = Inf
    d.new[, nn2] = Inf
    
    # odd nr of elements, add last guy to set with larger mean dist
    if (length(s1) + length(s2) == n - 1L) {
      last = setdiff(1:n, c(s1, s2))
      if (mean(d.old[last, s1]) > mean(d.old[last, s2]))
        s1 = c(s1, last)
      else
        s2 = c(s2, last)
    }
  }
  list(s1, s2)
}

doDPSSplits = function(x, k, inds = seq_len(nrow(x))) {
  if (log2(nrow(x)) < k) stop("Too many splits")
  if (k == 1L) {
    s.index = doDPSSplit(x)
    s = lapply(s.index, function(X) inds[X])
  } else {
    # split data into two data sets
    s.index = doDPSSplit(x)
    x1 = x[s.index[[1L]],]
    x2 = x[s.index[[2L]],]
    
    # get corresponding indices from full data set
    s = lapply(s.index, function(X) inds[X])
    s1 = s[[1L]]
    s2 = s[[2L]]
    
    if (anyMissing(x1) || anyMissing(x2))
      stop("There are NAs")
    
    s = c(
      doDPSSplits(x1, k - 1L, s1),
      doDPSSplits(x2, k - 1L, s2)
    )
  }
}

makeResampleDescDPS = function(iters = 8L) {
  iters = asCount(iters, positive = TRUE)
  k = log2(iters)
  if (as.integer(k) != k)
    stopf("'iters' must be a power of 2, but it is %i!", iters)
  makeResampleDescInternal("density preserving sampling", iters = iters)
}

#FIXME: read this

makePreprocWrapperPCA = function(learner) {
  checkArg(learner, "Learner")

  trainfun = function(data, target, args) {
    cns = colnames(data)
    nums = setdiff(cns[sapply(data, is.numeric)], target)
    if (!length(nums))
      return(list(data = data, control = list()))

    x = data[, nums]
    pca = prcomp(x, scale = TRUE)
    data = data[, setdiff(cns, nums), drop = FALSE]
    data = cbind(data, as.data.frame(pca$x))
    ctrl = list(center = pca$center, scale = pca$scale, rotation = pca$rotation)
    return(list(data = data, control = ctrl))
  }

  predictfun = function(data, target, args, control) {
    # no numeric features ?
    if (!length(control))
      return(data)

    cns = colnames(data)
    nums = cns[sapply(data, is.numeric)]
    x = as.matrix(data[, nums, drop = FALSE])
    x = scale(x, center = control$center, scale = control$scale)
    x = x %*% control$rotation
    data2 = data[, setdiff(cns, nums), drop = FALSE]
    data2 = cbind(data2, as.data.frame(x))
    return(data2)
  }
  makePreprocWrapper(learner, trainfun, predictfun)
}

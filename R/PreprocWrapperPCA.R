#' @title Perform a PCA on all numeric columns in the data set.
#'
#' @description
#' Before training a PCA will be performed on all numeric columns in the trainings dataset.
#' The PCA center, scale and rotation will be saved and applied to the test dataset.
#' Internally uses \code{\link{prcomp}} with \code{scale = TRUE} before training.
#'
#' @template arg_learner
#' @export
#' @family wrapper
#' @template ret_learner

makePreprocWrapperPCA = function(learner) {
  learner = checkLearner(learner)

  trainfun = function(data, target, args) {
    cns = colnames(data)
    nums = setdiff(cns[vlapply(data, is.numeric)], target)
    if (!length(nums))
      return(list(data = data, control = list()))
    x = data[, nums, drop = FALSE]
    pca = prcomp(x, scale = TRUE)
    data = data[, setdiff(cns, nums), drop = FALSE]
    data = cbind(data, as.data.frame(pca$x))
    ctrl = list(center = pca$center, scale = pca$scale, rotation = pca$rotation, pca.colnames = nums)
    list(data = data, control = ctrl)
  }

  predictfun = function(data, target, args, control) {
    # no numeric features ?
    if (!length(control))
      return(data)
    cns = colnames(data)
    nums = control$pca.colnames
    x = as.matrix(data[, nums, drop = FALSE])
    x = scale(x, center = control$center, scale = control$scale)
    x = x %*% control$rotation
    data = data[, setdiff(cns, nums), drop = FALSE]
    cbind(data, as.data.frame(x))
  }
  
  makePreprocWrapper(learner, trainfun, predictfun)
}

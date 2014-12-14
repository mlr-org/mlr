#' @title Perform a ICA on all numeric columns in the data set.
#'   
#' @description Before training a ICA (Independent Component Analysis) will be
#' performed on all numeric columns in the trainings dataset. Internally uses
#' \code{\link{e1071::ica}} before training.
#' 
#' @template arg_learner
#' @param lrate \code{numeric(1)} \cr
#'   learning rate
#' @param ... \cr
#'   see {\code{?e1071::ica}} for further arguments.
#' @export
#' @family wrapper
#' @template ret_learner

makePreprocWrapperICA = function(learner, lrate, ...) {
  learner = checkLearner(learner)

  trainfun = function(data, target, args) {
    cns = colnames(data)
    nums = setdiff(cns[vlapply(data, is.numeric)], target)
    if (!length(nums))
      return(list(data = data, control = list()))
    
    requirePackages("e1071", "makePreprocWrapperICA")
    x = data[, nums, drop = FALSE]
    ica = do.call(e1071::ica, c(list(X = x), args))
    data = data[, setdiff(cns, nums), drop = FALSE]
    data = cbind(data, as.data.frame(ica$projection))
    ctrl = c(dropNamed(ica, "projection"), list(ica.colnames = nums))
    list(data = data, control = ctrl)
  }  

  predictfun = function(data, target, args, control) {
    # no numeric features ?
    if (!length(control))
      return(data)

    cns = colnames(data)
    nums = control$ica.colnames
    x = as.matrix(data[, nums, drop = FALSE])
    x = x %*% t(control$weights)
    data = data[, setdiff(cns, nums), drop = FALSE]
    data = cbind(data, as.data.frame(x))
  }

  par.set = makeParamSet(
    makeNumericLearnerParam("lrate", lower = 0L),
    makeIntegerLearnerParam("epochs", lower = 1L, default = 100L),
    makeIntegerLearnerParam("ncomp", lower = 1L),
    makeDiscreteLearnerParam("fun", values = c("negative kurtosis", "positive kurtosis", "4th moment"), default = "negative kurtosis")
  )

  args = list(lrate = lrate, ...)

  makePreprocWrapper(learner, trainfun, predictfun, par.set, par.vals = args)
}

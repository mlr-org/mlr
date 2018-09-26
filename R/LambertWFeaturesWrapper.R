#' @title Gaussianize Numerical Data with a Lambert W Transform
#'
#' @description
#' Takes numeric data and "Gaussianizes" it using an h, hh, or s Lambert W transform.
#'
#' @template arg_learner
#' @param type [\code{character(1)}] \cr
#' What type of non-normality: symmetric heavy-tails "h" (default),
#'  skewed heavy-tails "hh", or just skewed "s".
#' @param methods [\code{character(1)}] \cr
#'  What estimator should be used: "MLE" or "IGMM". "IGMM" gives exactly
#' Gaussian characteristics (kurtosis = 3 for "h" or skewness = 0 for "s"),
#' "MLE" comes close to this. Default: "IGMM" since it is much faster than "MLE".
#' @param verbose [\code{logical(1)}] \cr
#' If TRUE, it prints out progress information in the console. Default: FALSE.
#' @export
#' @family wrapper
#' @template ret_learner
makePreprocWrapperLambert = function(learner, type = c("h", "hh", "s"),
                                     methods = c("IGMM", "MLE"), verbose = FALSE) {
  type = match.arg(type)
  methods = match.arg(methods)
  learner = checkLearner(learner)
  args = list(type = type, methods = methods, verbose = verbose)
  rm(list = names(args))

  trainfun = function(data, target, args) {
    cns = colnames(data)
    nums = setdiff(cns[sapply(data, is.numeric)], target)
    if (length(nums) == 0)
      stop("None of the data can be put through a Lambert W Transform. Either you do not have numeric data or only have a target.")
    x = as.matrix(data[, nums, drop = FALSE])
    x = LambertW::Gaussianize(x, type = args$type, method = args$methods, verbose = args$verbose,
                              return.tau.mat = TRUE)
    control = args
    control$tau.mat = x$tau.mat

    data = cbindSetDiff(x = x$input, data, cns, nums)
    return(list(data = data, control = control))
  }
  predictfun = function(data, target, args, control) {
    cns = colnames(data)
    nums = setdiff(cns[sapply(data, is.numeric)], target)
    if (length(nums) == 0)
      stop("None of the data can be put through a Lambert W Transform. Either you do not have numeric data or only have a target.")

    x = as.matrix(data[, nums, drop = FALSE])
    x = LambertW::Gaussianize(x, type = control$type, method = control$methods,
                              verbose = control$verbose, tau.mat = control$tau.mat)
    data = cbindSetDiff(x = x$input, data, cns, nums)

    return(data)
  }

  lrn = makePreprocWrapper(
    learner,
    train = trainfun,
    predict = predictfun,
    par.set = makeParamSet(
      makeDiscreteLearnerParam("type", values = c("h", "hh", "s"), default = "s"),
      makeDiscreteLearnerParam("methods", values = c("IGMM", "MLE"), default = "IGMM"),
      makeLogicalLearnerParam("verbose", default = FALSE, tunable = FALSE)
    )
  )
  addClasses(lrn, "PreprocWrapperLambert")
}



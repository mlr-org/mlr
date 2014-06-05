#' @title Downsample (subsample) a task or a data.frame.
#'
#' @description
#'   Decrease the observations in a \code{task} or a \code{data.frame}
#'   to a given percentage or absolute number.
#'
#' @param obj [\code{data.frame} | \code{\link{SupervisedTask}} | \code{\link{ResampleInstance}}]\cr
#'   Input data or a \code{ResampleInstance}.
#' @param target [\code{character(1)}]\cr
#'   Name of the column specifying the response.
#'   Only used when \code{obj} is a \code{data.frame} and \code{stratify = TRUE}, otherwise ignored.
#' @param perc [\code{numeric(1)}]\cr
#'   Percentage of the observations to keep in the downsampled data.
#' @param n [\code{numeric(1)}]\cr
#'   Total number of observations to keep in the downsampled data.
#' @param stratify [\code{boolean(1)}]\cr
#'   Should the downsample be stratified according to the target classes?
#' @seealso \code{\link{makeResampleInstance}}
#' @return [\code{data.frame} | \code{\link{SupervisedTask}} | \code{\link{ResampleInstance}}]. Same type as \code{obj}.
#' @export
downsample = function(obj, target, perc = NULL, n = NULL, stratify = FALSE) {
  checkArg(obj, c("data.frame", "SupervisedTask", "ResampleInstance"))
  checkDownsampleArguments(perc, n, stratify)
  UseMethod("downsample")
}

#' @export
downsample.data.frame = function(obj, target = NULL, perc = NULL, n = NULL, stratify = FALSE) {
  if(!isNotSet(target)) {
    checkArg(target, "character", na.ok=FALSE)
  }
  if(stratify && isNotSet(target)) {
    stop("Stratifying needs a target column!")
  }
  if(stratify && !is.factor(obj[, target])) {
    stop("Stratifying is not supported for data.frames where the target column is not a factor!")
  }
  if(isNotSet(target)) {
    if(is.null(perc)) {
      perc = n / nrow(obj)
    }
    holdoutDesc = makeResampleDesc(method = "Holdout", stratify = stratify, split = perc)
    holdoutInst = makeResampleInstance(desc = holdoutDesc, size = nrow(obj))
    res = obj[holdoutInst$train.inds[[1]], ]
  } else {
    tsk = makeClassifTask(id = "tmp.downsample", data = obj, target = target)
    tsk = downsample(tsk, perc = perc, n = n, stratify = stratify)
    res = getTaskData(tsk)
  }
  res
}

#' @export
downsample.SupervisedTask = function(obj, target, perc = NULL, n = NULL, stratify = FALSE) {
  if(is.null(perc)) {
    perc = n / obj$task.desc$size
  }
  holdoutDesc = makeResampleDesc(method = "Holdout", stratify = stratify, split = perc)
  holdoutInst = makeResampleInstance(desc = holdoutDesc, task = obj)
  subsetTask(task = obj, subset = holdoutInst$train.inds[[1]])
}

#' @export
downsample.ResampleInstance = function(obj, target, perc = NULL, n = NULL, stratify = FALSE) {
  if(stratify) {
    stop("Stratifying is not supported for a ResampleInstance!")
  }
  obj$train.inds = lapply(obj$train.inds, function(x) {
    if(is.null(n)) {
      n = round(perc * length(x))
    }
    if(n > length(x)) {
      warningf("The given n: %i is bigger than the observations in the sample: %i. Continue with using all observations.", n, length(x))
      n = length(x)
    }
    sample(x, size = n, replace = FALSE)
  })
}

checkDownsampleArguments = function(perc = NULL, n = NULL, stratify = FALSE) {
  if(!isNotSet(perc))
    checkArg(perc, "numeric", len=1L, na.ok=FALSE, lower=0, upper=1)
  if(!isNotSet(n))
    checkArg(convertInteger(n), "integer", len=1L, na.ok=FALSE, lower=1L)
  checkArg(stratify, "logical", len=1L, na.ok=FALSE)
  if(!isNotSet(n) && !isNotSet(perc)) {
    stop("You can only subset by n OR a percentage of observations!")
  }
  if(isNotSet(n) && isNotSet(perc)) {
    stop("You have to give a n or a percentage to downsample!")
  }
}


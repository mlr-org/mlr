#' @title Downsample (subsample) a task or a data.frame.
#'
#' @description
#'   Decrease the observations in a \code{task} or a \code{ResampleInstance}
#'   to a given percentage or absolute number.
#'
#' @param obj [\code{\link{SupervisedTask}} | \code{\link{ResampleInstance}}]\cr
#'   Input data or a \code{ResampleInstance}.
#' @param select [\code{character(1)}]\cr
#'   How to select the size of the downsampled dataset.
#'   \dQuote{perc} = select observations by percentage, \dQuote{abs} = select absolute number
#'   of observations.
#'   Default is \dQuote{perc}.
#' @param val [\code{numeric(1)}]\cr
#'   Depends on \code{select}:
#'   Either a percentage from [0, 1] or a number of observations.
#' @param stratify [\code{boolean(1)}]\cr
#'   Should the downsampled data be stratified according to the target classes? Default is \code{FALSE}.
#' @seealso \code{\link{makeResampleInstance}}
#' @return [\code{data.frame} | \code{\link{SupervisedTask}} | \code{\link{ResampleInstance}}]. Same type as \code{obj}.
#' @family downsample
#' @export
downsample = function(obj, select = "perc", val = 1, stratify = FALSE) {
  checkDownsampleArguments(select, val, stratify)
  UseMethod("downsample")
}

#' @export
downsample.SupervisedTask = function(obj, select = "perc", val = 1, stratify = FALSE) {
  if(select == "abs") {
    perc = val / obj$task.desc$size
  } else {
    perc = val
  }
  holdoutDesc = makeResampleDesc(method = "Holdout", stratify = stratify, split = perc)
  holdoutInst = makeResampleInstance(desc = holdoutDesc, task = obj)
  subsetTask(task = obj, subset = holdoutInst$train.inds[[1]])
}

#' @export
downsample.ResampleInstance = function(obj, select = "perc", val = 1, stratify = FALSE) {
  if(stratify) {
    stop("Stratifying is not supported for a ResampleInstance!")
  }
  obj$train.inds = lapply(obj$train.inds, function(x) {
    if(select == "perc") {
      n = round(val * length(x))
    } else {
      n = val
    }
    if(n > length(x)) {
      stopf("The given val = %i is bigger than the observations in the sample: %i.", n, length(x))
    }
    sample(x, size = n)
  })
}

checkDownsampleArguments = function(select, val, stratify) {
  switch(select,
    perc = checkArg(val, "numeric", len = 1L, na.ok = FALSE, lower = 0, upper = 1),
    abs = {
      n = convertInteger(val)
      checkArg(n, "integer", len = 1L, na.ok = FALSE, lower = 1L)
      })
  checkArg(stratify, "logical")
}
#' @title Over- or undersample binary classification task to handle class imbalancy.
#'
#' @description
#'
#' Oversampling: From the smaller class, observations are randomly drawn with repetitions.
#'
#' Undersampling: From the larger class, observations are randomly drawn without repetitions.
#'
#' @param obj [\code{data.frame} | \code{\link{ClassifTask}}]\cr
#'   Input data.
#' @param target [\code{character(1)}]\cr
#'   Name of the column specifying the response.
#'   Only used when \code{obj} is a data.frame, otherwise ignored.
#' @param rate [\code{numeric(1)}]\cr
#'   Factor to upsample the smaller or downsample the bigger class.
#'   For undersampling: Must be between 0 and 1,
#'   where 1 means no downsampling, 0.5 implies reduction to 50 percent
#'   and 0 would imply reduction to 0 observations.
#'   For oversampling: Must be between 1 and \code{Inf},
#'   where 1 means no oversampling and 2 would mean doubling the class size.
#' @return [\code{data.frame} | \code{\link{ClassifTask}}]. Same type as \code{obj}.
#' @export
oversample = function(obj, target, rate) {
  checkArg(obj, c("data.frame", "ClassifTask"))
  UseMethod("oversample")
}

#' @export
oversample.data.frame = function(obj, target, rate) {
  checkArg(target, "character", na.ok=FALSE)
  checkArg(rate, "numeric", len=1L, na.ok=FALSE, lower=0)
  sampleBinaryClass(obj, target, rate, cl=1L, replace=TRUE)
}

#' @export
oversample.ClassifTask = function(obj, target, rate) {
  d = oversample(obj$env$data, obj$task.desc$target, rate)
  changeData(obj, d)
}

#' @rdname oversample
#' @export
undersample = function(obj, target, rate) {
  UseMethod("undersample")
}

#' @export
undersample.data.frame= function(obj, target, rate) {
  checkArg(target, "character", na.ok=FALSE)
  checkArg(rate, "numeric", len=1L, na.ok=FALSE, lower=0, upper=1)
  sampleBinaryClass(obj, target, rate, cl=2L, replace=FALSE)
}

#' @export
undersample.ClassifTask = function(obj, target, rate) {
  d = undersample(obj$env$data, obj$task.desc$target, rate)
  changeData(obj, d)
}

sampleBinaryClass = function(data, target, rate, cl, replace) {
  y = as.character(data[, target])
  if (length(unique(y)) != 2L)
    stopf("Target must have exactly 2 levels")
  tab = table(y)
  small = getMinIndex(tab)
  small = names(tab)[small]
  big = setdiff(names(tab), small)
  cls = if (cl == 1L)
    c(small, big)
  else
    c(big, small)
  i1 = which(y == cls[[1L]])
  i2 = which(y == cls[[2L]])
  newsize = round(length(i1) * rate)
  newinds = sample(i1, newsize, replace=replace)
  data[c(newinds, i2), ]
}

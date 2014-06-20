#' @title Over- or undersample binary classification task to handle class imbalancy.
#'
#' @description
#'
#' Oversampling: From the smaller class, observations are randomly drawn with repetitions.
#'
#' Undersampling: From the larger class, observations are randomly drawn without repetitions.
#'
#' @template arg_task
#' @param rate [\code{numeric(1)}]\cr
#'   Factor to upsample the smaller or downsample the bigger class.
#'   For undersampling: Must be between 0 and 1,
#'   where 1 means no downsampling, 0.5 implies reduction to 50 percent
#'   and 0 would imply reduction to 0 observations.
#'   For oversampling: Must be between 1 and \code{Inf},
#'   where 1 means no oversampling and 2 would mean doubling the class size.
#' @template ret_task
#' @family OverUndersample
#' @export
oversample = function(task, rate) {
  checkTask2(task, "ClassifTask", binary = TRUE)
  checkArg(rate, "numeric", len = 1L, lower = 1)
  d = sampleBinaryClass(task$env$data, task$task.desc$target, rate, cl = 1L, replace = TRUE)
  changeData(task, d)
}

  #' @rdname oversample
#' @export
undersample = function(task, rate) {
  checkArg(task, "ClassifTask")
  checkArg(rate, "numeric", len = 1L, lower = 0, upper = 1)
  d = sampleBinaryClass(task$env$data, task$task.desc$target, rate, cl = 2L, replace = FALSE)
  changeData(task, d)
}

sampleBinaryClass = function(data, target, rate, cl, replace) {
  y = data[, target]
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
  newinds = sample(i1, newsize, replace = replace)
  data[c(newinds, i2), ]
}

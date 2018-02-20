#' @title Over- or undersample binary classification task to handle class imbalancy.
#'
#' @description
#'
#' Oversampling: For a given class (usually the smaller one) all existing observations are
#' taken and copied and extra observations are added by randomly sampling with replacement from this class.
#'
#' Undersampling: For a given class (usually the larger one) the number of observations is
#' reduced (downsampled) by randomly sampling without replacement from this class.
#'
#' @template arg_task
#' @param rate (`numeric(1)`)\cr
#'   Factor to upsample or downsample a class.
#'   For undersampling: Must be between 0 and 1,
#'   where 1 means no downsampling, 0.5 implies reduction to 50 percent
#'   and 0 would imply reduction to 0 observations.
#'   For oversampling: Must be between 1 and `Inf`,
#'   where 1 means no oversampling and 2 would mean doubling the class size.
#'   If `NULL` the rate will calculated automatically so that the minorty classes will have the same sample sizes as the majority class.
#' @param cl (`character(1)`)\cr
#'   Which class should be over- or undersampled. If `NULL`, `oversample`
#'   will select the smaller and `undersample` the larger class.
#' @template ret_task
#' @family imbalancy
#' @export
oversample = function(task, rate = NULL, cl = NULL) {
  checkTask(task, "ClassifTask")
  assertNumber(rate, lower = 1, null.ok = TRUE)
  assertSubset(cl, getTaskClassLevels(task))
  y = getTaskTargets(task)
  z = getMinMaxClass(y)
  r = rate
  min.names = if (is.null(cl)) z$min.names else cl
  j = lapply(min.names, function(s) {
    if (is.null(rate)) {
      r = z$max.size/z$min.size[s]
    }
    sampleMultiClass(y, rate = r, cl = s)
  })
  j = c(unlist(j), which(y %nin% min.names))
  subsetTask(task, j)
}

#' @rdname oversample
#' @export
undersample = function(task, rate, cl = NULL) {
  checkTask(task, "ClassifTask", binary = TRUE)
  assertNumber(rate, lower = 0, upper = 1)
  assertSubset(cl, getTaskClassLevels(task))
  y = getTaskTargets(task)
  if (is.null(cl)) {
    cl = getMinMaxClass(y)$max.name
  }
  j = sampleBinaryClass(y, rate = rate, cl = cl, resample.other.class = FALSE)
  subsetTask(task, j)
}


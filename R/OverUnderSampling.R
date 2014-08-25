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
#' @family imbalancy
#' @export
oversample = function(task, rate) {
  checkTask(task, "ClassifTask", binary = TRUE)
  assertNumber(rate, lower = 1)
  j = sampleBinaryClass(getTaskTargets(task), rate, cl = "min", clreplace = TRUE, 
    othreplace = FALSE, bagging = FALSE)
  subsetTask(task, j)
}

#' @rdname oversample
#' @export
undersample = function(task, rate) {
  checkTask(task, "ClassifTask", binary = TRUE)
  assertNumber(rate, lower = 0, upper = 1)
  j = sampleBinaryClass(getTaskTargets(task), rate, cl = "max", clreplace = FALSE,
    othreplace = FALSE, bagging = FALSE)
  subsetTask(task, j)
}

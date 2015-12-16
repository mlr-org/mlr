#' @title Over- or undersample binary classification task to handle class imbalancy.
#'
#' @description
#'
#' Oversampling: Observations are randomly drawn with repetitions.
#'
#' Undersampling: Observations are randomly drawn without repetitions.
#'
#' @template arg_task
#' @param rate [\code{numeric(1)}]\cr
#'   Factor to upsample or downsample a class.
#'   For undersampling: Must be between 0 and 1,
#'   where 1 means no downsampling, 0.5 implies reduction to 50 percent
#'   and 0 would imply reduction to 0 observations.
#'   For oversampling: Must be between 1 and \code{Inf},
#'   where 1 means no oversampling and 2 would mean doubling the class size.
#' @param cl [\code{character(1)}]\cr
#'   Which class should be over- or undersampled. If \code{NULL}, \code{oversample}
#'   will take the smaller class and \code{undersample} the bigger one.
#' @template ret_task
#' @family imbalancy
#' @export
oversample = function(task, rate, cl = NULL) {
  checkTask(task, "ClassifTask", binary = TRUE)
  assertNumber(rate, lower = 1)
  cl.nullcheck = testNull(cl)
  if (!cl.nullcheck) {
    assertCharacter(cl, max.len = 1L)
  }
  y = getTaskTargets(task)
  if (cl.nullcheck) {
    cl = getMinMaxClass(y)$min.name
  } else {
    assertChoice(cl, levels(y))
  }
  j = sampleBinaryClass(y, rate = rate, cl = cl, clreplace = TRUE, 
    othreplace = FALSE)
  subsetTask(task, j)
}

#' @rdname oversample
#' @export
undersample = function(task, rate, cl = NULL) {
  checkTask(task, "ClassifTask", binary = TRUE)
  assertNumber(rate, upper = 1)
  cl.nullcheck = testNull(cl)
  if (!cl.nullcheck) {
    assertCharacter(cl, max.len = 1L)
  }
  y = getTaskTargets(task)
  if (cl.nullcheck) {
    cl = getMinMaxClass(y)$max.name
  } else {
    assertChoice(cl, levels(y))
  }
  j = sampleBinaryClass(y, rate = rate, cl = cl, clreplace = FALSE,
    othreplace = FALSE)
  subsetTask(task, j)
}

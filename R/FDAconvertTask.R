#' @title Create task on functional data features.
#'
#' @description
#' The function transform a task of type \code{FDAClassifTask} into a standard
#' \code{ClassifTask}. For this, it creates a feature representation of the raw
#' functional data. The method used to create this feature representation must
#' be specified by the user in \code{method}. For \code{wavelets} or
#' \code{fourier} features, the resulting data does not contain temporal
#' structure anymore, so the returned task is a \code{ClassifTask}. For
#' \code{shapelets}, the learned shapelet model is returned. See
#' \code{{getTSShapeletFeatures}}.
#'
#'
#' @param task [\code{FDAClassifTask}]\cr
#'   Functional data classification task.
#' @param method [\code{character(1)}]\cr
#'   Which method is used to create time series features. Methods available.
#'   Wavelet transformation: \dQuote{wavelets}.
#'   Fourier transformation: \dQuote{fourier}.
#'   Shapelet model learning: \dQuote{shapelets}.
#' @param pars \cr
#'   Further parameters passed as argument e.g., for feature representation
#'   methods. See the methods' man pages.
#' @return Either [\code{ClassifTask}] based on the transformed data or the
#'   learned shapelet model [\code{ShapeletModel}].
#' @export
convertFDATaskToNormalTask = function(task, method, pars = NULL) {
  # check if task
  assertClass(task, classes = c("Task", "FDAClassifTask"))
  assertChoice(method, choices = c("wavelets", "fourier", "shapelets"))

  target = task$task.desc$target
  z = getTaskData(task, target.extra = TRUE, recode.target = "-1+1")

  if (method == "shapelets") {
    new.args = c(list(curves = z$data, label = as.factor(z$target)), as.list(unlist(pars)))
    modelSh = do.call(getFDAShapeletFeatures, new.args)
    return(modelSh)
  }

  tsf = extractFDAFeatures(data = z$data, target = target, method = method, args = pars)

  newdata = cbind(as.factor(z$target), tsf)
  colnames(newdata)[1] <- target  # rename target column
  newtask = makeClassifTask(data = newdata, target = target, positive = task$task.desc$positive)
  return(newtask)
}

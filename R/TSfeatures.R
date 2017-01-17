#' @title Create task on time series features.
#'
#' @description The function creates a feature representation of raw time series
#'   for a time series classification task. The method used to create this
#'   feature representation must be specified by the user in \code{method}.
#'   For \code{wavelets} or \code{fourier} features, the resulting data does not
#'   contain temporal structure anymore, so the returned task is a \code{ClassifTask}.
#'   For \code{shapelets}, the learned shapelet model is returned. See
#'   \code{\link{getTSShapeletFeatures}}.
#'
#'
#' @param task [\code{TimeSeriesClassifTask}]\cr
#'   Time series classification task.
#' @param method [\code{character(1)}]\cr
#'   Which method is used to create time series features. Two methods available.
#'   Wavelet transformation: \dQuote{wavelets}.
#'   Fourier transformation: \dQuote{fourier}.
#' @param pars  \cr
#'   Further parameters passed as argument e.g., for feature representation
#'   methods.
#'   Wavelet transformation: \code{filter} and \code{boundary}.
#'   Fourier transformation: \code{fft.coeff}.
#'
#' @return Either [\code{ClassifTask}] or the learned shapelet model.
#' @export
convertTSTaskToNormalTask = function(task, method, pars = NULL) {
  # check if task
  assertClass(task, classes = "Task")

  #check for Time Series Classif Task
  if (!any(class(task) == "TimeSeriesClassifTask"))
    stop("Task is not a 'TimeSeriesClassifTask'. Please check task.")
  #check valid feature extraction method
  if (!(method %in%  c("wavelets", "fourier", "shapelets")))
    stop("Method for feature extraction must be one of 'wavelets' or 'fourier' or 'shapelets'. Please check method.")
  # #check for valid pars
  # if (!(all(names(pars) %in% c("filter", "boundary", "fft.coeff"))))
  #   stop("Pars includes non valid arguments. Must be filter or boundary (wavelets) or fft.coeff (fourier).")

  target = task$task.desc$target
  z = getTaskData(task, target.extra = TRUE, recode.target = "-1+1")

  switch(method,
         wavelets = {tsf = getTSWaveletFeatures(data = z$data, target = target, include.target = FALSE, filter = pars$filter, boundary = pars$boundary)},
         fourier = {tsf = getTSFourierFeatures(data = z$data, target = target, include.target = FALSE, fft.coeff = pars$fft.coeff)}
         )

  if (method == "shapelets") {
    #FIXME: how to correctly use pars here for additional parameters?
    modelSh = getTSShapeletFeatures(curves = z$data, label.train = z$target )
    return(modelSh)
  }

  newdata = cbind(as.factor(z$target), tsf)
  colnames(newdata)[1] <- target  # rename target column
  newtask = makeClassifTask(data = newdata, target = target, positive = task$task.desc$positive)
  return(newtask)
}

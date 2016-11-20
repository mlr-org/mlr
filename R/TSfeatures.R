#' @title Create task on time series features.
#'
#' @description The function creates a feature representation of raw time series
#'   for a time series classification task. The method used to create this
#'   feature representation must be specified by the user in \code{method}.
#'   Since the resulting data does not contain temporal structure anymore, the
#'   returned task is a \code{ClassifTask}.
#'
#'
#' @param task [\code{TimeSeriesClassifTask}]\cr Time series classification
#'   task.
#' @param method [\code{character(1)}]\cr Which method is used to create time
#'   series features. Two methods available. Wavelet transformation:
#'   \dQuote{wavelets} Fourier transformation: \dQuote{fourier}
#' @param pars  \cr Further parameters passed as argument e.g., for feature
#'   representation methods. For wavelets, \code{filter} and \code{boundary}.
#'   For fourier,\code{fft.coeff}.
#' @return [\code{ClassifTask}].
#' @export
makeTSFeaturesClassifTask = function(task, method, pars = NULL) {

  # check if task
  assertClass(task, classes = "Task")

  #check for Time Series Classif Task
  if ( !any(class(task) == "TimeSeriesClassifTask") )
    stop("Task is not a 'TimeSeriesClassifTask'. Please check task.")
  #check valid feature extraction method
  if ( !(method %in%  c("wavelets", "fourier")) )
    stop("Method for feature extraction must be one of 'wavelets' or 'fourier'. Please check method.")
  #check for valid pars
  if ( !(all(names(pars) %in% c("filter", "boundary", "fft.coeff"))) )
    stop("Pars includes non valid arguments. Must be filter or boundary (wavelets) or fft.coeff (fourier).")



  z = getTaskData(task, target.extra = TRUE)
  switch(method,
         wavelets = {tsf = getTSWaveletFeatures(curves = z$data, filter = pars$filter, boundary = pars$boundary)},
         fourier = {tsf = getTSFourierFeatures(curves = z$data, fft.coeff = pars$fft.coeff)}
         )
  tsf = cbind(as.factor(z$target), tsf)
  colnames(tsf)[1] <- task$task.desc$target  # rename target column
  newtask = makeClassifTask(data = tsf, target = task$task.desc$target, positive = task$task.desc$positive)
  return(newtask)

}

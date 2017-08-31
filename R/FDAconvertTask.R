#' @title Create task on functional data features.
#'
#' @description
#' The function transforms a task of type \code{\link{FDAClassifTask}} into a standard
#' \code{\link{ClassifTask}}. For this, it creates a feature representation of the raw
#' functional data. The method used to create this feature representation must
#' be specified by the user in method. For wavelets or
#' fourier features, the resulting data does not contain temporal
#' structure anymore, so the returned task is a \code{\link{ClassifTask}}.
#'
#' @param task [\code{\link{FDATask}}]\cr
#'   Functional data classification task.
#' @param method [\code{character(1)}]\cr
#'   Which method is used to create time series features. Methods available.
#'   Wavelet transformation: \dQuote{wavelets}.
#'   Fourier transformation: \dQuote{fourier}.
#'   Shapelet model learning: \dQuote{shapelets}.
#' @param pars \cr
#'   Further parameters passed as argument e.g., for feature representation
#'   methods. See the methods man pages.
#' @return Either [\code{\link{ClassifTask}}] based on the transformed data or the
#'   learned shapelet model.
#' @export
convertFDATaskToNormalTask = function(task, method, pars = NULL, positive) {
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

  res = extractFDAFeatures(data = z$data, target = target, method = method, args = pars)
  tsf = res$feat
  newdata = cbind(as.factor(z$target), tsf)
  colnames(newdata)[1] = target  # rename target column
  newtask = makeClassifTask(data = newdata, target = target, positive = task$task.desc$positive)
  newtask$task.desc$meta = res$meta
  return(newtask)
}





#' @title Transformer from FDATask to normal machine learning regression task.
#'
#' @description
#' The function transform a task of type \code{\link{FDATask}} into a standard
#' \code{\link{RegrTask}}. For this, it creates a feature representation of the raw
#' functional data. The method used to create this feature representation must
#' be specified by the user in method. For wavelets or
#' fourier features, the resulting data does not contain temporal
#' structure anymore, so the returned task is a \code{\link{RegrTask}}.
#'
#' @param task [\code{\link{FDATask}}]\cr
#'   Functional data analysis task.
#' @param method [\code{character(1)}]\cr
#'   Which method is used to create curve features. Methods available.
# FIXME: link to the doc list of feature extraction methods. maybe in ?extractFDAFeatures?
#'   Wavelet transformation: \dQuote{wavelets}.
#'   Fourier transformation: \dQuote{fourier}.
#'   Shapelet model learning: \dQuote{shapelets}.
#'   FIXME: should add more here.
#' @param pars \cr
#'   Further parameters passed as argument e.g., for feature representation
#'   methods. See the methods man pages.
#' @return \code{\link{RegrTask}} based on the transformed data.
#' @export
#FIXME: what happens with scalar corvars? we REALL DONT want to throw them away!
trafoFDATaskToRegrTask = function(task, method, pars = NULL) {
  assertClass(task, classes = "FDATask")
  target = task$task.desc$target
  fdf = task$task.desc$fd.features
  z = getTaskData(task, target.extra = TRUE)
  # FIXME: extractMultiFDAFeatures should return "clean" names. potentially we would
  # like to recognize the "block" from the name? so like V1_1?
  tsf = extractMultiFDAFeatures(data = z$data, target = target, fd.features = fdf, method = method, args = pars)$feat
  newdata = as.data.frame(cbind((z$target), tsf))
  # FIXME: we have a problem if the target is called Vi!
  # FIXME: maybe make the extracted feature names a bit "stranger"?
  # FIXME: and in the rare case we get a name clash throw error here!
  # make sure that the feature column names are unique
  colnames(newdata) = c(target, paste0('V', 1:(ncol(newdata) - 1) ))
  newtask = makeRegrTask(data = newdata, target = target)
  return(newtask)
}
# this is quick and dirty ,all copy and paste
#' @export
trafoFDATaskToClassifTask = function(task, method, pars = NULL) {
  assertClass(task, classes = "FDATask")
  target = task$task.desc$target
  fdf = task$task.desc$fd.features
  z = getTaskData(task, target.extra = TRUE)
  # FIXME: extractMultiFDAFeatures should return "clean" names. potentially we would
  # like to recognize the "block" from the name? so like V1_1?
  res = extractMultiFDAFeatures(data = z$data, target = target, fd.features = fdf, method = method, args = pars)
  tsf = res$feat
  newdata = as.data.frame(cbind((z$target), tsf))
  # FIXME: we have a problem if the target is called Vi!
  # FIXME: maybe make the extracted feature names a bit "stranger"?
  # FIXME: and in the rare case we get a name clash throw error here!
  # make sure that the feature column names are unique
  colnames(newdata) = c(target, paste0('V', 1:(ncol(newdata) - 1) ))
  newdata[, target] = as.factor(newdata[, target]) # difference between regression and classification
  newtask = makeClassifTask(data = newdata, target = target, positive = task$task.desc$positive)
  newtask$task.desc$meta = res$meta
  return(newtask)
}



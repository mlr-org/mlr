#' @title Set missing factor levels in test data to NA
#' @description
#' Sets missing factor levels in test data to NA to avoid prediction error for lm models
#' @param model A lm* model fitted on training data.
#' @param test.data [\code{data.frame}] \cr
#'   Data set for prediction.
#' @return A \code{data.frame} with matching factor levels of test data and fitted model
missingLevelsTrain = function(model, test.data) {
  lmod = getLearnerModel(model)
  UseMethod("missingLevelsTrain", lmod)
}

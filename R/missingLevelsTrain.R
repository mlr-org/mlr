#' @title Set missing factor levels in test data to NA
#' @description
#' Sets missing factor levels in test data to NA to avoid prediction error for lm models
#' @param model \cr
#'   A model fitted on training data.
#' @param test.data [`data.frame`] \cr
#'   Data set for prediction.
#' @return `data.frame` with matching factor levels to fitted model
missingLevelsTrain = function(model, test.data) {
  lmod = getLearnerModel(model)
  UseMethod("missingLevelsTrain", lmod)
}

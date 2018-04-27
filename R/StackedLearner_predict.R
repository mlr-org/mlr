# Predict a Stacked Learner.
#
# Gets predictions from `getStackedBaseLearnerPredictions. Then apply algo:
#  \describe{
#  \item{`aggregate}}{Aggregate predictions using [`aggregatePredictions`]}.}
#   \item{`ensembleselection`}{Expand prediction according to frequency chosen in training,
#         then apply aggregation as for `aggregate`.}
#   \item{`superlearner}}{Convert list of \code{Prediction` to data.frame
#   (for classification with `predict.type = "prob"` the first feature
#    is removed to overcome mullticollinearity). Add original features if needed.
#    Apply meta model from training.}
#  }
# @param .learner ([`StackedLearner`])
# @param .model [`BaseEnsembleModel`]
# @param .newdata [`data.frame`]\cr Data to predict on
# @param ... ...
# @return Predictions are returned in matrix or vector.
#' @export
predictLearner.StackedLearner = function(.learner, .model, .newdata, ...) {

  # obtain predictions
  pred.list = getStackedBaseLearnerPredictions(model = .model, newdata = .newdata)

  # apply aggregate
  if (.learner$method == "aggregate") {
    final.pred = aggregatePredictions(.model, pred.list)

  # apply ensembleselection
  } else if (.learner$method == "ensembleselection") {
    final.pred = aggregatePredictions(.model, pred.list)

  # apply superlearner
  } else if (.learner$method == "superlearner") {
    use.feat = .model$learner$use.feat
    pred.data = lapply(pred.list, function(x) getPredictionDataNonMulticoll(x))
    pred.data = as.data.frame(pred.data)
    if (use.feat) {
      feat = .newdata[, colnames(.newdata) %nin% getTaskDesc(.model)$target, drop = FALSE]
      pred.data = cbind(pred.data, feat)
    }
    sm = .model$learner.model$super.model
    if (getMlrOption("show.info"))
      messagef("[Super Learner] Predict %s with %s features on %s observations", sm$learner$id, ncol(pred.data), nrow(pred.data))
    final.pred = predict(sm, newdata = pred.data)
  }
  # return
  if (.model$learner$predict.type == "prob") {
    return(as.matrix(getPredictionProbabilities(final.pred, cl = .model$task.desc$class.levels)))
  } else {
    return(final.pred$data$response)
  }
}

# @title Returns the predictions for each base learner.
#
# @description Returns the predictions for each base learner based on `newdata`.
#   If `newdata` is not supported, training prediction will be returned.
#   For `ensembleselection` prediction is only applyed to base models which were selected.
#   Predictions are made based on `model$learner.model$base.models}. If \code{save.on.disc = TRUE`
#   only a name file is saved which direct to a RDS containing the model.
#
# @param model [`BaseEnsembleModel`]\cr
#   BaseEnsembleModel, result of train.
# @param newdata [`data.frame`]\cr
#   New observations, for which the predictions using the specified base learners should be returned.
#   Default is `NULL` and extracts the base learner predictions that were made during the training.
getStackedBaseLearnerPredictions = function(model, newdata = NULL){

  if (is.null(newdata)) {
    pred = model$learner.model$pred.train
  } else {
    bms = model$learner.model$base.models
    # Get predictions from all basemodels
    pred = lapply(names(bms), function(x) {
      # Load model if it was saved on disc
      if (model$learner$save.on.disc)
        bms[[x]] = readRDS(bms[[x]])
      # Predict on newdata
      predict(bms[[x]], newdata = newdata)}
    )
  }
  return(setNames(pred, names(bms)))
}

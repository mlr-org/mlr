#' Predict a Stacked Learner.
#'
#' Gets predictions from \code{getStackedBaseLearnerPredictions}. Then apply algo:
#'  \describe{
#'   \item{\code{aggregate}}{Aggregate predictions using \code{\link{aggregatePredictions}}.}
#'   \item{\code{ensembleselection}}{Expand prediction according to frequency chosen in training, then apply aggregation as for 'aggregate'.}
#'   \item{\code{superlearner}}{Convert list of \code{Prediction} to data.frame
#'   (for classification with \code{predict.type = "prob"} the first feature
#'    is removed to overcome mullticollinearity). Add original features if needed.
#'    Apply meta model from training.}
#'  }
#' @param .learner ([`StackedLearner`])
#' @param .model [`BaseEnsembleModel`]
#' @param .newdata [`data.frame`]\cr Data to predict on
#' @param ... ...
#' @return Predictions are returned in matrix or vector.
#' @export
predictLearner.StackedLearner = function(.learner, .model, .newdata, ...) { # FIXME actually only .learner$method is needed
  # setup
  use.feat = .model$learner$use.feat
  sm.pt = .model$learner$predict.type
  bm.pt = unique(extractSubList(.model$learner$base.learners, "predict.type"))
  if (length(bm.pt) > 1) stopf("Prediction types of all base learners must be identical.")
  td = .model$task.desc
  method = .learner$method

  # obtain predictions
  pred.list = getStackedBaseLearnerPredictions(model = .model, newdata = .newdata)

  # apply aggregate
  if (method == "aggregate") {
    final.pred = aggregatePredictions(pred.list, sm.pt = sm.pt, pL = FALSE)
  # apply ensembleselection
  } else if (method == "ensembleselection") {
    freq = .model$learner.model$freq
    pred.list = expandPredList(pred.list, freq = freq)
    final.pred = aggregatePredictions(pred.list, sm.pt = sm.pt, pL = FALSE)
  # apply superlearner
  } else if (method == "superlearner") {
    pred.data = lapply(pred.list, function(x) getPredictionDataNonMulticoll(x))
    pred.data = as.data.frame(pred.data)
    if (use.feat) {
      feat = .newdata[, colnames(.newdata) %nin% td$target, drop = FALSE]
      pred.data = cbind(pred.data, feat)
    }
    sm = .model$learner.model$super.model
    if (getMlrOption("show.info"))
      messagef("[Super Learner] Predict %s with %s features on %s observations", sm$learner$id, ncol(pred.data), nrow(pred.data))
    final.pred = predict(sm, newdata = pred.data)
  }
  # return
  if (sm.pt == "prob") {
    return(as.matrix(getPredictionProbabilities(final.pred, cl = td$class.levels)))
  } else {
    return(final.pred$data$response)
  }
}

#' @title Returns the predictions for each base learner.
#'
#' @description Returns the predictions for each base learner based on \code{newdata}.
#'   If \code{newdata} is not supported, training prediction will be returned.
#'   For \code{ensembleselection} prediction is only applyed to base models which were selected.
#'   Predictions are made based on \code{model$learner.model$base.models}. If \code{save.on.disc = TRUE}
#'   only a name file is saved which direct to a RDS containing the model.
#'
#' @param model [\code{BaseEnsembleModel}]\cr
#'   BaseEnsembleModel, result of train.
#' @param newdata [\code{data.frame}]\cr
#'   New observations, for which the predictions using the specified base learners should be returned.
#'   Default is \code{NULL} and extracts the base learner predictions that were made during the training.
#' @export

getStackedBaseLearnerPredictions = function(model, newdata = NULL){
  stack.id = model$learner$id
  # checking
  if (is.null(newdata)) {
    pred = model$learner.model$pred.train
  } else {
    # get base learner and predict type
    method = model$learner.model$method
    if (method == "ensembleselection") {
      # only apply prediction to models which are relevant for ensembleselection
      used.bls = names(which(model$learner.model$freq > 0))
      bms = model$learner.model$base.models[used.bls]
    } else {
      bms = model$learner.model$base.models
    }
    pred = vector("list", length(bms))
    # Prediction
    # 1. models from RDS file
    if (model$learner$save.on.disc) {
      for (i in seq_along(bms)) { # FIXME: do in parallel
        m = readRDS(bms[[i]])
        pred[[i]] = predict(m, newdata = newdata)
      }
      bls.names = sapply(bms, function(x) convertModelNameToBlsName(x, stack.id))
    } else {
    # 2. models from object
      for (i in seq_along(bms)) {
        pred[[i]] = predict(bms[[i]], newdata = newdata)
      }
      bls.names = sapply(bms, function(X) X$learner$id) #names(.learner$base.learners)
    }
    names(pred) = bls.names
    # FIXME I don
    #broke.idx.pd = which(unlist(lapply(pred, function(x) checkIfNullOrAnyNA(x))))
    #if (length(broke.idx.pd) > 0) {
    #  messagef("Preds '%s' is broken in 'getStackedBaseLearnerPredictions' and will be removed\n", names(bls)[broke.idx])
    #  pred.data = pred.data[-broke.idx.pd, drop = FALSE]
    #  pred = pred[-broke.idx.pd, drop = FALSE]
    #}
  }
  pred
}

### other helpers ###

# Sets the predict.type for the super learner of a stacked learner
# @param learner ([StackedLearner]).
# @param predict.type (`character(1)`)\cr
#        `prob` for probabilities or `response` for classes.
#' @export
setPredictType.StackedLearner = function(learner, predict.type) {
  lrn = setPredictType.Learner(learner, predict.type)
  lrn$predict.type = predict.type
  if ("super.learner" %in% names(lrn)) lrn$super.learner$predict.type = predict.type
  return(lrn)
}


# Create a super learner task
#
# @param type "classif" or "regr"
# @param data data
# @param target target as character
makeSuperLearnerTask = function(type, data, target) {
  keep.idx = colSums(is.na(data)) == 0
  data = data[, keep.idx, drop = FALSE]
  if (getMlrOption("show.info") & (length(keep.idx) < ncol(data)))
    warningf("Feature '%s' will be removed\n", names(data)[!keep.idx])
  if (type == "classif") {
    removeConstantFeatures(obj = makeClassifTask(id = "level1data",
      data = data, target = target, fixup.data = "no"))
  } else {
    removeConstantFeatures(obj = makeRegrTask(id = "level1data",
      data = data, target = target, fixup.data = "no"))
  }
}


# Training and prediction in one function (used for parallelMap)
#
# @param bls [list of base.learner]
# @param task [Task]
# @param show.info show.info
# @param id Id needed to create unique model name
# @param save.on.disc Directory to save models to.
doTrainPredict = function(bls, task, show.info, id, save.on.disc) {
  setSlaveOptions()
  model = train(bls, task)

  if (show.info)
    messagef("[Base Learner] %s applied.", bl$id)
  pred = predict(model, task = task)

  if (!is.null(save.on.disc)) {
    model.file = tempfile(paste("model", id, bls$id, sep = "."), tmpdir = save.on.disc, fileext = ".RData")
    saveRDS(model, file = model.file)
    if (show.info) messagef("[Base Learner] %s applied. Model saved as %s", bls$id, model.file)
  }

  # Either return model or a path to the model
  if (!is.null(save.on.disc)) model = model.file
  return(list(base.models = model))
}

# Resampling and prediction in one function (used for parallelMap)
# @param bls [list of base.learner]
# @param task [Task]
# @param rin Resample Description
# @param measures Measures for resampling
# @param show.info show.info
# @param id Id needed to create unique model name
# @param save.on.disc save.on.disc
doTrainResample = function(bl, task, rin, measures, show.info, id, save.on.disc) {

  setSlaveOptions()
  assertClass(rin, "ResampleInstance")

  # Get OOB Predictions für train step
  r = resample(bl, task, rin, measures, show.info = FALSE)
  oob.preds = r$pred$data[order(r$pred$data$id), , drop = FALSE]

  # Model for prediction on newdata
  model = train(bl, task)

  if (show.info)
    messagef("[Base Learner] %s applied.", bl$id)

  if (!is.null(save.on.disc)) {
    model.file = tempfile(paste("model", id, bls$id, sep = "."), tmpdir = save.on.disc, fileext = ".RData")
    saveRDS(model, file = model.file)
    messagef("Model saved to %s", model.file)
  }

  # Either return model or a path to the model
  if (!is.null(save.on.disc)) model = model.file
  return(list(base.models = model, resres = r))
}


# Aggregate predictions
#
# Aggregate predicitons results by averaging (for \code{regr}, and  \code{classif} with prob) or mode ( \code{classif} with response).
# (works for regr, classif, multiclass)
#
# @param pred.list [list of \code{Predictions}]\cr
# @param predict.type Final predict type, "prob" or "response"
# @param pL FALSE if Predictions with truth (test data), TRUE for truth=NA (new data)
# FIXME: add more methods (geometric mean, rank specific stuff)
aggregateModelPredictions = function(.model, pred.list, weights = NULL) {

  # return pred if list only contains one pred
  if (length(pred.list) == 1) {
    return(pred.list[[1]])
  }
  assertList(pred.list)

  # Check if all task.descs are equal
  x = lapply(pred.list, function(x) getTaskDesc(x))
  task.desc = x[[1]]
  td.equal = unlist(lapply(2:length(x), function(i) all.equal(task.desc, x[[i]])))
  if (any(!td.equal)) stopf("Task descriptions in prediction '1' and '%s' differ!", which(task.unequal)[1])

  # Define weights (1/n for aggregation, weighted for ensembleselection)
  if (is.null(weights)) {
    weights = rep(1, length(pred.list)) / length(pred.list)
  }

  predict.type = .model$predict.type
  class.levels = task.desc$class.levels

  if (.model$type == "classif") {
    # Get probabilities as a matrix.
    preds = lapply(pred.list, function(pred, class.levels) {
      if(pred$predict.type == "prob") {
        as.matrix(getPredictionProbabilities(pred, class.levels))
      } else {
        as.matrix(createDummyFeatures(getPredictionResponse(pred))[, class.levels])
      }
    }, class.levels)
    y = apply(simplify2array(preds), c(1, 2), weighted.mean,  w = weights, na.rm = TRUE)
    # In case we want to predict the response, get the max over all columns
    if (predict.type == "response") y = factor(max.col(y), labels = class.levels)
  } else {
    preds = lapply(pred.list, getPredictionResponse)
    y = apply(simplify2array(preds), c(1, 2), weighted.mean, w = weights, na.rm = TRUE)
  }
  return(makePrediction(task.desc = task.desc, row.names(pred.list[[1]]$data),
    id = .model$learner$id, truth = pred.list[[1]]$data$truth, predict.type = predict.type,
    predict.threshold = NULL, y, time = NA_real_))
}


# Returns response or probabilites from Prediction.
# The first feature of a multiclass classification prediction will be removed
# in order to overcome multicollinearity problems.
# @param pred Prediction from predict or resample
getPredictionDataNonMulticoll = function(pred) {
  if (any(class(pred) == "ResampleResult")) {
    pred = pred$pred
  }
  pt = pred$predict.type
  td = getTaskDesc(pred)
  # if classification with probabilities
  if (pt == "prob") {
    pred.matrix = pred$data[, paste("prob", td$class.levels, sep = ".")]
    colnames(pred.matrix) = td$class.levels
    pred.matrix[, -1, drop = TRUE]
  } else {
    # for perdict.type = "response"
    getPredictionResponse(pred)
  }
}


# Plot the staacked learner and additional available info
plotStackedLearnerModel = function(mod) {
  # Create the data.frame
  bms = mod$learner.model$base.models

  method = mod$learner.model$method
  df = data.frame(
    "xcenter" = seq(0, 1, length.out = length(bms)),
    "names" = names(bms)
  )

  if (method %in% c("superlearner", "ensembleselection"))
    df$perf = mod$learner.model$bls.perf
  if (method == "ensembleselection")
    df$freq = mod$learner.model$freq

  # Plot depending on method
  p = ggplot2::ggplot(df)
  if (method %in% c("superlearner", "ensebleselection")) {
  # Color boxes by performance and add performance
    p = p +
      ggplot2::geom_tile(aes(x = xcenter, y = 0, fill = perf), color = NA,
        width = 1 / (length(bms) + 1), height = 0.4, alpha = 0.7) +
      ggplot2::geom_text(aes(x = xcenter, y = - 0.1, label = round(perf, 2), fontface = "bold"))
  }
  if (method == "superlearner") {
    measure = getDefaultMeasure(mod$task.desc)
    p = p +
      ggplot2::annotate("tile", x = 0.5, y = 1, width = 0.6, height = 0.4, fill = "white", color = "black") +
      ggplot2::annotate("text", x = 0.5, y = 1.1, label = "Superlearner:") +
      ggplot2::annotate("text", x = 0.5, y = 0.9, label = mod$learner.model$super.model$learner$id) +
      ggplot2::guides(fill=guide_legend(title = measure$id))
  }

  if (method == "ensembleselection") {
    measure = mod$learner.model$measure
    p = p +
      ggplot2::geom_text(aes(x = xcenter, y = 0.3, label = freq)) +
      ggplot2::guides(fill=guide_legend(title = measure$id))
  }

  p = p +
    ggplot2::geom_tile(aes(x = xcenter, y = 0), color = "black", fill = NA, width = 1 / (length(bms) + 1), height = 0.4) +
    ggplot2::geom_text(aes(x = xcenter, y = 0.1, label = names), fontface = "bold") +
    ggplot2::scale_fill_continuous(low = "lightgreen", high = "darkgreen") +
    ggplot2::coord_cartesian(ylim = c(- 1, 2)) +
    ggplot2::theme_void()
  invisible(p)
}


# # Expand Predictions according to frequency argument
# #
# # @param pred.list [\code{list} of \code{Predictions}]\cr
# #  List of Predictions which should be expanded.
# # @param freq [\code{named vector}]\cr
# #  Named vector containing the frequency of the chosen predictions.
# #  Vector names must be set to the model names.
# expandPredList = function(pred.list, freq) {
#   assertClass(pred.list, "list")
#   assertClass(freq, "numeric")
#   # checkListElementClass(pred.list, "Prediction")
#   only.preds = unique(unlist(lapply(pred.list, function(x) any(class(x) == "Prediction"))))
#   if (!only.preds) stopf("List elements in 'pred.list' are not all of class 'Prediction'")
#
#   keep = names(which(freq > 0))
#   freq = freq[keep]
#   pred.list = pred.list[keep]
#   grid = data.frame(model = names(freq), freq, row.names = NULL)
#   expand = as.character(rep(grid$model, grid$freq))
#   final.pred.list = vector("list", length(expand))
#   names(final.pred.list) = paste(expand, 1:length(expand), sep = "_")
#
#   for (i in seq_along(expand)) {
#     use = expand[i]
#     final.pred.list[i] = pred.list[use]
#   }
#   final.pred.list
#

# # Order a scores vector and return the best init numbers
# orderScore = function(scores, minimize, init) {
#   # checks
#   assertClass(scores, "numeric")
#   assertChoice(minimize, c(TRUE, FALSE))
#   assertInt(init, lower = 1, upper = length(scores))
#   # body
#   if (is.null(init)) init = length(scores)
#   if (minimize) {
#     order(scores)[1:init]
#   } else {
#     rev(order(scores))[1:init]
#   }
# }


#' #' Remove Stacking models from disc.
#' #'
#' #' @param stack.id (`character(1)`)\cr
#' #' Name of stack.
#' #' @param bls.ids (`character(1)`)\cr
#' #' Vector of base learner names.
#' #' @export
#' removeStackingModelsOnDisc = function(stack.id = NULL, bls.ids = NULL) {
#'   term = paste0("rm saved.model.", stack.id, "*", bls.ids, ".RData")
#'   system(term)
#' }
#'

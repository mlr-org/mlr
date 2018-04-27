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

# Returns response from Prediction object
# @param pred Prediction
# @param full.matrix Wether all n prediction values should be returned or in case of binary classification only one
getResponse = function(pred, full.matrix = NULL) {
  # if classification with probabilities
  if (pred$predict.type == "prob") {
    if (full.matrix) {
      # return matrix of probabilities
      td = pred$task.desc
      predReturn = pred$data[, paste("prob", td$class.levels, sep = ".")]
      colnames(predReturn) = td$class.levels
      return(predReturn)
    } else {
      # return only vector of probabilities for binary classification
      return(getPredictionProbabilities(pred))
    }
  } else {
    # for regression case
    getPredictionResponse(pred)
  }
}


# Returns response or probabilites from Prediction with the speciality that the
# first feature of a multiclass classification prediction will be removed to
# overcome multicollinearity problems
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
      pred.matrix = pred.matrix[, -1, drop = TRUE] #
      return(pred.matrix)
  } else {
    # for perdict.type = "response"
    getPredictionResponse(pred)
  }
}

# Create a super learner task
#
# @param type "classif" or "regr"
# @param data data
# @param target target as character
# FIXME: "save" version which rm constant features and features with NAs. BUT
# might not be useful owing to the fact that predictLearner does not know which
# features are removed
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

# Count the ratio (used if base.learner predict.type = "response" and
# super.learner predict.type is "prob")
# @param pred.data Prediction data
# @param levels Target levels of classifiaction task
# @param model.weight Model weights, default is 1/number of data points
rowWiseRatio = function(pred.data, levels, model.weight = NULL) {
  m = length(levels)
  p = ncol(pred.data)
  if (is.null(model.weight)) {
    model.weight = rep(1/p, p)
  }
  mat = matrix(0,nrow(pred.data), m)
  for (i in 1:m) {
    ids = matrix(pred.data == levels[i], nrow(pred.data), p)
    for (j in 1:p)
      ids[, j] = ids[, j] * model.weight[j]
    mat[, i] = rowSums(ids)
  }
  colnames(mat) = levels
  return(mat)
}


# Training and prediction in one function (used for parallelMap)
# @param bls [list of base.learner]
# @param task [Task]
# @param show.info show.info
# @param id Id needed to create unique model name
# @param save.on.disc save.on.disc
doTrainPredict = function(bls, task, show.info, id, save.on.disc) {
  setSlaveOptions()
  model = train(bls, task)
  pred = predict(model, task = task)
  if (save.on.disc) {
    model.id = paste("saved.model", id, bls$id, "RData", sep = ".")
    saveRDS(model, file = model.id)
    if (show.info)
      messagef("[Base Learner] %s applied. Model saved as %s", bls$id, model.id)
    X = list(base.models = model.id, pred = pred)
  } else { # save.on.disc = FALSE:
    if (show.info)
      messagef("[Base Learner] %s is applied. ", bls$id)
    X = list(base.models = model, pred = pred)
  }
 X
}

# Resampling and prediction in one function (used for parallelMap)
# @param bls [list of base.learner]
# @param task [Task]
# @param rin Resample Description
# @param measures Measures for resampling
# @param show.info show.info
# @param id Id needed to create unique model name
# @param save.on.disc save.on.disc
doTrainResample = function(bls, task, rin, measures, show.info, id, save.on.disc) {
  setSlaveOptions()
  model = train(bls, task)
  r = resample(bls, task, rin, measures, show.info = FALSE)
  if (save.on.disc) {
    model.id = paste("saved.model", id, bls$id, "RData", sep = ".")
    saveRDS(model, file = model.id)
    if (show.info)
      messagef("[Base Learner] %s applied. Model saved as %s", bls$id, model.id)
    X = list(base.models = model.id, resres = r)
  } else { # save.on.disc = FALSE:
    if (show.info)
      messagef("[Base Learner] %s applied.", bls$id)
    X = list(base.models = model, resres = r)
  }
  #print(paste(object.size(r)[1]/1000000, "MB"))
  X
}


# Check if NULL or any NA in x
checkIfNullOrAnyNA = function(x) {
  if (is.null(x)) return(TRUE)
  if (any(is.na(x))) return(TRUE)
  else FALSE
}


# Order a scores vector and return the best init numbers
orderScore = function(scores, minimize, init) {
  # checks
  assertClass(scores, "numeric")
  assertChoice(minimize, c(TRUE, FALSE))
  assertInt(init, lower = 1, upper = length(scores))
  # body
  if (is.null(init)) init = length(scores)
  if (minimize) {
    order(scores)[1:init]
  } else {
    rev(order(scores))[1:init]
  }
}

# Convert models names (when model was saved on disc) to base learner name
# @param base.model.id Unique ID used to save model on disc
# @param stack.id ID from makeStackedLearner
convertModelNameToBlsName = function(base.model.id, stack.id) {
  id = substr(base.model.id, 1, nchar(base.model.id) - 6) # remove .RData
  id = substr(id, 13 + nchar(stack.id) + 1, nchar(id))
  id
}

#' Remove Stacking models from disc.
#'
#' @param stack.id (`character(1)`)\cr
#' Name of stack.
#' @param bls.ids (`character(1)`)\cr
#' Vector of base learner names.
#' @export
removeStackingModelsOnDisc = function(stack.id = NULL, bls.ids = NULL) {
  term = paste0("rm saved.model.", stack.id, "*", bls.ids, ".RData")
  system(term)
}


# Aggregate predictions
#
# Aggregate predicitons results by averaging (for \code{regr}, and  \code{classif} with prob) or mode ( \code{classif} with response).
# (works for regr, classif, multiclass)
#
# @param pred.list [list of \code{Predictions}]\cr
# @param sm.pt Final predict type, "prob" or "response"
# @param pL FALSE if Predictions with truth (test data), TRUE for truth=NA (new data)
# FIXME: add more methods (geometric mean, rank specific stuff)
aggregatePredictions = function(pred.list, sm.pt = NULL, pL = FALSE) {
  # return pred if list only contains one pred
  if (length(pred.list) == 1) {
    #messagef("'pred.list' has only one prediction and returns that one unlisted. Argument 'sm.pt' will not be applied.")
    return(pred.list[[1]])
  }
  # Check if "equal"
  x = lapply(pred.list, function(x) getTaskDesc(x))
  task.unequal = unlist(lapply(2:length(x), function(i) !all.equal(x[[1]], x[[i]])))
  if (any(task.unequal)) stopf("Task descriptions in prediction '1' and '%s' differ. This is not possible!", which(task.unequal)[1])

  x = lapply(pred.list, function(x) x$predict.type)
  pts.unequal = unlist(lapply(2:length(x), function(i) !all.equal(x[[1]], x[[i]])))
  if (any(pts.unequal)) stopf("Predict type in prediction '1' and '%s' differ. This is not possible!",  which(pts.unequal)[1])

  #x = unlist(lapply(pred.list, function(x) checkIfNullOrAnyNA(x$data$response)))
  #print(which(x))
  #print(pred.list)
  #if (any(x)) messagef("Prediction '%s' is broken and will be removed.", which(x))
  #pred.list = pred.list[!x]

  # Body
  pred1 = pred.list[[1]]
  type = getTaskType(pred1)
  td = getTaskDesc(pred1)
  rn = row.names(pred1$data)
  pt = pred1$predict.type
  if (is.null(sm.pt)) sm.pt = pt

  assertChoice(sm.pt, choices = c("prob", "response"))
  ti = NA_real_
  pred.length = length(pred.list)

  # Reduce results
  # type = "classif"
  if (type == "classif") {
    # pt = "prob"
    if (pt == "prob") {
      # same method for sm.pt response and prob
      preds = lapply(pred.list, getPredictionProbabilities, cl = td$class.levels)
      y = Reduce("+", preds) / pred.length
      if (sm.pt == "response") {
        y = factor(max.col(y), labels = td$class.levels)
      }
    # pt = "response"
    } else {
      if (sm.pt == "response") {
        preds = as.data.frame(lapply(pred.list, getPredictionResponse))
        y = factor(apply(preds, 1L, computeMode), td$class.levels)
      } else {
        # rowWiseRatio copied from Tong He (he said it's not the best solution).
        # This method should be rarely used, because pt = "response",
        # sm.pt = "prob" should perfrom worse than setting pt = "prob" (due to
        # information loss when convertring probs to factors)
        preds = as.data.frame(lapply(pred.list, function(x) x$data$response))
        y = rowWiseRatio(preds, td$class.levels, model.weight = NULL)
      }
    }
  # type = "regr"
  } else {
    preds = lapply(pred.list, getPredictionResponse)
    y = Reduce("+", preds)/pred.length
  }
  if (pL) {
    nNA = rep(NA, NROW(y))
      return(makePrediction(task.desc = td, rn, id = nNA, truth = nNA, predict.type = sm.pt, predict.threshold = NULL, y, time = ti))
  } else {
    id = pred1$data$id
    tr = pred1$data$truth
    return(makePrediction(task.desc = td, rn, id = id, truth = tr, predict.type = sm.pt, predict.threshold = NULL, y, time = ti))
  }
}


# Expand Predictions according to frequency argument
#
# @param pred.list [\code{list} of \code{Predictions}]\cr
#  List of Predictions which should be expanded.
# @param freq [\code{named vector}]\cr
#  Named vector containing the frequency of the chosen predictions.
#  Vector names must be set to the model names.
expandPredList = function(pred.list, freq) {
  assertClass(pred.list, "list")
  assertClass(freq, "numeric")
  # checkListElementClass(pred.list, "Prediction")
  only.preds = unique(unlist(lapply(pred.list, function(x) any(class(x) == "Prediction"))))
  if (!only.preds) stopf("List elements in 'pred.list' are not all of class 'Prediction'")

  keep = names(which(freq > 0))
  freq = freq[keep]
  pred.list = pred.list[keep]
  grid = data.frame(model = names(freq), freq, row.names = NULL)
  expand = as.character(rep(grid$model, grid$freq))
  final.pred.list = vector("list", length(expand))
  names(final.pred.list) = paste(expand, 1:length(expand), sep = "_")

  for (i in seq_along(expand)) {
    use = expand[i]
    final.pred.list[i] = pred.list[use]
  }
 final.pred.list
}

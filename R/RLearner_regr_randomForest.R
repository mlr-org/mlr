#' @export
makeRLearner.regr.randomForest = function() {
  makeRLearnerRegr(
    cl = "regr.randomForest",
    package = "randomForest",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "ntree", default = 500L, lower = 1L),
      makeIntegerLearnerParam(id = "se.ntree", default = 100L, lower = 1L, when = "both", requires = quote(se.method == "bootstrap")),
      makeDiscreteLearnerParam(id = "se.method", default = "sd",
        values = c("bootstrap", "jackknife", "sd"),
        requires = quote(se.method %in% "jackknife" && keep.inbag == TRUE),
        when = "both"),
      makeIntegerLearnerParam(id = "se.boot", default = 50L, lower = 1L, when = "both"),
      makeIntegerLearnerParam(id = "mtry", lower = 1L),
      makeLogicalLearnerParam(id = "replace", default = TRUE),
      makeUntypedLearnerParam(id = "strata", tunable = FALSE),
      makeIntegerVectorLearnerParam(id = "sampsize", lower = 1L),
      makeIntegerLearnerParam(id = "nodesize", default = 5L, lower = 1L),
      makeIntegerLearnerParam(id = "maxnodes", lower = 1L),
      makeLogicalLearnerParam(id = "importance", default = FALSE),
      makeLogicalLearnerParam(id = "localImp", default = FALSE),
      makeIntegerLearnerParam(id = "nPerm", default = 1L),
      makeLogicalLearnerParam(id = "proximity", default = FALSE, tunable = FALSE),
      makeLogicalLearnerParam(id = "oob.prox", requires = quote(proximity == TRUE), tunable = FALSE),
      makeLogicalLearnerParam(id = "do.trace", default = FALSE, tunable = FALSE),
      makeLogicalLearnerParam(id = "keep.forest", default = TRUE, tunable = FALSE),
      makeLogicalLearnerParam(id = "keep.inbag", default = FALSE, tunable = FALSE)
    ),
    properties = c("numerics", "factors", "ordered", "se", "oobpreds", "featimp"),
    name = "Random Forest",
    short.name = "rf",
    note = "See the section about 'regr.randomForest' in `?makeLearner` for information about se estimation. Note that the rf can freeze the R process if trained on a task with 1 feature which is constant. This can happen in feature forward selection, also due to resampling, and you need to remove such features with removeConstantFeatures. keep.inbag is NULL by default but if predict.type = 'se' and se.method = 'jackknife' (the default) then it is automatically set to TRUE.",
    callees = "randomForest"
  )
}

#' @export
trainLearner.regr.randomForest = function(.learner, .task, .subset, .weights = NULL, se.method = "sd", keep.inbag = NULL, se.boot = 50L, se.ntree = 100L, ...) {
  data = getTaskData(.task, .subset, target.extra = TRUE)
  if (is.null(keep.inbag)) keep.inbag = (se.method == "jackknife" && .learner$predict.type == "se")
  m = randomForest::randomForest(x = data[["data"]], y = data[["target"]], keep.inbag = keep.inbag, ...)
  if (.learner$predict.type == "se" && se.method == "bootstrap") {
    base.lrn = setPredictType(.learner, "response")
    base.lrn = setHyperPars(base.lrn, ntree = se.ntree)
    bag.rf = makeBaggingWrapper(base.lrn, se.boot, bw.replace = TRUE)
    m2 = train(bag.rf, .task, .subset, .weights)
    m = list(single.model = m, bagged.models = m2)
  }
  return(m)
}

#' @export
predictLearner.regr.randomForest = function(.learner, .model, .newdata, se.method = "sd", ...) {
  if (se.method == "bootstrap") {
    pred = predict(.model$learner.model$single.model, newdata = .newdata, ...)
  } else {
    pred = predict(.model$learner.model, newdata = .newdata, predict.all = (.learner$predict.type == "se"), ...)
  }
  if (.learner$predict.type == "se") {
    if (se.method == "bootstrap") {
      se = bootstrapStandardError(.learner, .model, .newdata, ...)
      return(cbind(pred, se))
    } else if (se.method == "jackknife") {
      se = jacknifeStandardError(
        aggregated.predictions = pred$aggregate,
        individual.predictions = pred$individual,
        bag.counts = .model$learner.model$inbag)
    } else if (se.method == "sd") {
      se = sdStandardError(individual.predictions = pred$individual)
    }
    return(cbind(pred$aggregate, se))
  } else {
    return(pred)
  }
}

#' @export
getOOBPredsLearner.regr.randomForest = function(.learner, .model) {
  getLearnerModel(.model, more.unwrap = TRUE)$predicted
}

# Computes brute force or noisy bootstrap
# Set ntree = se.ntree for the brute force bootstrap
# Set se.ntree << ntree for the noisy bootstrap (mc bias corrected)
bootstrapStandardError = function(.learner, .model, .newdata,
  se.ntree = 100L, se.boot = 50L, ...) {

  single.model = getLearnerModel(.model)$single.model # get raw RF model
  bagged.models = getLearnerModel(getLearnerModel(.model)$bagged.models) # get list of unbagged mlr models
  pred.bagged = lapply(bagged.models, function(x) predict(getLearnerModel(x), newdata = .newdata, predict.all = TRUE))
  pred.boot.all = extractSubList(pred.bagged, "individual", simplify = FALSE)
  ntree = single.model$ntree
  # following the formula in 3.3 in Sexton and Laake 2009 - Standard errors for bagged and random forest estimators
  # M = ntree    # number of ensembles
  # R = se.ntree # new (reduced) number of ensembles
  # B = se.boot  # number of bootstrap samples
  # Bias is defined as
  # (1/R - 1/M) / (BR*(R-1)) *
  # (sum over all B:
  #   (sum over all R:
  #     (prediction for x of ensemble r in bootstrap b - average prediction for x over all ensambles in bootsrap b )^2
  #   )
  # )
  bias = rowSums(matrix(vapply(pred.boot.all, function(p) rowSums(p - rowMeans(p))^2, numeric(nrow(pred.boot.all[[1]]))), nrow = nrow(.newdata), ncol = se.boot, byrow = FALSE))
  bias = ((1 / se.ntree) - (1 / ntree)) / (se.boot * se.ntree * (se.ntree - 1)) * bias
  pred.boot.aggregated = extractSubList(pred.bagged, "aggregate")
  pred.boot.aggregated = matrix(pred.boot.aggregated, nrow = nrow(.newdata), ncol = se.boot, byrow = FALSE)
  var.boot = apply(pred.boot.aggregated, 1, var) - bias
  var.boot = pmax(var.boot, 0)
  sqrt(var.boot)
}

# Computes the mc bias-corrected jackknife after bootstrap
# @param aggregated.predictions `vector(n)`
#   Vector of length n of predictions, aggregated over all individual predictions
# @param individual.predictions `matrix`
#   The individual predictions. Each row represents one individual and each column represents the predictions of one base learner.
# @param bag.counts `matrix`
#   These are the inbag counts of the model. Each row represents an observation of the training set and each row represents one base learner.
#   The number indicates how often this observation exists in the bootstrap sample for the respective base learner.
jacknifeStandardError = function(aggregated.predictions, individual.predictions, bag.counts) {

  nbase = ncol(individual.predictions)
  bag.counts = bag.counts[rowSums(bag.counts == 0) > 0, , drop = FALSE]
  n = nrow(bag.counts)
  oob = bag.counts == 0
  jack.n = apply(oob, 1, function(x) rowMeans(individual.predictions[, x, drop = FALSE]))
  if (is.vector(jack.n)) {
    jack.n = t(as.matrix(jack.n))
  }
  jack = (n - 1) / n * rowSums((jack.n - aggregated.predictions)^2)
  bias = (exp(1) - 1) * n / nbase^2 * rowSums((individual.predictions - aggregated.predictions)^2)
  jab = pmax(jack - bias, 0)
  sqrt(jab)
}

# computes the standard deviation across trees
# @param individual.predictions `matrix`
#   The individual predictions. Each row represents one individual and each column represents the predictions of one base learner.
sdStandardError = function(individual.predictions) {
  apply(individual.predictions, 1, sd)
}

#' @export
getFeatureImportanceLearner.regr.randomForest = function(.learner, .model, ...) {
  getFeatureImportanceLearner.classif.randomForest(.learner, .model, ...)
}

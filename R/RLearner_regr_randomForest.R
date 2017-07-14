#' @title RandomForest regression learner.
#'
#' @description
#' mlr learner for regression tasks using \code{\link[randomForest]{randomForest}}.
#'
#' This doc page exists, as we added additional uncertainty estimation functionality
#' (\code{predict.type = "se"}) for the randomForest, which is not provided by the underlying package.
#'
#' Currently implemented methods are:
#'
#' \itemize{
#' \item If \code{se.method = "jackknife"}, the default, the standard error of a prediction
#'   is estimated by computing the jackknife-after-bootstrap, the mean-squared difference between
#'   the prediction made by only using trees which did not contain said observation and
#'   the ensemble prediction.
#' \item If \code{se.method = "bootstrap"} the standard error of a prediction is estimated by
#'   bootstrapping the random forest, where the number of bootstrap replicates and the number of
#'   trees in the ensemble are controlled by \code{se.boot} and \code{se.ntree} respectively,
#'   and then taking the standard deviation of the bootstrap predictions. The "brute force" bootstrap
#'   is executed when \code{ntree = se.ntree}, the latter of which controls the number of trees in the
#'   individual random forests which are bootstrapped. The "noisy bootstrap" is executed when \code{se.ntree < ntree}
#'   which is less computationally expensive. A Monte-Carlo bias correction may make the latter option
#'   prefarable in many cases. Defaults are \code{se.boot = 50} and \code{se.ntree = 100}.
#'
#' \item If \code{se.method = "sd"}, the standard deviation of the predictions across trees is
#'   returned as the variance estimate.
#'   This can be computed quickly but is also a very naive estimator.
#' }
#'
#' For both \dQuote{jackknife} and \dQuote{bootstrap}, a Monte-Carlo bias correction is applied and,
#' in the case that this results in a negative variance estimate, the values are truncated at 0.
#'
#' Note that when using the \dQuote{jackknife} procedure for se estimation, using a small number of
#' trees can lead to training data observations that are never out-of-bag. The current implementation
#' ignores these observations, but in the original definition, the resulting se estimation would be undefined.
#'
#' Please note that all of the mentioned \code{se.method} variants do not affect the computation
#' of the posterior mean \dQuote{response} value. This is always the same as from the underlying
#' randomForest.
#'
#' @references [Joseph Sexton] and [Petter Laake];
#' [Standard errors for bagged and random forest estimators],
#' Computational Statistics and Data Analysis Volume 53, 2009, [801-811].
#' Also see: [Stefan Wager], [Trevor Hastie], and [Bradley Efron];
#' [Confidence Intervals for Random Forests: The Jackknife and the Infinitesimal Jackknife],
#' Journal of Machine Learning Research Volume 15, 2014, [1625-1651].
#'
#' @name regr.randomForest
#' @rdname regr.randomForest
NULL

#' @export
makeRLearner.regr.randomForest = function() {
  makeRLearnerRegr(
    cl = "regr.randomForest",
    package = "randomForest",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "ntree", default = 500L, lower = 1L),
      makeIntegerLearnerParam(id = "se.ntree", default = 100L, lower = 1L, when = "both", requires = quote(se.method == "bootstrap")),
      makeDiscreteLearnerParam(id = "se.method", default = "jackknife",
        values = c("bootstrap", "jackknife",  "sd"),
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
    note = "See `?regr.randomForest` for information about se estimation. Note that the rf can freeze the R process if trained on a task with 1 feature which is constant. This can happen in feature forward selection, also due to resampling, and you need to remove such features with removeConstantFeatures. keep.inbag is NULL by default but if predict.type = 'se' and se.method = 'jackknife' (the default) then it is automatically set to TRUE.",
    callees = "randomForest"
  )
}

#' @export
trainLearner.regr.randomForest = function(.learner, .task, .subset, .weights = NULL, se.method = "jackknife", keep.inbag = NULL, se.boot = 50L, se.ntree = 100L, ...) {
  data = getTaskData(.task, .subset, target.extra = TRUE)
  m = randomForest::randomForest(x = data[["data"]], y = data[["target"]],
    keep.inbag = if (is.null(keep.inbag)) TRUE else keep.inbag, ...)
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
predictLearner.regr.randomForest = function(.learner, .model, .newdata, se.method = "jackknife", ...) {
  if (se.method == "bootstrap")
    pred = predict(.model$learner.model$single.model, newdata = .newdata, ...)
  else
    pred = predict(.model$learner.model, newdata = .newdata, ...)
  if (.learner$predict.type == "se") {
    se.fun = switch(se.method,
      bootstrap = bootstrapStandardError,
      jackknife = jackknifeStandardError,
      sd = sdStandardError
    )
    se = se.fun(.learner, .model, .newdata, ...)
    return(cbind(pred, se))
  } else {
    return(pred)
  }
}

#' @export
getOOBPredsLearner.regr.randomForest = function(.learner, .model) {
  .model$learner.model$predicted
}

# Computes brute force or noisy bootstrap
# Set ntree = se.ntree for the brute force bootstrap
# Set se.ntree << ntree for the noisy bootstrap (mc bias corrected)
bootstrapStandardError = function(.learner, .model, .newdata,
  se.ntree = 100L, se.boot = 50L, ...) {
  single.model = getLearnerModel(.model)$single.model #get raw RF model
  bagged.models = getLearnerModel(getLearnerModel(.model)$bagged.models) #get list of unbagged mlr models
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
  bist = ((1 / se.ntree) - (1 / ntree)) / (se.boot * se.ntree * (se.ntree - 1)) * bias
  pred.boot.aggregated = extractSubList(pred.bagged, "aggregate")
  pred.boot.aggregated = matrix(pred.boot.aggregated, nrow = nrow(.newdata), ncol = se.boot, byrow = FALSE)
  var.boot = apply(pred.boot.aggregated, 1, var) - bias
  var.boot = pmax(var.boot, 0)
  sqrt(var.boot)
}

# Computes the mc bias-corrected jackknife after bootstrap
jackknifeStandardError = function(.learner, .model, .newdata, ...) {
  model = .model$learner.model
  model$inbag = model$inbag[rowSums(model$inbag == 0) > 0, , drop = FALSE]
  n = nrow(model$inbag)
  ntree = model$ntree
  pred = predict(model, newdata = .newdata, predict.all = TRUE, ...)
  oob = model$inbag == 0
  jack.n = apply(oob, 1, function(x) rowMeans(pred$individual[, x, drop = FALSE]))
  if (is.vector(jack.n)) {
    jack.n = t(as.matrix(jack.n))
  }
  jack = (n - 1) / n * rowSums((jack.n - pred$aggregate)^2)
  bias = (exp(1) - 1) * n / ntree^2 * rowSums((pred$individual - pred$aggregate)^2)
  jab = pmax(jack - bias, 0)
  sqrt(jab)
}

# computes the standard deviation across trees
sdStandardError = function(.learner, .model, .newdata, ...) {
  pred = predict(.model$learner.model, newdata = .newdata, predict.all = TRUE, ...)
  apply(pred$individual, 1, sd)
}

#' @export
getFeatureImportanceLearner.regr.randomForest = function(.learner, .model, ...) {
  getFeatureImportanceLearner.classif.randomForest(.learner, .model, ...)
}

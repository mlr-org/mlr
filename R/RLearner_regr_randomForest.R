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
#'   trees in the ensemble are controlled by \code{se.boot} and \code{ntree.for.se} respectively,
#'   and then taking the standard deviation of the bootstrap predictions. The "brute force" bootstrap
#'   is executed when \code{ntree = ntree.for.se}, the latter of which controls the number of trees in the
#'   individual random forests which are bootstrapped. The "noisy bootstrap" is executed when \code{ntree.for.se < ntree}
#'   which is less computationally expensive. A Monte-Carlo bias correction may make the latter option
#'   prefarable in many cases. Defaults are \code{se.boot = 50} and \code{ntree.for.se = 100}.
#'
#' \item If \code{se.method = "sd"}, the standard deviation of the predictions across trees is
#'   returned as the variance estimate.
#'   This can be computed quickly but is also a very naive estimator.
#' }
#'
#' For both \dQuote{jackknife} and \dQuote{bootstrap}, a Monte-Carlo bias correction is applied and,
#' in the case that this results in a negative variance estimate, the values are truncated at 0.
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
      makeIntegerLearnerParam(id = "ntree.for.se", default = 100L, lower = 1L),
      makeDiscreteLearnerParam(id = "se.method", default = "jackknife",
                               values = c("bootstrap", "jackknife",  "sd"),
                               requires = quote(se.method %in% c("jackknife") && keep.inbag == TRUE)),
      makeIntegerLearnerParam(id = "se.boot", default = 50L, lower = 1L),
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
    note = "See `?regr.randomForest` for information about se estimation. Note that the rf can freeze the R process if trained on a task with 1 feature which is constant. This can happen in feature forward selection, also due to resampling, and you need to remove such features with removeConstantFeatures. keep.inbag is NULL by default but if predict.type = 'se' and se.method = 'jackknife' (the default) then it is automatically set to TRUE."
  )
}

#' @export
trainLearner.regr.randomForest = function(.learner, .task, .subset, .weights = NULL,
  se.method = "jackknife", keep.inbag = NULL, se.boot = 50L, ntree.for.se = 100L, ...) {
  if (.learner$predict.type == "se" & .learner$par.vals$se.method == "bootstrap") {
    base.lrn = setPredictType(.learner, "response")
    base.lrn = setHyperPars(base.lrn, ntree = .learner$par.vals$ntree.for.se)
    bag.rf = makeBaggingWrapper(base.lrn, .learner$par.vals$se.boot, bw.replace = TRUE)
    m = train(bag.rf, .task, .subset, .weights)
  } else {
    data = getTaskData(.task, .subset, target.extra = TRUE)
    m = randomForest::randomForest(x = data[["data"]], y = data[["target"]],
      keep.inbag = if (is.null(keep.inbag)) TRUE else keep.inbag, ...)
  }
  return(m)
}

#' @export
predictLearner.regr.randomForest = function(.learner, .model, .newdata, ...) {
  if (.learner$predict.type == "se") {
    se.fun = switch(.learner$par.vals$se.method,
      bootstrap = bootstrapStandardError,
      jackknife = jackknifeStandardError,
      sd = sdStandardError
    )
    se.fun(.learner, .model, .newdata, ...)
  } else {
    predict(.model$learner.model, newdata = .newdata, ...)
  }
}

#' @export
getOOBPredsLearner.regr.randomForest = function(.learner, .model) {
  .model$learner.model$predicted
}

# Computes brute force or noisy bootstrap
# Set ntree = ntree.for.se for the brute force bootstrap
# Set ntree.for.se << ntree for the noisy bootstrap (mc bias corrected)
bootstrapStandardError = function(.learner, .model, .newdata, ...) {
  pred.all.boot = lapply(getLearnerModel(.model$learner.model), function(x)
    predict(x$learner.model, newdata = .newdata, predict.all = TRUE)$individual)
  b = .learner$par.vals$se.boot
  r = .learner$par.vals$ntree.for.se
  m = .learner$par.vals$ntree
  bias = ((1 / r) - (1 / m)) / (b * r * (r - 1)) *
    rowSums(matrix(sapply(pred.all.boot, function(p) rowSums((p - mean(p))^2)),
                   nrow = nrow(.newdata), ncol = r, byrow = TRUE))
  pred = getPredictionResponse(predict(.model$learner.model, newdata = .newdata))
  pred.boot = lapply(getLearnerModel(.model$learner.model), predict, newdata = .newdata, ...)
  pred.boot = extractSubList(pred.boot, c("data", "response"))
  if (is.vector(pred.boot)) {
    pred.boot = matrix(pred.boot, nrow = nrow(.newdata), ncol = r, byrow = TRUE)
  }
  var.boot = apply(pred.boot, 1, var) - bias
  var.boot = pmax(var.boot, 0)
  cbind(pred, sqrt(var.boot))
}

# Computes the mc bias-corrected jackknife after bootstrap
jackknifeStandardError = function(.learner, .model, .newdata, ...) {
  model = .model$learner.model
  n = nrow(model$inbag)
  ntree = model$ntree
  pred = predict(model, newdata = .newdata, predict.all = TRUE)
  oob = model$inbag == 0
  jack.n = apply(oob, 1, function(x) rowMeans(pred$individual[, x, drop = FALSE]))
  if (is.vector(jack.n)) {
    jack.n = t(as.matrix(jack.n))
  }
  jack = (n - 1) / n * rowSums((jack.n - pred$aggregate)^2)
  bias = (exp(1) - 1) * n / ntree^2 * rowSums((pred$individual - pred$aggregate)^2)
  jab = pmax(jack - bias, 0)
  return(cbind(pred$aggregate, sqrt(jab)))
}

# computes the standard deviation across trees
sdStandardError = function(.learner, .model, .newdata, ...) {
  pred = predict(.model$learner.model, newdata = .newdata, predict.all = TRUE, ...)
  se = apply(pred$individual, 1, sd)
  return(cbind(pred$aggregate, se))
}

#' @export
getFeatureImportanceLearner.regr.randomForest = function(.learner, .model, ...) {
  getFeatureImportanceLearner.classif.randomForest(.learner, .model, ...)
}

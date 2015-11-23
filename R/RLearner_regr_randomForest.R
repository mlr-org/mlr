# FIXME: document BS options

#' @export
makeRLearner.regr.randomForest = function() {
  makeRLearnerRegr(
    cl = "regr.randomForest",
    package = "randomForest",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "ntree", default = 500L, lower = 1L),
      makeIntegerLearnerParam(id = "ntree.for.se", default = 100L, lower = 1L),
      makeDiscreteLearnerParam(id = "se.method", default = "infjackknife",
                               values = c("bootstrap", "jackknife", "infjackknife", "sd"),
                               requires = quote(se.method %in% c("jackknife", "infjackknife") &&
                                                  keep.inbag == TRUE)),
      makeIntegerLearnerParam(id = "se.boot", default = 50L, lower = 1L),
      makeLogicalLearnerParam(id = "calibrate", default = TRUE, tunable = FALSE),
      makeIntegerLearnerParam(id = "mtry", lower = 1L),
      makeLogicalLearnerParam(id = "replace", default = TRUE),
      makeIntegerLearnerParam(id = "sampsize", lower = 1L),
      makeIntegerLearnerParam(id = "nodesize", default = 5L, lower = 1L),
      makeIntegerLearnerParam(id = "maxnodes", lower = 1L),
      makeLogicalLearnerParam(id = "importance", default = FALSE),
      makeLogicalLearnerParam(id = "localImp", default = FALSE),
      makeLogicalLearnerParam(id = "do.trace", default = FALSE, tunable = FALSE),
      makeLogicalLearnerParam(id = "keep.inbag", default = FALSE, tunable = FALSE)
    ),
    par.vals = list(
      se.method = "bootstrap",
      se.boot = 50L,
      ntree.for.se = 100L
    ),
    properties = c("numerics", "factors", "ordered", "se"),
    name = "Random Forest",
    short.name = "rf"
  )
}

#' @export
trainLearner.regr.randomForest = function(.learner, .task, .subset, .weights = NULL, ...) {
  if (.learner$predict.type == "se" &
        .learner$par.vals$se.method == "bootstrap") {
    base.lrn = setPredictType(.learner, "response")
    base.lrn = setHyperPars(base.lrn, ntree = .learner$par.vals$ntree.for.se)
    bag.rf = makeBaggingWrapper(base.lrn, .learner$par.vals$se.boot, bw.replace = TRUE)
    m = train(bag.rf, .task, .subset, .weights)
  } else {
    data = getTaskData(.task, .subset, target.extra = TRUE)
    m = randomForest::randomForest(x = data[["data"]], y = data[["target"]], ...)
    ## ugly hack
    ## if the formula interface is used the training data is saved, but not with the default interface
    ## the training data is used when predict.type = "se" and calibration is not FALSE
    if (.learner$predict.type == "se") {
      if (is.null(.learner$par.vals$calibrate)) {
        m$data = data
      } else {
        if (.learner$par.vals$calibrate) {
          m$data = data
        }
      }
    }
  }
  return(m)
}


#' @export
predictLearner.regr.randomForest = function(.learner, .model, .newdata, ...) {
  if (.learner$predict.type == "se") {
    se.fun = switch(.learner$par.vals$se.method,
      bootstrap = bootstrapStandardError,
      jackknife = jackknifeStandardError,
      infjackknife = infinitesimalJackknifeStandardError,
      sd = sdStandardError
    )
    se.fun(.learner, .model, .newdata, ...)
  } else {
    predict(.model$learner.model, newdata = .newdata, ...)
  }
}

# Computes brute force or noisy bootstrap
# Set ntree = ntree.for.se for the brute force bootstrap
# Set ntree.for.se << ntree for the noisy bootstrap (mc bias corrected)
bootstrapStandardError = function(.learner, .model, .newdata, ...) {
  pred.all.boot = lapply(getLearnerModel(.model$learner.model), function(x)
    predict(x$learner.model, newdata = .newdata, predict.all = TRUE)$individual)
  B = .learner$par.vals$se.boot
  R = .learner$par.vals$ntree.for.se
  M = .learner$par.vals$ntree
  bias = ((1 / R) - (1 / M)) / (B * R * (R - 1)) *
    rowSums(matrix(sapply(pred.all.boot, function(p) rowSums((p - mean(p))^2)),
                   nrow = nrow(.newdata), ncol = R, byrow = TRUE))
  pred = getPredictionResponse(predict(.model$learner.model, newdata = .newdata))
  pred.boot = lapply(getLearnerModel(.model$learner.model), predict, newdata = .newdata, ...)
  pred.boot = extractSubList(pred.boot, c("data", "response"))
  if (is.vector(pred.boot)) {
    pred.boot = matrix(pred.boot, nrow = nrow(.newdata), ncol = R, byrow = TRUE)
  }
  var.boot = apply(pred.boot, 1, var) - bias
  var.boot[var.boot <= 0] = 0
  cbind(pred, sqrt(var.boot))
}

# Computes the mc bias-corrected jackknife after bootstrap
jackknifeStandardError = function(.learner, .model, .newdata, ...) {
  model = .model$learner.model
  n = nrow(model$inbag)
  ntree = model$ntree
  pred = predict(model, newdata = .newdata, predict.all = TRUE)
  oob = t(sapply(seq_len(n), function(i) model$inbag[i, ] == 0))
  jack_n = apply(oob, 1, function(x) rowMeans(pred$individual[, x, drop = FALSE]))
  if (is.vector(jack_n)) {
    jack_n = t(as.matrix(jack_n))
  }
  jack = (n - 1) / n * rowSums((jack_n - pred$aggregate)^2)
  bias = (exp(1) - 1) * n / ntree^2 * rowSums((pred$individual - pred$aggregate)^2)
  jab = jack - bias
  jab[jab < 0] = 0
  return(cbind(pred$aggregate, sqrt(jab)))
}


# computes the mc bias corrected infintesimal jackknife using randomForestCI
infinitesimalJackknifeStandardError = function(.learner, .model, .newdata, ...) {
  ## ugly, must be a better way to do this
  idx <- seq_len(nrow(.newdata))
  if (!is.null(.learner$par.vals$calibrate)) {
    if (.learner$par.vals$calibrate) {
      calibrate = TRUE
    } else {
      calibrate = FALSE
    }
  } else {
    calibrate = TRUE
  }
  if (nrow(.newdata) <= 20L & calibrate) {
    .newdata = rbind(.newdata, .model$learner.model$data)
  }
  ret = randomForestCI::randomForestInfJack(.model$learner.model, .newdata, calibrate, ...)
  ret$var.hat[ret$var.hat < 0] = 0
  return(cbind(ret$y.hat[idx], sqrt(ret$var.hat)[idx]))
}

# computes the standard deviation across trees
sdStandardError = function(.learner, .model, .newdata, ...) {
  pred = predict(.model$learner.model, newdata = .newdata, predict.all = TRUE, ...)
  se = apply(pred$individual, 1, sd)
  return(cbind(pred$aggregate, se))
}

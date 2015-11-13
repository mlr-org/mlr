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
                               values = c("bootstrap", "jackknife", "noisy.bootstrap", "infjackknife", "sd"),
                               requires = quote(se.method %in% c("jackknife", "infjackknife") &&
                                                  keep.inbag == TRUE)),
      makeIntegerLearnerParam(id = "nr.of.bootstrap.samples", default = 5L, lower = 1L),
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
      nr.of.bootstrap.samples = 5L
    ),
    properties = c("numerics", "factors", "ordered", "se"),
    name = "Random Forest",
    short.name = "rf"
  )
}

#' @export
trainLearner.regr.randomForest = function(.learner, .task, .subset, .weights = NULL, ...) {
  f = getTaskFormula(.task)
  par.vals = .learner$par.vals

  m = randomForest::randomForest(f, data = getTaskData(.task, .subset), ...)

  # we have to do some preprocessing here if we need the standard error
  if (.learner$predict.type == "se") {
    if (par.vals$se.method %in% c("bootstrap", "noisy.bootstrap")) {
      train = getTaskData(.task, .subset)

      # set some params for bootstraping
      numberOfBootstraps = par.vals[["nr.of.bootstrap.samples"]]
      bootstrapSize = nrow(train)

      # generate bootstrap samples
      samplesIdx = replicate(numberOfBootstraps, sample(seq_len(bootstrapSize), replace = TRUE))

      # determine whether we work with reduced ensemble size (noisy bootstrap) or not
      ntree = if (par.vals$se.method == "bootstrap") par.vals$ntree else par.vals$ntree.for.se

      # fit models on the bootstrap samples
      models = apply(samplesIdx, 2, function(bootstrapIdx) {
        randomForest::randomForest(f, data = train[bootstrapIdx,, drop = FALSE],...)
      })

     # save models in attrribute
      attr(m, "mlr.se.bootstrap.models") = models
    }
  }
  return(m)
}

#' @export
predictLearner.regr.randomForest = function(.learner, .model, .newdata, ...) {
  if (.learner$predict.type == "se") {
    se.fun = switch(.learner$par.vals$se.method,
      bootstrap = bootstrapStandardError,
      noisy.bootstrap = bootstrapStandardError,
      jackknife = jackknifeStandardError,
      infjackknife = infinitesimalJackknifeStandardError,
      sd = sdStandardError
    )
    se.fun(.learner, .model, .newdata, ...)
  } else {
    predict(.model$learner.model, newdata = .newdata, ...)
  }
}

# Computes the (potentially bias-corrected respcetively noisy)
# bootstrap estimator of the standard error
bootstrapStandardError = function(.learner, .model, .newdata, ...) {
    # copy learner and change response type
    par.vals = .learner$par.vals

    models = attr(.model$learner.model, "mlr.se.bootstrap.models")
    B = length(models)
    R = par.vals$ntree
    M = if (is.null(par.vals$ntree.for.se)) par.vals$ntree else par.vals$ntree.for.se

    # make predictions for newdata based on each "bootstrap model"
    preds = lapply(models, function(model) {
      # save predictions of every single ensemble member, i.e., decision tree
      predict(model, .newdata, predict.all = TRUE)
    })

    # n x B matrix of reponses of B forests
    aggr.responses = extractSubList(preds, "aggregate", simplify = "cols")
    mean.responses = rowMeans(aggr.responses)
    # list of n x M matrices of ensemble responses
    ind.responses = extractSubList(preds, "individual", simplify = FALSE, use.names = FALSE)

    # R substracts columnswise matrix - vector, 2nd is actually apply(aggr.responses, 1, var)
    res = cbind(mean.responses, rowSums((aggr.responses - mean.responses)^2) / (B - 1))

    if (par.vals$se.method == "noisy.bootstrap") {
      # Bias contributed significantly to the error of the biased bootstrap estimator
      # Thus, compute a corrected version
      # FIXME: check if this works properly
      const = ((1/R) - (1/M))/(B*R*(R-1))
      for (i in seq_row(.newdata)) {
        bias = 0
        for (b in seq_len(B)) {
          for (r in seq_len(R)) {
            bias = bias + (ind.responses[[b]][i, r] - aggr.responses[i, b])^2
          }
        }

        bias = bias * const
        res[i, 2L] = res[i, 2L] - bias
      }
    }

    # var --> sd
    res[, 2L] = sqrt(res[, 2L])
    return(res)
}

# Computes the bias-corrected jackknife after bootstrap
jackknifeStandardError = function(.learner, .model, .newdata, ...) {
  model = .model$learner.model
  n = nrow(model$inbag)
  ntree = model$ntree
  pred = predict(model, .newdata, predict.all = TRUE)
  oob = t(sapply(seq_len(n), function(i) model$inbag[i, ] == 0))
  jack_n = apply(oob, 1, function(x) rowMeans(pred$individual[, x]))
  jack = (n - 1) / n * rowSums((jack_n - pred$aggregate)^2)
  bias = (exp(1) - 1) * n / ntree^2 * rowSums((pred$individual - pred$aggregate)^2)
  jab = jack - bias
  jab[jab < 0] = 0
  return(cbind(pred$aggregate, sqrt(jab)))
}


# computes the bias corrected infintesimal jackknife using randomForestCI
infinitesimalJackknifeStandardError = function(.learner, .model, .newdata, ...) {
  if (is.null(.learner$par.vals$calibrate) & nrow(.newdata) > 20L)
    calibrate = TRUE
  else
    calibrate = FALSE
  ret = randomForestCI::randomForestInfJack(.model$learner.model, .newdata, calibrate)
  ret$var.hat[ret$var.hat < 0] = 0
  ij.se = sqrt(ret$var.hat)
  return(cbind(ret$y.hat, ij.se))
}

# computes the standard deviation across trees
sdStandardError = function(.learner, .model, .newdata, ...) {
  pred = predict(.model$learner.model, newdata = .newdata, predict.all = TRUE)
  se = apply(pred$individual, 1, sd)
  return(cbind(pred$aggregate, se))
}

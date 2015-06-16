# FIXME: document BS options

#' @export
makeRLearner.regr.randomForest = function() {
  makeRLearnerRegr(
    cl = "regr.randomForest",
    package = "randomForest",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "ntree", default = 500L, lower = 1L),
      makeIntegerLearnerParam(id = "ntree.for.se", default = 100L, lower = 1L),
      makeDiscreteLearnerParam(id = "se.method", default = "bootstrap", values = c("bootstrap", "jackknife", "noisy.bootstrap")),
      makeIntegerLearnerParam(id = "nr.of.bootstrap.samples", default = 5L, lower = 1L),
      makeIntegerLearnerParam(id = "mtry", lower = 1L),
      makeLogicalLearnerParam(id = "replace", default = TRUE),
      makeIntegerLearnerParam(id = "sampsize", lower = 1L),
      makeIntegerLearnerParam(id = "nodesize", default = 1L, lower = 1L),
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
      jackknife = jackknifeStandardError
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
    M = if(is.null(par.vals$ntree.for.se)) par.vals$ntree else par.vals$ntree.for.se

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
    res = cbind(mean.responses, rowSums((aggr.responses - mean.responses)^2) / (B-1))

    if (par.vals$se.method == "noisy.bootstrap") {
      # Bias contributed significantly to the error of the biased bootstrap estimator
      # Thus, compute a corrected version
      # FIXME: check if this works properly
      const = ((1/R) - (1/M))/(B*R*(R-1))
      for (i in seq_row(.newdata)) {
        bias = 0
        for (b in seq_len(B)) {
          for (r in seq_len(R)) {
            bias = bias + (ind.responses[[b]][i, r] - aggr.responses[i,b])^2
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

# Computes the (potentially bias-corrected respcetively noisy)
# jackknife estimator of the standard error
jackknifeStandardError = function(.learner, .model, .newdata, ...) {
    # extract relevant data from
    model = .model$learner.model

    # inbag needed to determine which observation was included in the training
    # of each ensemble member
    inbag = model$inbag
    n = nrow(inbag)

    # keep predictions of all ensemble members
    rf.preds = predict(model, .newdata, predict.all = TRUE)

    # determine number of participating ensembles
    # FIXME: formula looks strange. proof read ALL code in this file!
    M = lapply(seq_len(n), function(i) sum(abs(inbag[i, ] - 1)))
    # determine ensemlbe members, where observation i is not included
    idx = lapply(seq_len(n), function(i) which(inbag[i, ] == 0L))

    # estimate
    res = matrix(NA_real_, ncol = n, nrow = nrow(.newdata))
    for (j in seq_row(.newdata)) {
      for (i in seq_len(n)) {
        res[j, i] = sum(rf.preds$individual[j, idx[[i]]]) / M[[i]]
      }
    }

    mean.responses = rf.preds$aggregate
    se.preds = apply(res, 1L, function(row) {
      sum((row - mean(row))^2)  / n
    })
    return(cbind(mean.responses, se.preds))
}

#' @title Performance measures.
#' @name measures
#'
#' @description
#' A performance measure is evaluated after a single train/predict step and returns a single number to assess the quality
#' of the prediction (or maybe only the model, think AIC).
#' The measure itself knows whether it wants to be minimized or maximized and for what tasks it is applicable.
#'
#' All supported measures can be found by \code{\link{listMeasures}} or as a table
#' in the tutorial appendix: \url{http://mlr-org.github.io/mlr-tutorial/release/html/measures/}.
#'
#' If you want a measure for a misclassification cost matrix, look at \code{\link{makeCostMeasure}}.
#' If you want to implement your own measure, look at \code{\link{makeMeasure}}.
#'
#' Most measures can directly be accessed via the function named after the scheme measureX (e.g. measureSSE).
#'
#' For clustering measures, we compact the predicted cluster IDs such that they form a continuous series
#' starting with 1. If this is not the case, some of the measures will generate warnings.
#'
#' @param truth [\code{factor}]\cr
#'   Vector of the true class.
#' @param response [\code{factor}]\cr
#'   Vector of the predicted class.
#' @param negative [\code{character(1)}]\cr
#'   The name of the negative class.
#' @param positive [\code{character(1)}]\cr
#'   The name of the positive class.
#' @param probabilities [\code{numeric} | \code{matrix}]\cr
#'   a) For purely binary classification measures: The predicted probabilities for the positive class as a numeric vector.
#'   b) For multiclass classification measures: The predicted probabilities for all classes, always as a numeric matrix, where
#'   columns are named with class labels.
#' @name measures
#' @rdname measures
#' @family performance
NULL

###############################################################################
### general ###
###############################################################################
#' @export
#' @rdname measures
#' @format none
featperc = makeMeasure(id = "featperc", minimize = TRUE, best = 0, worst = 1,
  properties = c("classif", "classif.multi", "multilabel", "regr", "surv", "costsens", "cluster", "req.model", "req.pred"),
  name = "Percentage of original features used for model",
  note =  "Useful for feature selection.",
  fun = function(task, model, pred, feats, extra.args) {
    length(model$features) / sum(pred$task.desc$n.feat)
  }
)

#' @export timetrain
#' @rdname measures
#' @format none
timetrain = makeMeasure(id = "timetrain", minimize = TRUE, best = 0, worst = Inf,
  properties = c("classif", "classif.multi", "multilabel", "regr", "surv", "costsens", "cluster", "req.model"),
  name = "Time of fitting the model",
  fun = function(task, model, pred, feats, extra.args) {
    model$time
  }
)

#' @export timepredict
#' @rdname measures
#' @format none
timepredict = makeMeasure(id = "timepredict", minimize = TRUE, best = 0, worst = Inf,
  properties = c("classif", "classif.multi", "multilabel", "regr", "surv", "costsens", "cluster", "req.pred"),
  name = "Time of predicting test set",
  fun = function(task, model, pred, feats, extra.args) {
    pred$time
  }
)

#' @export timeboth
#' @rdname measures
#' @format none
timeboth = makeMeasure(id = "timeboth", minimize = TRUE, best = 0, worst = Inf,
  properties = c("classif", "classif.multi", "multilabel", "regr", "surv", "costsens", "cluster", "req.model", "req.pred"),
  name = "timetrain + timepredict",
  fun = function(task, model, pred, feats, extra.args) {
    model$time + pred$time
  }
)

###############################################################################
### regression ###
###############################################################################

#' @export sse
#' @rdname measures
#' @format none
sse = makeMeasure(id = "sse", minimize = TRUE, best = 0, worst = Inf,
  properties = c("regr", "req.pred", "req.truth"),
  name = "Sum of squared errors",
  fun = function(task, model, pred, feats, extra.args) {
    measureSSE(pred$data$truth, pred$data$response)
  }
)

#' @export measureSSE
#' @rdname measures
#' @format none
measureSSE = function(truth, response) {
  sum((response - truth)^2)
}

#' @export mse
#' @rdname measures
#' @format none
mse = makeMeasure(id = "mse", minimize = TRUE, best = 0, worst = Inf,
  properties = c("regr", "req.pred", "req.truth"),
  name = "Mean of squared errors",
  fun = function(task, model, pred, feats, extra.args) {
    measureMSE(pred$data$truth, pred$data$response)
  }
)

#' @export measureMSE
#' @rdname measures
#' @format none
measureMSE = function(truth, response) {
  mean((response - truth)^2)
}

#' @export rmse
#' @format none
#' @rdname measures
#' @format none
rmse = makeMeasure(id = "rmse", minimize = TRUE, best = 0, worst = Inf,
  properties = c("regr", "req.pred", "req.truth"),
  name = "Root mean square error",
  note = "The RMSE is aggregated as sqrt(mean(rmse.vals.on.test.sets^2)). If you don't want that, you could also use `test.mean`.",
  fun = function(task, model, pred, feats, extra.args) {
    measureRMSE(pred$data$truth, pred$data$response)
  },
  aggr = test.rmse
)

#' @export measureRMSE
#' @rdname measures
#' @format none
measureRMSE = function(truth, response) {
  sqrt(measureMSE(truth, response))
}

#' @export medse
#' @rdname measures
#' @format none
medse = makeMeasure(id = "medse", minimize = TRUE, best = 0, worst = Inf,
  properties = c("regr", "req.pred", "req.truth"),
  name = "Median of squared errors",
  fun = function(task, model, pred, feats, extra.args) {
    measureMEDSE(pred$data$truth, pred$data$response)
  }
)

#' @export measureMEDSE
#' @rdname measures
#' @format none
measureMEDSE = function(truth, response) {
  median((response - truth)^2)
}

#' @export sae
#' @rdname measures
#' @format none
sae = makeMeasure(id = "sae", minimize = TRUE, best = 0, worst = Inf,
  properties = c("regr", "req.pred", "req.truth"),
  name = "Sum of absolute errors",
  fun = function(task, model, pred, feats, extra.args) {
    measureSAE(pred$data$truth, pred$data$response)
  }
)

#' @export measureSAE
#' @rdname measures
#' @format none
measureSAE = function(truth, response) {
  sum(abs(response - truth))
}

#' @export mae
#' @rdname measures
#' @format none
mae = makeMeasure(id = "mae", minimize = TRUE, best = 0, worst = Inf,
  properties = c("regr", "req.pred", "req.truth"),
  name = "Mean of absolute errors",
  fun = function(task, model, pred, feats, extra.args) {
    measureMAE(pred$data$truth, pred$data$response)
  }
)

#' @export measureMAE
#' @rdname measures
#' @format none
measureMAE = function(truth, response) {
  mean(abs(response - truth))
}

#' @export medae
#' @rdname measures
#' @format none
medae = makeMeasure(id = "medae", minimize = TRUE, best = 0, worst = Inf,
  properties = c("regr", "req.pred", "req.truth"),
  name = "Median of absolute errors",
  fun = function(task, model, pred, feats, extra.args) {
    measureMEDAE(pred$data$truth, pred$data$response)
  }
)

#' @export measureMEDAE
#' @rdname measures
#' @format none
measureMEDAE = function(truth, response) {
  median(abs(response - truth))
}

#' @export rsq
#' @rdname measures
#' @format none
rsq = makeMeasure(id = "rsq", minimize = FALSE, best = 1, worst = -Inf,
  properties = c("regr", "req.pred", "req.truth"),
  name = "Coefficient of determination",
  note = "Also called R-squared, which is 1 - residual_sum_of_squares / total_sum_of_squares.",
  fun = function(task, model, pred, feats, extra.args) {
    measureRSQ(pred$data$truth, pred$data$response)
  }
)

#' @export measureRSQ
#' @rdname measures
#' @format none
measureRSQ = function(truth, response) {
  rss = measureSSE(truth, response)
  ess = sum((truth - mean(truth))^2L)
  1 - rss / ess
}

#' @export expvar
#' @rdname measures
#' @format none
expvar = makeMeasure(id = "expvar", minimize = FALSE, best = 1, worst = 0,
  properties = c("regr", "req.pred", "req.truth"),
  name = "Explained variance",
  note = "Similar to measaure rsq (R-squared). Defined as explained_sum_of_squares / total_sum_of_squares.",
  fun = function(task, model, pred, feats, extra.args) {
    measureEXPVAR(pred$data$truth, pred$data$response)
  }
)

#' @export measureEXPVAR
#' @rdname measures
#' @format none
measureEXPVAR = function(truth, response) {
  regss = sum((response - mean(truth))^2L)
  ess = sum((truth - mean(truth))^2L)
  regss / ess
}

#' @export arsq
#' @rdname measures
#' @format none
arsq = makeMeasure(id = "adjrsq", minimize = FALSE, best = 1, worst = 0,
  properties = c("regr", "req.pred", "req.truth"),
  name = "Adjusted coefficient of determination",
  note = "Adjusted R-squared is only defined for normal linear regression",
  fun = function(task, model, pred, feats, extra.args) {
    n = length(pred$data$truth)
    p = length(model$features)
    1 - (1 - measureRSQ(pred$data$truth, pred$data$response)) * (p / (n - p - 1L))
  }
)

###############################################################################
### classif multi ###
###############################################################################
#' @export mmce
#' @rdname measures
#' @format none
mmce = makeMeasure(id = "mmce", minimize = TRUE, best = 0, worst = 1,
  properties = c("classif", "classif.multi", "req.pred", "req.truth"),
  name = "Mean misclassification error",
  fun = function(task, model, pred, feats, extra.args) {
    measureMMCE(pred$data$truth, pred$data$response)
  }
)

#' @export measureMMCE
#' @rdname measures
#' @format none
measureMMCE = function(truth, response) {
  mean(response != truth)
}

#' @export acc
#' @rdname measures
#' @format none
acc = makeMeasure(id = "acc", minimize = FALSE, best = 1, worst = 0,
  properties = c("classif", "classif.multi", "req.pred", "req.truth"),
  name = "Accuracy",
  fun = function(task, model, pred, feats, extra.args) {
    measureACC(pred$data$truth, pred$data$response)
  }
)

#' @export measureACC
#' @rdname measures
#' @format none
measureACC = function(truth, response) {
  mean(response == truth)
}

#' @export ber
#' @rdname measures
#' @format none
ber = makeMeasure(id = "ber", minimize = TRUE, best = 0, worst = 1,
  properties = c("classif", "classif.multi", "req.pred", "req.truth"),
  name = "Balanced error rate",
  note = "Mean of misclassification error rates on all individual classes.",
  fun = function(task, model, pred, feats, extra.args) {
    # special case for predictions from FailureModel
    if (anyMissing(pred$data$response))
      return(NA_real_)
    n = length(pred$task.desc$class.levels) + 1L
    mean(getConfMatrix(pred, relative = TRUE)[-n, n])
  }
)

#' @export multiclass.aunu
#' @rdname measures
#' @format none
multiclass.aunu = makeMeasure(id = "multiclass.aunu", minimize = FALSE, best = 1, worst = 0.5,
  properties = c("classif", "classif.multi", "req.pred", "req.truth", "req.prob"),
  name = "Average multiclass AUC",
  note = "Computes the AUC treating a c-dimensional classifier as c two-dimensional classifiers, where classes are assumed to have uniform distribution, in order to have a measure which is independent of class distribution change. See Ferri et. al: https://www.math.ucdavis.edu/~saito/data/roc/ferri-class-perf-metrics.pdf",
  fun = function(task, model, pred, feats, extra.args) {
    measureAUNU(getPredictionProbabilities(pred, pred$task.desc$class.levels), pred$data$truth)
  }
)

#' @export measureAUNU
#' @rdname measures
#' @format none
measureAUNU = function(probabilities, truth) {
  mean(vnapply(1:nlevels(truth), function(i) colAUC(probabilities[, i], truth == levels(truth)[i])))
}

#' @export multiclass.aunp
#' @rdname measures
#' @format none
multiclass.aunp = makeMeasure(id = "multiclass.aunp", minimize = FALSE, best = 1, worst = 0.5,
  properties = c("classif", "classif.multi", "req.pred", "req.truth", "req.prob"),
  name = "Weighted average multiclass AUC",
  note = "Computes the AUC treating a c-dimensional classifier as c two-dimensional classifiers, taking into account the prior probability of each class (p(j)). See Ferri et. al: https://www.math.ucdavis.edu/~saito/data/roc/ferri-class-perf-metrics.pdf",
  fun = function(task, model, pred, feats, extra.args) {
    measureAUNP(getPredictionProbabilities(pred, pred$task.desc$class.levels), pred$data$truth)
  }
)

#' @export measureAUNP
#' @rdname measures
#' @format none
measureAUNP = function(probabilities, truth) {
  sum(vnapply(1:nlevels(truth), function(i) mean(truth == levels(truth)[i]) * colAUC(probabilities[,i], truth == levels(truth)[i])))  
}

#' @export multiclass.au1u
#' @rdname measures
#' @format none
multiclass.au1u = makeMeasure(id = "multiclass.au1u", minimize = FALSE, best = 1, worst = 0.5,
  properties = c("classif", "classif.multi", "req.pred", "req.truth", "req.prob"),
  name = "Average 1 vs. 1 multiclass AUC",
  note =  "Computes AUC of c(c - 1) binary classifiers (all possible pairwise combinations) while considering uniform distribution of the classes. See Ferri et. al: https://www.math.ucdavis.edu/~saito/data/roc/ferri-class-perf-metrics.pdf",
  fun = function(task, model, pred, feats, extra.args) {
    measureAU1U(getPredictionProbabilities(pred, pred$task.desc$class.levels), pred$data$truth)
  }
)

#' @export measureAU1U
#' @rdname measures
#' @format none
measureAU1U = function(probabilities, truth) {
  m = colAUC(probabilities, truth)
  c = c(combn(1:nlevels(truth), 2))
  mean(m[cbind(rep(1:nrow(m), each = 2), c)])
}

#' @export multiclass.au1p
#' @rdname measures
#' @format none
multiclass.au1p = makeMeasure(id = "multiclass.au1p", minimize = FALSE, best = 1, worst = 0.5,
  properties = c("classif", "classif.multi", "req.pred", "req.truth", "req.prob"),
  name = "Weighted average 1 vs. 1 multiclass AUC",
  note = "Computes AUC of c(c - 1) binary classifiers while considering the a priori distribution of the classes. See Ferri et. al paper: https://www.math.ucdavis.edu/~saito/data/roc/ferri-class-perf-metrics.pdf",                             
  fun = function(task, model, pred, feats, extra.args) {
    measureAU1P(getPredictionProbabilities(pred, pred$task.desc$class.levels), pred$data$truth)
  }
)

#' @export measureAU1P
#' @rdname measures
#' @format none
measureAU1P = function(probabilities, truth) {
  m = colAUC(probabilities, truth)
  weights = table(truth) / length(truth)
  m = m * matrix(rep(weights, each = nrow(m)), ncol = length(weights))
  c = c(combn(1:nlevels(truth), 2))
  sum(m[cbind(rep(1:nrow(m), each = 2), c)]) / (nlevels(truth) - 1)
}

#' @export multiclass.brier
#' @rdname measures
#' @format none
multiclass.brier = makeMeasure(id = "multiclass.brier", minimize = TRUE, best = 0, worst = 2,
  properties = c("classif", "classif.multi", "req.pred", "req.truth", "req.prob"),
  name = "Multiclass Brier score",
  note = "Defined as: (1/n) sum_i sum_j (y_ij - p_ij)^2, where y_ij = 1 if observation i has class j (else 0), and p_ij is the predicted probablity of observation i for class j. From http://docs.lib.noaa.gov/rescue/mwr/078/mwr-078-01-0001.pdf",
  fun = function(task, model, pred, feats, extra.args) {
    measureMulticlassBrier(getPredictionProbabilities(pred, pred$task.desc$class.levels), pred$data$truth)
  }
)

#' @export measureMulticlassBrier
#' @rdname measures
#' @format none
measureMulticlassBrier = function(probabilities, truth) {
  truth = factor(truth, levels = colnames(probabilities))
  mat01 = model.matrix( ~ . -1, data = as.data.frame(truth))
  mean(rowSums((probabilities - mat01)^2))
}

#' @export logloss
#' @rdname measures
#' @format none
logloss = makeMeasure(id = "logloss", minimize = TRUE, best = 0, worst = Inf,
  properties = c("classif", "classif.multi", "req.truth", "req.prob"),
  name = "Logarithmic loss",
  note = "Defined as: -mean(log(p_i)), where p_i is the predicted probability of the true class of observation i. Inspired by https://www.kaggle.com/wiki/MultiClassLogLoss.",
  fun = function(task, model, pred, feats, extra.args) {
    measureLogloss(getPredictionProbabilities(pred, cl = pred$task.desc$class.levels), pred$data$truth)
  }
)

#' @export measureLogloss
#' @rdname measures
#' @format none
measureLogloss = function(probabilities, truth){
  eps = 1e-15
  #let's confine the predicted probabilities to [eps,1-eps], so logLoss doesn't reach infinity under any circumstance
  probabilities[probabilities > 1-eps] = 1-eps
  probabilities[probabilities < eps] = eps
  truth = match(as.character(truth), colnames(probabilities))
  p = getRowEls(probabilities, truth)
  -1*mean(log(p))
}

###############################################################################
### classif binary ###
###############################################################################
#' @export auc
#' @rdname measures
#' @format none
auc = makeMeasure(id = "auc", minimize = FALSE, best = 1, worst = 0,
  properties = c("classif", "req.pred", "req.truth", "req.prob"),
  name = "Area under the curve",
  fun = function(task, model, pred, feats, extra.args) {
    # ROCR does not work with NAs
    if (anyMissing(pred$data$response) || length(unique(pred$data$truth)) == 1L)
      return(NA_real_)
    measureAUC(getPredictionProbabilities(pred), pred$data$truth, pred$task.desc$negative, pred$task.desc$positive)
  }
)

#' @export measureAUC
#' @rdname measures
#' @format none
measureAUC = function(probabilities, truth, negative, positive) {
  rpreds = asROCRPredictionIntern(probabilities, truth, negative, positive)
  ROCR::performance(rpreds, "auc")@y.values[[1L]]
}

#' @export brier
#' @rdname measures
#' @format none
brier = makeMeasure(id = "brier", minimize = TRUE, best = 0, worst = 1,
  properties = c("classif", "req.pred", "req.truth", "req.prob"),
  name = "Brier score",
  fun = function(task, model, pred, feats, extra.args) {
    measureBrier(getPredictionProbabilities(pred), pred$data$truth, pred$task.desc$negative, pred$task.desc$positive)
  }
)

#' @export measureBrier
#' @rdname measures
#' @format none
measureBrier = function(probabilities, truth, negative, positive) {
  y = as.numeric(truth == positive)
  mean((y - probabilities)^2)
}

#' @export brier.scaled
#' @rdname measures
#' @format none
brier.scaled = makeMeasure(id = "brier.scaled", minimize = FALSE, best = 1, worst = 0,
  properties = c("classif", "req.pred", "req.truth", "req.prob"),
  name = "Brier scaled",
  note = "Brier score scaled to [0,1], see http://www.ncbi.nlm.nih.gov/pmc/articles/PMC3575184/.",
  fun = function(task, model, pred, feats, extra.args) {
    measureBrierScaled(getPredictionProbabilities(pred), pred$data$truth, pred$task.desc$negative, pred$task.desc$positive)
  }
)

#' @export measureBrierScaled
#' @rdname measures
#' @format none
measureBrierScaled = function(probabilities, truth, negative, positive) {
  y = as.numeric(truth == positive)
  brier = mean((y - probabilities)^2)
  inc = mean(probabilities)
  brier.max = inc * (1 - inc)^2 + (1 - inc) * inc^2
  1 - brier / brier.max
}

#' @export bac
#' @rdname measures
#' @format none
bac = makeMeasure(id = "bac", minimize = FALSE, best = 1, worst = 0,
  properties = c("classif", "req.pred", "req.truth"),
  name = "Balanced accuracy",
  note = "Mean of true positive rate and true negative rate.",
  fun = function(task, model, pred, feats, extra.args) {
    mean(c(tp$fun(pred = pred) / sum(pred$data$truth == pred$task.desc$positive),
      tn$fun(pred = pred) / sum(pred$data$truth == pred$task.desc$negative)))
  }
)

#' @export measureBAC
#' @rdname measures
#' @format none
measureBAC = function(truth, response, negative, positive) {
  mean(c(
    measureTP(truth, response, positive) / sum(truth == positive),
    measureTN(truth, response, negative) / sum(truth == negative)
  ))
}

#' @export tp
#' @rdname measures
#' @format none
tp = makeMeasure(id = "tp", minimize = FALSE, best = Inf, worst = 0,
  properties = c("classif", "req.pred", "req.truth"),
  name = "True positives",
  fun = function(task, model, pred, feats, extra.args) {
    measureTP(pred$data$truth, pred$data$response, pred$task.desc$positive)
  }
)

#' @export measureTP
#' @rdname measures
#' @format none
measureTP = function(truth, response, positive) {
  sum(truth == response & response == positive)
}

#' @export tn
#' @rdname measures
#' @format none
tn = makeMeasure(id = "tn", minimize = FALSE, best = Inf, worst = 0,
  properties = c("classif", "req.pred", "req.truth"),
  name = "True negatives",
  note = "Also called correct rejections.",
  fun = function(task, model, pred, feats, extra.args) {
    measureTN(pred$data$truth, pred$data$response, pred$task.desc$negative)
  }
)

#' @export measureTN
#' @rdname measures
#' @format none
measureTN = function(truth, response, negative) {
  sum(truth == response & response == negative)
}

#' @export fp
#' @rdname measures
#' @format none
fp = makeMeasure(id = "fp", minimize = TRUE, best = 0, worst = Inf,
  properties = c("classif", "req.pred", "req.truth"),
  name = "False positives",
  note = "Also called false alarms.",
  fun = function(task, model, pred, feats, extra.args) {
    measureFP(pred$data$truth, pred$data$response, pred$task.desc$positive)
  }
)

#' @export measureFP
#' @rdname measures
#' @format none
measureFP = function(truth, response, positive) {
  sum(truth != response & response == positive)
}

#' @export fn
#' @rdname measures
#' @format none
fn = makeMeasure(id = "fn", minimize = TRUE, best = 0, worst = Inf,
  properties = c("classif", "req.pred", "req.truth"),
  name = "False negatives",
  note = "Also called misses.",
  fun = function(task, model, pred, feats, extra.args) {
    measureFN(pred$data$truth, pred$data$response, pred$task.desc$negative)
  }
)

#' @export measureFN
#' @rdname measures
#' @format none
measureFN = function(truth, response, negative) {
  sum(truth != response & response == negative)
}

#' @export tpr
#' @rdname measures
#' @format none
tpr = makeMeasure(id = "tpr", minimize = FALSE, best = 1, worst = 0,
  properties = c("classif", "req.pred", "req.truth"),
  name = "True positive rate",
  note = "Also called hit rate or recall.",
  fun = function(task, model, pred, feats, extra.args) {
    measureTPR(pred$data$truth, pred$data$response, pred$task.desc$positive)
  }
)

#' @export measureTPR
#' @rdname measures
#' @format none
measureTPR = function(truth, response, positive) {
  measureTP(truth, response, positive) / sum(truth == positive)
}

#' @export tnr
#' @rdname measures
#' @format none
tnr = makeMeasure(id = "tnr", minimize = FALSE, best = 1, worst = 0,
  properties = c("classif", "req.pred", "req.truth"),
  name = "True negative rate",
  note = "Also called specificity.",
  fun = function(task, model, pred, feats, extra.args) {
    measureTNR(pred$data$truth, pred$data$response, pred$task.desc$negative)
  }
)

#' @export measureTNR
#' @rdname measures
#' @format none
measureTNR = function(truth, response, negative) {
  measureTN(truth, response, negative) / sum(truth == negative)
}

#' @export fpr
#' @rdname measures
#' @format none
fpr = makeMeasure(id = "fpr", minimize = TRUE, best = 0, worst = 1,
  properties = c("classif" , "req.pred", "req.truth"),
  name = "False positive rate",
  note = "Also called false alarm rate or fall-out.",
  fun = function(task, model, pred, feats, extra.args) {
    measureFPR(pred$data$truth, pred$data$response, pred$task.desc$negative, pred$task.desc$positive)
  }
)

#' @export measureFPR
#' @rdname measures
#' @format none
measureFPR = function(truth, response, negative, positive) {
  measureFP(truth, response, positive) / sum(truth == negative)
}

#' @export fnr
#' @rdname measures
#' @format none
fnr = makeMeasure(id = "fnr", minimize = TRUE, best = 0, worst = 1,
  properties = c("classif", "req.pred", "req.truth"),
  name = "False negative rate",
  fun = function(task, model, pred, feats, extra.args) {
    measureFNR(pred$data$truth, pred$data$response, pred$task.desc$negative, pred$task.desc$positive)
  }
)

#' @export measureFNR
#' @rdname measures
#' @format none
measureFNR = function(truth, response, negative, positive) {
  measureFN(truth, response, negative) / sum(truth == positive)
}

#' @export ppv
#' @rdname measures
#' @format none
ppv = makeMeasure(id = "ppv", minimize = FALSE, best = 1, worst = 0,
  properties = c("classif", "req.pred", "req.truth"),
  name = "Positive predictive value",
  note = "Also called precision.",
  fun = function(task, model, pred, feats, extra.args) {
    measurePPV(pred$data$truth, pred$data$response, pred$task.desc$positive)
  }
)

#' @export measurePPV
#' @rdname measures
#' @format none
measurePPV = function(truth, response, positive) {
  measureTP(truth, response, positive) / sum(response == positive)
}

#' @export npv
#' @rdname measures
#' @format none
npv = makeMeasure(id = "npv", minimize = FALSE, best = 1, worst = 0,
  properties = c("classif", "req.pred", "req.truth"),
  name = "Negative predictive value",
  fun = function(task, model, pred, feats, extra.args) {
    measureNPV(pred$data$truth, pred$data$response, pred$task.desc$negative)
  }
)

#' @export measureNPV
#' @rdname measures
#' @format none
measureNPV = function(truth, response, negative) {
  measureTN(truth, response, negative) / sum(response == negative)
}

#' @export fdr
#' @rdname measures
#' @format none
fdr = makeMeasure(id = "fdr", minimize = TRUE, best = 0, worst = 1,
  properties = c("classif", "req.pred", "req.truth"),
  name = "False discovery rate",
  fun = function(task, model, pred, feats, extra.args) {
    measureFDR(pred$data$truth, pred$data$response, pred$task.desc$positive)
  }
)

#' @export measureFDR
#' @rdname measures
#' @format none
measureFDR = function(truth, response, positive) {
  measureFP(truth, response, positive) / sum(response == positive)
}

#' @export mcc
#' @rdname measures
#' @format none
mcc = makeMeasure(id = "mcc", minimize = FALSE,
  properties = c("classif", "req.pred", "req.truth"), best = 1, worst = -1,
  name = "Matthews correlation coefficient",
  fun = function(task, model, pred, feats, extra.args) {
    measureMCC(pred$data$truth, pred$data$response, pred$task.desc$negative, pred$task.desc$positive)
  }
)

#' @export measureMCC
#' @rdname measures
#' @format none
measureMCC = function(truth, response, negative, positive) {
  tn = measureTN(truth, response, negative)
  tp = measureTP(truth, response, positive)
  fn = measureFN(truth, response, negative)
  fp = measureFP(truth, response, positive)
  (tp * tn - fp * fn) /
    sqrt((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn))
}

#' @export f1
#' @rdname measures
#' @format none
f1 = makeMeasure(id = "f1", minimize = FALSE, best = 1, worst = 0,
  properties = c("classif", "req.pred", "req.truth"),
  name = "F1 measure",
  fun = function(task, model, pred, feats, extra.args) {
    measureF1(pred$data$truth, pred$data$response, pred$task.desc$positive)
  }
)

measureF1 = function(truth, response, positive) {
  2 * measureTP(truth, response, positive) /
    (sum(truth == positive) + sum(response == positive))
}

#' @export gmean
#' @rdname measures
#' @format none
gmean = makeMeasure(id = "gmean", minimize = FALSE, best = 1, worst = 0,
  properties = c("classif", "req.pred", "req.truth"),
  name = "G-mean",
  note = "Geometric mean of recall and specificity.",
  fun = function(task, model, pred, feats, extra.args) {
    measureGMEAN(pred$data$truth, pred$data$response, pred$task.desc$negative, pred$task.desc$positive)
  }
)

#' @export measureGMEAN
#' @rdname measures
#' @format none
#' @references
#' He, H. & Garcia, E. A. (2009)
#' \emph{Learning from Imbalanced Data.}
#' IEEE Transactions on Knowledge and Data Engineering, vol. 21, no. 9. pp. 1263-1284.
measureGMEAN = function(truth, response, negative, positive) {
  sqrt(measureTPR(truth, response, positive) * measureTNR(truth, response, negative))
}

#' @export gpr
#' @rdname measures
#' @format none
gpr = makeMeasure(id = "gpr", minimize = FALSE, best = 1, worst = 0,
  properties = c("classif", "req.pred", "req.truth"),
  name = "Geometric mean of precision and recall",
  fun = function(task, model, pred, feats, extra.args) {
    measureGPR(pred$data$truth, pred$data$response, pred$task.desc$positive)
  }
)

#' @export measureGPR
#' @rdname measures
#' @format none
measureGPR = function(truth, response, positive) {
  sqrt(measurePPV(truth, response, positive) * measureTPR(truth, response, positive))
}

###############################################################################
### multilabel ###
###############################################################################
#' @export multilabel.hamloss
#' @rdname measures
#' @format none
multilabel.hamloss = makeMeasure(id = "multilabel.hamloss", minimize = TRUE, best = 0, worst = 1,
  properties = c("multilabel", "req.pred", "req.truth"),
  name = "Hamming loss",
  note = "Proportion of labels whose relevance is incorrectly predicted,
  following the definition by Charte and Charte: https://journal.r-project.org/archive/2015-2/charte-charte.pdf",
  fun = function(task, model, pred, feats, extra.args) {
    measureMultilabelHamloss(getPredictionTruth.PredictionMultilabel(pred),
      getPredictionResponse.PredictionMultilabel(pred))
})

#' @export measureMultilabelHamloss
#' @rdname measures
#' @format none
measureMultilabelHamloss = function(truth, response) {
  mean(truth != response)
}

#' @export multilabel.subset01
#' @rdname measures
#' @format none
multilabel.subset01 = makeMeasure(id = "multilabel.subset01", minimize = TRUE, best = 0, worst = 1,
  properties = c("multilabel", "req.pred", "req.truth"),
  name = "Subset-0-1 loss",
  note = "Proportion of observations where the complete multilabel set (all 0-1-lables) is not correctly predicted,
  following the definition by Charte and Charte: https://journal.r-project.org/archive/2015-2/charte-charte.pdf",
  fun = function(task, model, pred, feats, extra.args) {
    measureMultilabelSubset01(getPredictionTruth.PredictionMultilabel(pred),
    getPredictionResponse.PredictionMultilabel(pred))
  }
)

#' @export measureMultilabelSubset01
#' @rdname measures
#' @format none
measureMultilabelSubset01 = function(truth, response) {
  mean(!apply(truth == response, 1, all))
}

#' @export multilabel.f1
#' @rdname measures
#' @format none
multilabel.f1 = makeMeasure(id = "multilabel.f1", minimize = FALSE, best = 1, worst = 0,
  properties = c("multilabel", "req.pred", "req.truth"),
  name = "F1 measure (multilabel)",
  note = "Harmonic mean of precision and recall on a per instance basis (Micro-F1),
  following the definition by Montanes et al.: http://www.sciencedirect.com/science/article/pii/S0031320313004019",
  fun = function(task, model, pred, feats, extra.args) {
    measureMultiLabelF1(getPredictionTruth.PredictionMultilabel(pred),
    getPredictionResponse.PredictionMultilabel(pred))
  }
)

#' @export measureMultiLabelF1
#' @rdname measures
#' @format none
measureMultiLabelF1 = function(truth, response) {
  fi = numeric(nrow(truth))
  for (i in seq_row(truth)) {
    if (sum(truth[i, ]) + sum(response[i, ]) == 0) {
      fi[i] = 1
    } else {
      fi[i] = 2 * sum(truth[i, ] * response[i, ]) / (sum(truth[i, ]) + sum(response[i, ]))
    }
  }
  mean(fi)
}

#' @export multilabel.acc
#' @rdname measures
#' @format none
multilabel.acc = makeMeasure(id = "multilabel.acc", minimize = FALSE, best = 1, worst = 0,
  properties = c("multilabel", "req.pred", "req.truth"),
  name = "Accuracy (multilabel)",
  note = "Mean of proportion of correctly predicted labels with respect to the total number of labels for each instance,
  following the definition by Charte and Charte: https://journal.r-project.org/archive/2015-2/charte-charte.pdf",
  fun = function(task, model, pred, feats, extra.args) {
    measureMultilabelACC(getPredictionTruth.PredictionMultilabel(pred),
    getPredictionResponse.PredictionMultilabel(pred))
  }
)

#' @export measureMultilabelACC
#' @rdname measures
#' @format none
measureMultilabelACC = function(truth, response) {
  acc = numeric(nrow(truth))
  for (i in seq_row(truth)) {
    if (sum(truth[i, ]) + sum(response[i, ]) == 0) {
      acc[i] = 1
    } else {
      acc[i] = sum(truth[i, ] * response[i, ]) / (sum(truth[i, ]) + sum(response[i, ]) - sum(truth[i, ] * response[i, ]))
    }
  }
  mean(acc)
}

#' @export multilabel.ppv
#' @rdname measures
#' @format none
multilabel.ppv = makeMeasure(id = "multilabel.ppv", minimize = FALSE, best = 1, worst = 0,
  properties = c("multilabel", "req.pred", "req.truth"),
  name = "Postive predicive value (multilabel)",
  note = "Also called precision. Mean of ratio of truly predicted labels for each instance,
  following the definition by Charte and Charte: https://journal.r-project.org/archive/2015-2/charte-charte.pdf",
  fun = function(task, model, pred, feats, extra.args) {
    measureMultilabelPPV(getPredictionTruth.PredictionMultilabel(pred),
    getPredictionResponse.PredictionMultilabel(pred))
  }
)

#' @export measureMultilabelPPV
#' @rdname measures
#' @format none
measureMultilabelPPV = function(truth, response) {
  prec = numeric(nrow(truth))
  for (i in seq_row(truth)) {
    if (sum(response[i, ]) == 0) {
      prec[i] = 1
    } else {
      prec[i] = sum(truth[i, ] * response[i, ]) / sum(response[i, ])
    }
  }
  mean(prec)
}

#' @export multilabel.tpr
#' @rdname measures
#' @format none
multilabel.tpr = makeMeasure(id = "multilabel.tpr", minimize = FALSE, best = 1, worst = 0,
  properties = c("multilabel", "req.pred", "req.truth"),
  name = "TPR (multilabel)",
  note = "Also called recall. Mean of proportion of predicted labels which are relevant for each instance,
  following the definition by Charte and Charte: https://journal.r-project.org/archive/2015-2/charte-charte.pdf",
  fun = function(task, model, pred, feats, extra.args) {
    measureMultilabelTPR(getPredictionTruth.PredictionMultilabel(pred),
    getPredictionResponse.PredictionMultilabel(pred))
  }
)

#' @export measureMultilabelTPR
#' @rdname measures
#' @format none
measureMultilabelTPR = function(truth, response) {
  rec = numeric(nrow(truth))
  for (i in seq_row(truth)) {
    if (sum(truth[i, ]) == 0) {
      rec[i] = 1
    } else {
      rec[i] = sum(truth[i, ] * response[i, ]) / sum(truth[i, ])
    }
  }
  mean(rec)
}

###############################################################################
### survival ###
###############################################################################
#' @export cindex
#' @rdname measures
#' @format none
cindex = makeMeasure(id = "cindex", minimize = FALSE, best = 1, worst = 0,
  properties = c("surv", "req.pred", "req.truth"),
  name = "Concordance index",
  fun = function(task, model, pred, feats, extra.args) {
    requirePackages("Hmisc", default.method = "load")
    resp = pred$data$response
    if (anyMissing(resp))
      return(NA_real_)
    # FIXME: we need to convert to he correct survival type
    s = Surv(pred$data$truth.time, pred$data$truth.event)
    Hmisc::rcorr.cens(-1 * resp, s)[["C Index"]]
  }
)

###############################################################################
### cost-sensitive ###
###############################################################################
#' @export meancosts
#' @rdname measures
meancosts = makeMeasure(id = "meancosts", minimize = TRUE, best = 0, worst = Inf,
  properties = c("costsens", "req.pred", "req.task"),
  name = "Mean costs of the predicted choices",
  fun = function(task, model, pred, feats, extra.args) {
    classes = as.character(pred$data$response)
    ids = pred$data$id
    costs = getTaskCosts(task)
    y = mapply(function(id, cl) {
      costs[id, cl]
    }, ids, classes, SIMPLIFY = TRUE, USE.NAMES = FALSE)
    mean(y)
  }
)

#' @export mcp
#' @rdname measures
mcp = makeMeasure(id = "mcp", minimize = TRUE, best = 0, worst = Inf,
  properties = c("costsens", "req.pred", "req.task"),
  name = "Misclassification penalty",
  note = "Average difference between costs of oracle and model prediction.",
  fun = function(task, model, pred, feats, extra.args) {
    mc = meancosts$fun(task, NULL, pred, NULL, extra.args)
    oc = mean(apply(getTaskCosts(task), 1L, min))
    mc - oc
  }
)

###############################################################################
### clustering ###
###############################################################################
#' @export db
#' @rdname measures
#' @format none
db = makeMeasure(id = "db", minimize = TRUE, best = 0, worst = Inf,
  properties = c("cluster", "req.pred", "req.feats"),
  name = "Davies-Bouldin cluster separation measure",
  note ="See `?clusterSim::index.DB`.",
  fun = function(task, model, pred, feats, extra.args) {
    if (length(unique(pred$data$response)) > 1L) {
      requirePackages("clusterSim", default.method = "load")
      r = as.integer(as.factor(pred$data$response))
      clusterSim::index.DB(feats, r)$DB
    } else {
      NA
    }
  }
)

#' @export dunn
#' @rdname measures
#' @format none
dunn = makeMeasure(id = "dunn", minimize = FALSE, best = Inf, worst = 0,
  properties = c("cluster", "req.pred", "req.feats"),
  name = "Dunn index",
  note = "See `?clValid::dunn`.",
  fun = function(task, model, pred, feats, extra.args) {
    # produced a confusing note in some cases, see issue #232
    suppressMessages(requirePackages("clValid", default.method = "load"))
    r = as.integer(as.factor(pred$data$response))
    clValid::dunn(Data = feats, clusters = r)
  }
)

#' @export G1
#' @rdname measures
#' @format none
G1 = makeMeasure(id = "G1", minimize = FALSE, best = Inf, worst = 0,
  properties = c("cluster", "req.pred", "req.feats"),
  name = "Calinski-Harabasz pseudo F statistic",
  note = "See `?clusterSim::index.G1`.",
  fun = function(task, model, pred, feats, extra.args) {
    requirePackages("clusterSim", default.method = "load")
    r = as.integer(as.factor(pred$data$response))
    clusterSim::index.G1(feats, r)
  }
)

#' @export G2
#' @rdname measures
#' @format none
G2 = makeMeasure(id = "G2", minimize = FALSE, best = Inf, worst = 0,
  properties = c("cluster", "req.pred", "req.feats"),
  name = "Baker and Hubert adaptation of Goodman-Kruskal's gamma statistic",
  note = "See `?clusterSim::index.G2`.",
  fun = function(task, model, pred, feats, extra.args) {
    requirePackages("clusterSim", default.method = "load")
    r = as.integer(as.factor(pred$data$response))
    clusterSim::index.G2(clusterSim::dist.GDM(feats), r)
  }
)

#' @export silhouette
#' @rdname measures
#' @format none
silhouette = makeMeasure(id = "silhouette", minimize = FALSE, best = Inf, worst = 0,
  properties = c("cluster", "req.pred", "req.feats"),
  name = "Rousseeuw's silhouette internal cluster quality index",
  note = "See `?clusterSim::index.S`.",
  fun = function(task, model, pred, feats, extra.args) {
    requirePackages("clusterSim", default.method = "load")
    r = as.integer(as.factor(pred$data$response))
    clusterSim::index.S(clusterSim::dist.GDM(feats), r)
  }
)

#' @title Performance measures.
#'
#' @description
#' A performance measure is evaluated after a single train/predict step and
#' returns a single number to assess the quality of the prediction (or maybe
#' only the model, think AIC). The measure itself knows whether it wants to be
#' minimized or maximized and for what tasks it is applicable.
#'
#' All supported measures can be found by [listMeasures] or as a table in the
#' tutorial appendix: <https://mlr.mlr-org.com/articles/tutorial/measures.html>.
#'
#' If you want a measure for a misclassification cost matrix, look at
#' [makeCostMeasure]. If you want to implement your own measure, look at
#' [makeMeasure].
#'
#' Most measures can directly be accessed via the function named after the
#' scheme measureX (e.g. measureSSE).
#'
#' For clustering measures, we compact the predicted cluster IDs such that they
#' form a continuous series starting with 1. If this is not the case, some of
#' the measures will generate warnings.
#'
#' Some measure have parameters. Their defaults are set in the constructor
#' [makeMeasure] and can be overwritten using [setMeasurePars].
#'
#' @param truth ([factor])\cr
#'   Vector of the true class.
#' @param response ([factor])\cr
#'   Vector of the predicted class.
#' @param negative (`character(1)`)\cr
#'   The name of the negative class.
#' @param positive (`character(1)`)\cr
#'   The name of the positive class.
#' @param probabilities ([numeric] | [matrix])\cr
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
  note = "Useful for feature selection.",
  fun = function(task, model, pred, feats, extra.args) {
    length(model$features) / sum(pred$task.desc$n.feat)
  })

#' @export timetrain
#' @rdname measures
#' @format none
timetrain = makeMeasure(id = "timetrain", minimize = TRUE, best = 0, worst = Inf,
  properties = c("classif", "classif.multi", "multilabel", "regr", "surv", "costsens", "cluster", "req.model"),
  name = "Time of fitting the model",
  fun = function(task, model, pred, feats, extra.args) {
    model$time
  })

#' @export timepredict
#' @rdname measures
#' @format none
timepredict = makeMeasure(id = "timepredict", minimize = TRUE, best = 0, worst = Inf,
  properties = c("classif", "classif.multi", "multilabel", "regr", "surv", "costsens", "cluster", "req.pred"),
  name = "Time of predicting test set",
  fun = function(task, model, pred, feats, extra.args) {
    pred$time
  })

#' @export timeboth
#' @rdname measures
#' @format none
timeboth = makeMeasure(id = "timeboth", minimize = TRUE, best = 0, worst = Inf,
  properties = c("classif", "classif.multi", "multilabel", "regr", "surv", "costsens", "cluster", "req.model", "req.pred"),
  name = "timetrain + timepredict",
  fun = function(task, model, pred, feats, extra.args) {
    model$time + pred$time
  })

###############################################################################
### regression ###
###############################################################################

#' @export sse
#' @rdname measures
#' @format none
sse = makeMeasure(id = "sse", minimize = TRUE, best = 0, worst = Inf,
  properties = c("regr", "req.pred", "req.truth"),
  name = "Sum of squared errors",
  note = "Defined as: sum((response - truth)^2)",
  fun = function(task, model, pred, feats, extra.args) {
    measureSSE(pred$data$truth, pred$data$response)
  })

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
  note = "Defined as: mean((response - truth)^2)",
  fun = function(task, model, pred, feats, extra.args) {
    measureMSE(pred$data$truth, pred$data$response)
  })

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
  name = "Root mean squared error",
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
  note = "Defined as: median((response - truth)^2).",
  fun = function(task, model, pred, feats, extra.args) {
    measureMEDSE(pred$data$truth, pred$data$response)
  })

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
  note = "Defined as: sum(abs(response - truth))",
  fun = function(task, model, pred, feats, extra.args) {
    measureSAE(pred$data$truth, pred$data$response)
  })

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
  note = "Defined as: mean(abs(response - truth))",
  fun = function(task, model, pred, feats, extra.args) {
    measureMAE(pred$data$truth, pred$data$response)
  })

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
  note = "Defined as: median(abs(response - truth)).",
  fun = function(task, model, pred, feats, extra.args) {
    measureMEDAE(pred$data$truth, pred$data$response)
  })

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
  })

#' @export measureRSQ
#' @rdname measures
#' @format none
measureRSQ = function(truth, response) {
  rss = measureSSE(truth, response)
  ess = sum((truth - mean(truth))^2L)
  if (ess == 0) {
    warning("Measure is undefined if all truth values are equal.")
    return(NA_real_)
  }
  1 - rss / ess
}

#' @export expvar
#' @rdname measures
#' @format none
expvar = makeMeasure(id = "expvar", minimize = FALSE, best = 1, worst = 0,
  properties = c("regr", "req.pred", "req.truth"),
  name = "Explained variance",
  note = "Similar to measure rsq (R-squared). Defined as explained_sum_of_squares / total_sum_of_squares.",
  fun = function(task, model, pred, feats, extra.args) {
    measureEXPVAR(pred$data$truth, pred$data$response)
  })

#' @export measureEXPVAR
#' @rdname measures
#' @format none
measureEXPVAR = function(truth, response) {
  regss = sum((response - mean(truth))^2L)
  ess = sum((truth - mean(truth))^2L)
  if (ess == 0) {
    warning("Measure is undefined if all truth values are equal.")
    return(NA_real_)
  }
  regss / ess
}

#' @export rrse
#' @rdname measures
#' @format none
rrse = makeMeasure(id = "rrse", minimize = TRUE, best = 0, worst = Inf,
  properties = c("regr", "req.pred", "req.truth"),
  name = "Root relative squared error",
  note = "Defined as sqrt (sum_of_squared_errors / total_sum_of_squares). Undefined for single instances and when every truth value is identical. In this case the output will be NA.",
  fun = function(task, model, pred, feats, extra.args) {
    measureRRSE(pred$data$truth, pred$data$response)
  })

#' @export measureRRSE
#' @rdname measures
#' @format none
measureRRSE = function(truth, response) {
  tss = sum((truth - mean(truth))^2L)
  if (tss == 0) {
    warning("Measure is undefined if all truth values are equal.")
    return(NA_real_)
  }
  sqrt(measureSSE(truth, response) / tss)
}

#' @export rae
#' @rdname measures
#' @format none
rae = makeMeasure(id = "rae", minimize = TRUE, best = 0, worst = Inf,
  properties = c("regr", "req.pred", "req.truth"),
  name = "Relative absolute error",
  note = "Defined as sum_of_absolute_errors / mean_absolute_deviation. Undefined for single instances and when every truth value is identical. In this case the output will be NA.",
  fun = function(task, model, pred, feats, extra.args) {
    measureRAE(pred$data$truth, pred$data$response)
  })

#' @export measureRAE
#' @rdname measures
#' @format none
measureRAE = function(truth, response) {
  meanad = sum(abs(truth - mean(truth)))
  if (meanad == 0) {
    warning("Measure is undefined if all truth values are equal.")
    return(NA_real_)
  }
  return(measureSAE(truth, response) / meanad)
}

#' @export mape
#' @rdname measures
#' @format none
mape = makeMeasure(id = "mape", minimize = TRUE, best = 0, worst = Inf,
  properties = c("regr", "req.pred", "req.truth"),
  name = "Mean absolute percentage error",
  note = "Defined as the abs(truth_i - response_i) / truth_i. Won't work if any truth value is equal to zero. In this case the output will be NA.",
  fun = function(task, model, pred, feats, extra.args) {
    measureMAPE(pred$data$truth, pred$data$response)
  })

#' @export measureMAPE
#' @rdname measures
#' @format none
measureMAPE = function(truth, response) {
  if (any(truth == 0)) {
    warning("Measure is undefined if any truth value is equal to 0.")
    return(NA_real_)
  }
  return(mean(abs((truth - response) / truth)))
}

#' @export msle
#' @rdname measures
#' @format none
msle = makeMeasure(id = "msle", minimize = TRUE, best = 0, worst = Inf,
  properties = c("regr", "req.pred", "req.truth"),
  name = "Mean squared logarithmic error",
  note = "Defined as: mean((log(response + 1, exp(1)) - log(truth + 1, exp(1)))^2).
  This measure is mostly used for count data, note that all predicted and actual target values must be greater or equal '-1'
  to compute the measure.",
  fun = function(task, model, pred, feats, extra.args) {
    measureMSLE(pred$data$truth, pred$data$response)
  })

#' @export measureMSLE
#' @rdname measures
#' @format none
measureMSLE = function(truth, response) {
  if (any(truth < -1)) {
    stop("All truth values must be greater or equal -1")
  }
  if (any(response < -1)) {
    stop("All predicted values must be greater or equal -1")
  }

  mean((log(response + 1) - log(truth + 1))^2)
}

#' @export rmsle
#' @rdname measures
#' @format none
rmsle = makeMeasure(id = "rmsle", minimize = TRUE, best = 0, worst = Inf,
  properties = c("regr", "req.pred", "req.truth"),
  name = "Root mean squared logarithmic error",
  note = "Defined as: sqrt(msle). Definition taken from:
  Definition taken from: https: / /www.kaggle.com / wiki / RootMeanSquaredLogarithmicError.
  This measure is mostly used for count data, note that all predicted and actual target values
  must be greater or equal '-1' to compute the measure.",
  fun = function(task, model, pred, feats, extra.args) {
    measureRMSLE(pred$data$truth, pred$data$response)
  })

#' @export measureRMSLE
#' @rdname measures
#' @format none
measureRMSLE = function(truth, response) {
  sqrt(measureMSLE(truth, response))
}

#' @export kendalltau
#' @rdname measures
#' @format none
kendalltau = makeMeasure(id = "kendalltau", minimize = FALSE, best = 1, worst = -1,
  properties = c("regr", "req.pred", "req.truth"),
  name = "Kendall's tau",
  note = "Defined as: Kendall's tau correlation between truth and response. Only looks at the order.
  See Rosset et al.: http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.95.1398&rep=rep1&type=pdf.",
  fun = function(task, model, pred, feats, extra.args) {
    measureKendallTau(pred$data$truth, pred$data$response)
  })

#' @export measureKendallTau
#' @rdname measures
#' @format none
measureKendallTau = function(truth, response) {
  cor(truth, response, use = "na.or.complete", method = "kendall")
}

#' @export spearmanrho
#' @rdname measures
#' @format none
spearmanrho = makeMeasure(id = "spearmanrho", minimize = FALSE, best = 1, worst = -1,
  properties = c("regr", "req.pred", "req.truth"),
  name = "Spearman's rho",
  note = "Defined as: Spearman's rho correlation between truth and response. Only looks at the order.
  See Rosset et al.: http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.95.1398&rep=rep1&type=pdf.",
  fun = function(task, model, pred, feats, extra.args) {
    measureSpearmanRho(pred$data$truth, pred$data$response)
  })

#' @export measureSpearmanRho
#' @rdname measures
#' @format none
measureSpearmanRho = function(truth, response) {
  cor(truth, response, use = "na.or.complete", method = "spearman")
}

###############################################################################
### classif multi ###
###############################################################################
#' @export mmce
#' @rdname measures
#' @format none
mmce = makeMeasure(id = "mmce", minimize = TRUE, best = 0, worst = 1,
  properties = c("classif", "classif.multi", "req.pred", "req.truth"),
  name = "Mean misclassification error",
  note = "Defined as: mean(response != truth)",
  fun = function(task, model, pred, feats, extra.args) {
    measureMMCE(pred$data$truth, pred$data$response)
  })

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
  note = "Defined as: mean(response == truth)",
  fun = function(task, model, pred, feats, extra.args) {
    measureACC(pred$data$truth, pred$data$response)
  })

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
    measureBER(pred$data$truth, pred$data$response)
  })

#' @export measureBER
#' @rdname measures
#' @format none
measureBER = function(truth, response) {
  # special case for predictions from FailureModel
  if (anyMissing(response)) {
    return(NA_real_)
  }
  mean(diag(1 - (table(truth, response) / table(truth, truth))))
}

#' @export multiclass.aunu
#' @rdname measures
#' @format none
multiclass.aunu = makeMeasure(id = "multiclass.aunu", minimize = FALSE, best = 1, worst = 0.5,
  properties = c("classif", "classif.multi", "req.pred", "req.truth", "req.prob"),
  name = "Average 1 vs. rest multiclass AUC",
  note = "Computes the AUC treating a c-dimensional classifier as c two-dimensional classifiers, where classes are assumed to have uniform distribution, in order to have a measure which is independent of class distribution change. See Ferri et al.: https://www.math.ucdavis.edu/~saito/data/roc/ferri-class-perf-metrics.pdf.",
  fun = function(task, model, pred, feats, extra.args) {
    measureAUNU(getPredictionProbabilities(pred, pred$task.desc$class.levels), pred$data$truth)
  })

#' @export measureAUNU
#' @rdname measures
#' @format none
measureAUNU = function(probabilities, truth) {
  if (length(unique(truth)) != nlevels(truth)) {
    warning("Measure is undefined if there isn't at least one sample per class.")
    return(NA_real_)
  }
  mean(vnapply(1:nlevels(truth), function(i) colAUC(probabilities[, i], truth == levels(truth)[i])))
}

#' @export multiclass.aunp
#' @rdname measures
#' @format none
multiclass.aunp = makeMeasure(id = "multiclass.aunp", minimize = FALSE, best = 1, worst = 0.5,
  properties = c("classif", "classif.multi", "req.pred", "req.truth", "req.prob"),
  name = "Weighted average 1 vs. rest multiclass AUC",
  note = "Computes the AUC treating a c-dimensional classifier as c two-dimensional classifiers, taking into account the prior probability of each class. See Ferri et al.: https://www.math.ucdavis.edu/~saito/data/roc/ferri-class-perf-metrics.pdf.",
  fun = function(task, model, pred, feats, extra.args) {
    measureAUNP(getPredictionProbabilities(pred, pred$task.desc$class.levels), pred$data$truth)
  })

#' @export measureAUNP
#' @rdname measures
#' @format none
measureAUNP = function(probabilities, truth) {
  if (length(unique(truth)) != nlevels(truth)) {
    warning("Measure is undefined if there isn't at least one sample per class.")
    return(NA_real_)
  }
  sum(vnapply(1:nlevels(truth), function(i) mean(truth == levels(truth)[i]) * colAUC(probabilities[, i], truth == levels(truth)[i])))
}

#' @export multiclass.au1u
#' @rdname measures
#' @format none
multiclass.au1u = makeMeasure(id = "multiclass.au1u", minimize = FALSE, best = 1, worst = 0.5,
  properties = c("classif", "classif.multi", "req.pred", "req.truth", "req.prob"),
  name = "Average 1 vs. 1 multiclass AUC",
  note = "Computes AUC of c(c - 1) binary classifiers (all possible pairwise combinations) while considering uniform distribution of the classes. See Ferri et al.: https://www.math.ucdavis.edu/~saito/data/roc/ferri-class-perf-metrics.pdf.",
  fun = function(task, model, pred, feats, extra.args) {
    measureAU1U(getPredictionProbabilities(pred, pred$task.desc$class.levels), pred$data$truth)
  })

#' @export measureAU1U
#' @rdname measures
#' @format none
measureAU1U = function(probabilities, truth) {
  m = colAUC(probabilities, truth)
  c = c(combn(1:nlevels(truth), 2))
  mean(m[cbind(rep(seq_len(nrow(m)), each = 2), c)])
}

#' @export multiclass.au1p
#' @rdname measures
#' @format none
multiclass.au1p = makeMeasure(id = "multiclass.au1p", minimize = FALSE, best = 1, worst = 0.5,
  properties = c("classif", "classif.multi", "req.pred", "req.truth", "req.prob"),
  name = "Weighted average 1 vs. 1 multiclass AUC",
  note = "Computes AUC of c(c - 1) binary classifiers while considering the a priori distribution of the classes. See Ferri et al.: https://www.math.ucdavis.edu/~saito/data/roc/ferri-class-perf-metrics.pdf.",
  fun = function(task, model, pred, feats, extra.args) {
    measureAU1P(getPredictionProbabilities(pred, pred$task.desc$class.levels), pred$data$truth)
  })

#' @export measureAU1P
#' @rdname measures
#' @format none
measureAU1P = function(probabilities, truth) {
  m = colAUC(probabilities, truth)
  weights = table(truth) / length(truth)
  m = m * matrix(rep(weights, each = nrow(m)), ncol = length(weights))
  c = c(combn(1:nlevels(truth), 2))
  sum(m[cbind(rep(seq_len(nrow(m)), each = 2), c)]) / (nlevels(truth) - 1)
}

#' @export multiclass.brier
#' @rdname measures
#' @format none
multiclass.brier = makeMeasure(id = "multiclass.brier", minimize = TRUE, best = 0, worst = 2,
  properties = c("classif", "classif.multi", "req.pred", "req.truth", "req.prob"),
  name = "Multiclass Brier score",
  note = "Defined as: (1/n) sum_i sum_j (y_ij - p_ij)^2, where y_ij = 1 if observation i has class j (else 0), and p_ij is the predicted probability of observation i for class j. From http://docs.lib.noaa.gov/rescue/mwr/078/mwr-078-01-0001.pdf.",
  fun = function(task, model, pred, feats, extra.args) {
    measureMulticlassBrier(getPredictionProbabilities(pred, pred$task.desc$class.levels), pred$data$truth)
  })

#' @export measureMulticlassBrier
#' @rdname measures
#' @format none
measureMulticlassBrier = function(probabilities, truth) {
  truth = factor(truth, levels = colnames(probabilities))
  mat01 = createDummyFeatures(truth)
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
  })

#' @export measureLogloss
#' @rdname measures
#' @format none
measureLogloss = function(probabilities, truth) {
  eps = 1e-15
  # let's confine the predicted probabilities to [eps,1 - eps], so logLoss doesn't reach infinity under any circumstance
  probabilities[probabilities > 1 - eps] = 1 - eps
  probabilities[probabilities < eps] = eps
  truth = match(as.character(truth), colnames(probabilities))
  p = getRowEls(probabilities, truth)
  -1 * mean(log(p))
}

#' @export ssr
#' @rdname measures
#' @format none
ssr = makeMeasure(id = "ssr", minimize = FALSE, best = 1, worst = 0,
  properties = c("classif", "classif.multi", "req.truth", "req.prob"),
  name = "Spherical Scoring Rule",
  note = "Defined as: mean(p_i(sum_j(p_ij))), where p_i is the predicted probability of the true class of observation i and p_ij is the predicted probablity of observation i for class j.
  See: Bickel, J. E. (2007). Some comparisons among quadratic, spherical, and logarithmic scoring rules. Decision Analysis, 4(2), 49-65.",
  fun = function(task, model, pred, feats, extra.args) {
    measureSSR(getPredictionProbabilities(pred, cl = pred$task.desc$class.levels), pred$data$truth)
  })

#' @export measureSSR
#' @rdname measures
#' @format none
measureSSR = function(probabilities, truth) {
  truth = match(as.character(truth), colnames(probabilities))
  p = getRowEls(probabilities, truth)
  mean(p / sqrt(rowSums(probabilities^2)))
}

#' @export qsr
#' @rdname measures
#' @format none
qsr = makeMeasure(id = "qsr", minimize = FALSE, best = 1, worst = -1,
  properties = c("classif", "classif.multi", "req.truth", "req.prob"),
  name = "Quadratic Scoring Rule",
  note = "Defined as: 1 - (1/n) sum_i sum_j (y_ij - p_ij)^2, where y_ij = 1 if observation i has class j (else 0), and p_ij is the predicted probablity of observation i for class j.
  This scoring rule is the same as 1 - multiclass.brier.
  See: Bickel, J. E. (2007). Some comparisons among quadratic, spherical, and logarithmic scoring rules. Decision Analysis, 4(2), 49-65.",
  fun = function(task, model, pred, feats, extra.args) {
    measureQSR(getPredictionProbabilities(pred, cl = pred$task.desc$class.levels), pred$data$truth)
  })

#' @export measureQSR
#' @rdname measures
#' @format none
measureQSR = function(probabilities, truth) {
  # We add this line because binary tasks only output one probability column
  if (is.null(dim(probabilities))) probabilities = cbind(probabilities, 1 - probabilities)
  truth = factor(truth, levels = colnames(probabilities))
  1 - mean(rowSums((probabilities - createDummyFeatures(truth))^2))
}

#' @export lsr
#' @rdname measures
#' @format none
lsr = makeMeasure(id = "lsr", minimize = FALSE, best = 0, worst = -Inf,
  properties = c("classif", "classif.multi", "req.truth", "req.prob"),
  name = "Logarithmic Scoring Rule",
  note = "Defined as: mean(log(p_i)), where p_i is the predicted probability of the true class of observation i.
  This scoring rule is the same as the negative logloss, self-information or surprisal.
  See: Bickel, J. E. (2007). Some comparisons among quadratic, spherical, and logarithmic scoring rules. Decision Analysis, 4(2), 49-65.",
  fun = function(task, model, pred, feats, extra.args) {
    measureLSR(getPredictionProbabilities(pred, cl = pred$task.desc$class.levels), pred$data$truth)
  })

#' @export measureLSR
#' @rdname measures
#' @format none
measureLSR = function(probabilities, truth) {
  -1 * measureLogloss(probabilities, truth)
}

#' @export kappa
#' @rdname measures
#' @format none
kappa = makeMeasure(id = "kappa", minimize = FALSE, best = 1, worst = -1,
  properties = c("classif", "classif.multi", "req.pred", "req.truth"),
  name = "Cohen's kappa",
  note = "Defined as: 1 - (1 - p0) / (1 - pe). With: p0 = 'observed frequency of
    agreement' and pe = 'expected agremeent frequency under independence",
  fun = function(task, model, pred, feats, extra.args) {
    measureKAPPA(pred$data$truth, pred$data$response)
  })

#' @export measureKAPPA
#' @rdname measures
#' @format none
measureKAPPA = function(truth, response) {

  # get confusion matrix
  conf.mat = table(truth, response)
  conf.mat = conf.mat / sum(conf.mat)

  # observed agreement frequency
  p0 = sum(diag(conf.mat))

  # get expected probs under independence
  rowsum = rowSums(conf.mat)
  colsum = colSums(conf.mat)
  pe = sum(rowsum * colsum) / sum(conf.mat)^2

  # calculate kappa
  1 - (1 - p0) / (1 - pe)
}

#' @export wkappa
#' @rdname measures
#' @format none
wkappa = makeMeasure(id = "wkappa", minimize = FALSE, best = 1, worst = -1,
  properties = c("classif", "classif.multi", "req.pred", "req.truth"),
  name = "Mean quadratic weighted kappa",
  note = "Defined as: 1 - sum(weights * conf.mat) / sum(weights * expected.mat),
    the weight matrix measures seriousness of disagreement with the squared euclidean metric.",
  fun = function(task, model, pred, feats, extra.args) {
    measureWKAPPA(pred$data$truth, pred$data$response)
  })

#' @export measureWKAPPA
#' @rdname measures
#' @format none
measureWKAPPA = function(truth, response) {

  # get confusion matrix
  conf.mat = table(truth, response)
  conf.mat = conf.mat / sum(conf.mat)

  # get expected probs under independence
  rowsum = rowSums(conf.mat)
  colsum = colSums(conf.mat)
  expected.mat = rowsum %*% t(colsum)

  # get weights
  class.values = seq_along(levels(truth)) - 1L
  weights = outer(class.values, class.values, FUN = function(x, y) (x - y)^2)

  # calculate weighted kappa
  1 - sum(weights * conf.mat) / sum(weights * expected.mat)
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
  note = "Integral over the graph that results from computing fpr and tpr for many different thresholds.",
  fun = function(task, model, pred, feats, extra.args) {
    if (anyMissing(pred$data$response) || length(unique(pred$data$truth)) == 1L) {
      return(NA_real_)
    }
    measureAUC(getPredictionProbabilities(pred), pred$data$truth, pred$task.desc$negative, pred$task.desc$positive)
  })

#' @export measureAUC
#' @rdname measures
#' @format none
measureAUC = function(probabilities, truth, negative, positive) {
  if (is.factor(truth)) {
    i = as.integer(truth) == which(levels(truth) == positive)
  } else {
    i = truth == positive
  }
  if (length(unique(i)) < 2L) {
    stop("truth vector must have at least two classes")
  }
  # Use fast ranking function from data.table for larger vectors
  if (length(i) > 5000L) {
    r = frankv(probabilities)
  } else {
    r = rank(probabilities)
  }
  n.pos = as.numeric(sum(i))
  n.neg = length(i) - n.pos
  (sum(r[i]) - n.pos * (n.pos + 1) / 2) / (n.pos * n.neg)
}

#' @export brier
#' @rdname measures
#' @format none
brier = makeMeasure(id = "brier", minimize = TRUE, best = 0, worst = 1,
  properties = c("classif", "req.pred", "req.truth", "req.prob"),
  name = "Brier score",
  note = "The Brier score is defined as the quadratic difference between the probability and the value (1,0) for the class.
  That means we use the numeric representation 1 and 0 for our target classes. It is similiar to the mean squared error in regression.
  multiclass.brier is the sum over all one vs. all comparisons and for a binary classifcation 2 * brier.",
  fun = function(task, model, pred, feats, extra.args) {
    measureBrier(getPredictionProbabilities(pred), pred$data$truth, pred$task.desc$negative, pred$task.desc$positive)
  })

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
  })

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
  properties = c("classif", "classif.multi", "req.pred", "req.truth"),
  name = "Balanced accuracy",
  note = "For binary tasks, mean of true positive rate and true negative rate.",
  fun = function(task, model, pred, feats, extra.args) {
    measureBAC(pred$data$truth, pred$data$response)
  })

#' @export measureBAC
#' @rdname measures
#' @format none
measureBAC = function(truth, response) {
  mean(diag(table(truth, response) / table(truth, truth)))
}

#' @export tp
#' @rdname measures
#' @format none
tp = makeMeasure(id = "tp", minimize = FALSE, best = Inf, worst = 0,
  properties = c("classif", "req.pred", "req.truth"),
  name = "True positives",
  note = "Sum of all correctly classified observations in the positive class.",
  fun = function(task, model, pred, feats, extra.args) {
    measureTP(pred$data$truth, pred$data$response, pred$task.desc$positive)
  })

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
  note = "Sum of correctly classified observations in the negative class. Also called correct rejections.",
  fun = function(task, model, pred, feats, extra.args) {
    measureTN(pred$data$truth, pred$data$response, pred$task.desc$negative)
  })

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
  note = "Sum of misclassified observations in the positive class. Also called false alarms.",
  fun = function(task, model, pred, feats, extra.args) {
    measureFP(pred$data$truth, pred$data$response, pred$task.desc$positive)
  })

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
  note = "Sum of misclassified observations in the negative class. Also called misses.",
  fun = function(task, model, pred, feats, extra.args) {
    measureFN(pred$data$truth, pred$data$response, pred$task.desc$negative)
  })

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
  note = "Percentage of correctly classified observations in the positive class. Also called hit rate or recall or sensitivity.",
  fun = function(task, model, pred, feats, extra.args) {
    measureTPR(pred$data$truth, pred$data$response, pred$task.desc$positive)
  })

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
  note = "Percentage of correctly classified observations in the negative class. Also called specificity.",
  fun = function(task, model, pred, feats, extra.args) {
    measureTNR(pred$data$truth, pred$data$response, pred$task.desc$negative)
  })

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
  properties = c("classif", "req.pred", "req.truth"),
  name = "False positive rate",
  note = "Percentage of misclassified observations in the positive class. Also called false alarm rate or fall-out.",
  fun = function(task, model, pred, feats, extra.args) {
    measureFPR(pred$data$truth, pred$data$response, pred$task.desc$negative, pred$task.desc$positive)
  })

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
  note = "Percentage of misclassified observations in the negative class.",
  fun = function(task, model, pred, feats, extra.args) {
    measureFNR(pred$data$truth, pred$data$response, pred$task.desc$negative, pred$task.desc$positive)
  })

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
  note = "Defined as: tp / (tp + fp). Also called precision. If the denominator is 0, PPV is set to be either 1 or 0 depending on whether the highest probability prediction is positive (1) or negative (0).",
  fun = function(task, model, pred, feats, extra.args) {
    if (pred$predict.type == "prob") {
      prob = getPredictionProbabilities(pred)
    } else {
      prob = NULL
    }
    measurePPV(pred$data$truth, pred$data$response, pred$task.desc$positive, prob)
  })

#' @export measurePPV
#' @rdname measures
#' @format none

measurePPV = function(truth, response, positive, probabilities = NULL) {
  denominator = sum(response == positive)
  ifelse(denominator == 0, measureEdgeCase(truth, positive, probabilities), measureTP(truth, response, positive) / denominator)
}
measureEdgeCase = function(truth, positive, prob) {
  if (!is.null(prob)) {
    rs = sort(prob, index.return = TRUE)
    erst = ifelse(truth[getLast(rs$ix)] == positive, 1, 0)
  } else {
    erst = NA
  }
  erst
}


#' @export npv
#' @rdname measures
#' @format none
npv = makeMeasure(id = "npv", minimize = FALSE, best = 1, worst = 0,
  properties = c("classif", "req.pred", "req.truth"),
  name = "Negative predictive value",
  note = "Defined as: tn / (tn + fn).",
  fun = function(task, model, pred, feats, extra.args) {
    measureNPV(pred$data$truth, pred$data$response, pred$task.desc$negative)
  })

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
  note = "Defined as: fp / (tp + fp).",
  fun = function(task, model, pred, feats, extra.args) {
    measureFDR(pred$data$truth, pred$data$response, pred$task.desc$positive)
  })

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
  note = "Defined as (tp * tn - fp * fn) / sqrt((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn)), denominator set to 1 if 0",
  fun = function(task, model, pred, feats, extra.args) {
    measureMCC(pred$data$truth, pred$data$response, pred$task.desc$negative, pred$task.desc$positive)
  })

#' @export measureMCC
#' @rdname measures
#' @format none
measureMCC = function(truth, response, negative, positive) {

  tn = as.numeric(measureTN(truth, response, negative))
  tp = as.numeric(measureTP(truth, response, positive))
  fn = as.numeric(measureFN(truth, response, negative))
  fp = as.numeric(measureFP(truth, response, positive))

  denom = sqrt((tp + fp) * (tp + fn) * (tn + fp) * (tn + fn))
  # According to Wikipedia, the denominator can be set arbitrarily if it's 0. 1 seems to make as much sense as anything else.
  if (denom == 0) denom = 1

  (tp * tn - fp * fn) / denom
}

#' @export f1
#' @rdname measures
#' @format none
f1 = makeMeasure(id = "f1", minimize = FALSE, best = 1, worst = 0,
  properties = c("classif", "req.pred", "req.truth"),
  name = "F1 measure",
  note = "Defined as: 2 * tp/ (sum(truth == positive) + sum(response == positive))",
  fun = function(task, model, pred, feats, extra.args) {
    measureF1(pred$data$truth, pred$data$response, pred$task.desc$positive)
  })

#' @export measureF1
#' @rdname measures
#' @format none
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
  })

#' @export measureGMEAN
#' @rdname measures
#' @format none
#' @references
#' He, H. & Garcia, E. A. (2009)
#' *Learning from Imbalanced Data.*
#' IEEE Transactions on Knowledge and Data Engineering, vol. 21, no. 9. pp. 1263-1284.
measureGMEAN = function(truth, response, negative, positive) {
  sqrt(measureTPR(truth, response, positive) * measureTNR(truth, response, negative))
}

#' @export gpr
#' @rdname measures
#' @format none
gpr = makeMeasure(id = "gpr", minimize = FALSE, best = 1, worst = 0,
  properties = c("classif", "req.pred", "req.truth"),
  name = "Geometric mean of precision and recall.",
  note = "Defined as: sqrt(ppv * tpr)",
  fun = function(task, model, pred, feats, extra.args) {
    measureGPR(pred$data$truth, pred$data$response, pred$task.desc$positive)
  })

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
  note = "Proportion of labels that are predicted incorrectly, following the definition
  by Charte and Charte: https://journal.r-project.org/archive/2015-2/charte-charte.pdf.",
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
  note = "Proportion of observations where the complete multilabel set (all 0-1-labels) is predicted incorrectly,
  following the definition by Charte and Charte: https://journal.r-project.org/archive/2015-2/charte-charte.pdf.",
  fun = function(task, model, pred, feats, extra.args) {
    measureMultilabelSubset01(getPredictionTruth.PredictionMultilabel(pred),
      getPredictionResponse.PredictionMultilabel(pred))
  })

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
  note = "Harmonic mean of precision and recall on a per instance basis (Micro-F1), following the
  definition by Montanes et al.: http: / /www.sciencedirect.com / science / article / pii / S0031320313004019.
  Fractions where the denominator becomes 0 are replaced with 1 before computing the average across all instances.",
  fun = function(task, model, pred, feats, extra.args) {
    measureMultilabelF1(getPredictionTruth.PredictionMultilabel(pred),
      getPredictionResponse.PredictionMultilabel(pred))
  })

#' @export measureMultilabelF1
#' @rdname measures
#' @format none
measureMultilabelF1 = function(truth, response) {
  numerator = 2 * rowSums(truth & response)
  denominator = rowSums(truth + response)
  mean(ifelse(denominator == 0, 1, numerator / denominator))
}

#' @export multilabel.acc
#' @rdname measures
#' @format none
multilabel.acc = makeMeasure(id = "multilabel.acc", minimize = FALSE, best = 1, worst = 0,
  properties = c("multilabel", "req.pred", "req.truth"),
  name = "Accuracy (multilabel)",
  note = "Averaged proportion of correctly predicted labels with respect to the total number of labels for each instance,
  following the definition by Charte and Charte: https: / /journal.r-project.org / archive / 2015 - 2 / charte-charte.pdf.
  Fractions where the denominator becomes 0 are replaced with 1 before computing the average across all instances.",
  fun = function(task, model, pred, feats, extra.args) {
    measureMultilabelACC(getPredictionTruth.PredictionMultilabel(pred),
      getPredictionResponse.PredictionMultilabel(pred))
  })

#' @export measureMultilabelACC
#' @rdname measures
#' @format none
measureMultilabelACC = function(truth, response) {
  numerator = rowSums(truth & response)
  denominator = rowSums(truth | response)
  mean(ifelse(denominator == 0, 1, numerator / denominator))
}

#' @export multilabel.ppv
#' @rdname measures
#' @format none
multilabel.ppv = makeMeasure(id = "multilabel.ppv", minimize = FALSE, best = 1, worst = 0,
  properties = c("multilabel", "req.pred", "req.truth"),
  name = "Positive predictive value (multilabel)",
  note = "Also called precision. Averaged ratio of correctly predicted labels for each instance,
  following the definition by Charte and Charte: https: / /journal.r-project.org / archive / 2015 - 2 / charte-charte.pdf.
  Fractions where the denominator becomes 0 are ignored in the average calculation.",
  fun = function(task, model, pred, feats, extra.args) {
    measureMultilabelPPV(getPredictionTruth.PredictionMultilabel(pred),
      getPredictionResponse.PredictionMultilabel(pred))
  })

#' @export measureMultilabelPPV
#' @rdname measures
#' @format none
measureMultilabelPPV = function(truth, response) {
  numerator = rowSums(truth & response)
  denominator = rowSums(response)
  mean(numerator / denominator, na.rm = TRUE)
}

#' @export multilabel.tpr
#' @rdname measures
#' @format none
multilabel.tpr = makeMeasure(id = "multilabel.tpr", minimize = FALSE, best = 1, worst = 0,
  properties = c("multilabel", "req.pred", "req.truth"),
  name = "TPR (multilabel)",
  note = "Also called recall. Averaged proportion of predicted labels which are relevant for each instance,
  following the definition by Charte and Charte: https: / /journal.r-project.org / archive / 2015 - 2 / charte-charte.pdf.
  Fractions where the denominator becomes 0 are ignored in the average calculation.",
  fun = function(task, model, pred, feats, extra.args) {
    measureMultilabelTPR(getPredictionTruth.PredictionMultilabel(pred),
      getPredictionResponse.PredictionMultilabel(pred))
  })

#' @export measureMultilabelTPR
#' @rdname measures
#' @format none
measureMultilabelTPR = function(truth, response) {
  numerator = rowSums(truth & response)
  denominator = rowSums(truth)
  mean(numerator / denominator, na.rm = TRUE)
}

###############################################################################
### survival ###
###############################################################################
#' @export cindex
#' @rdname measures
#' @format none
cindex = makeMeasure(id = "cindex", minimize = FALSE, best = 1, worst = 0,
  properties = c("surv", "req.pred", "req.truth"),
  name = "Harrell's Concordance index",
  note = "Fraction of all pairs of subjects whose predicted survival times are correctly ordered among all subjects that can actually be ordered. In other words, it is the probability of concordance between the predicted and the observed survival.",
  fun = function(task, model, pred, feats, extra.args) {
    requirePackages("_Hmisc")
    y = getPredictionResponse(pred)
    if (anyMissing(y)) {
      return(NA_real_)
    }
    s = getPredictionTruth(pred)
    Hmisc::rcorr.cens(-1 * y, s)[["C Index"]]
  })

#' @export cindex.uno
#' @rdname measures
#' @format none
#' @references
#' H. Uno et al.
#' *On the C-statistics for Evaluating Overall Adequacy of Risk Prediction Procedures with Censored Survival Data*
#' Statistics in medicine. 2011;30(10):1105-1117. <https://doi.org/10.1002/sim.4154>.
cindex.uno = makeMeasure(id = "cindex.uno", minimize = FALSE, best = 1, worst = 0,
  properties = c("surv", "req.pred", "req.truth", "req.model", "req.task"),
  name = "Uno's Concordance index",
  note = "Fraction of all pairs of subjects whose predicted survival times are correctly ordered among all subjects that can actually be ordered. In other words, it is the probability of concordance between the predicted and the observed survival. Corrected by weighting with IPCW as suggested by Uno. Implemented in survAUC::UnoC.",
  fun = function(task, model, pred, feats, extra.args) {
    requirePackages("_survAUC")
    y = getPredictionResponse(pred)
    if (anyMissing(y)) {
      return(NA_real_)
    }
    surv.train = getTaskTargets(task, recode.target = "surv")[model$subset]
    max.time = assertNumber(extra.args$max.time, null.ok = TRUE) %??% max(getTaskTargets(task)[, 1L])
    survAUC::UnoC(Surv.rsp = surv.train, Surv.rsp.new = getPredictionTruth(pred), time = max.time, lpnew = y)
  },
  extra.args = list(max.time = NULL)
)

#' @export iauc.uno
#' @rdname measures
#' @format none
#' @references
#' H. Uno et al.
#' *Evaluating Prediction Rules for T-Year Survivors with Censored Regression Models*
#' Journal of the American Statistical Association 102, no. 478 (2007): 527-37. <https://www.jstor.org/stable/27639883>.
iauc.uno = makeMeasure(id = "iauc.uno", minimize = FALSE, best = 1, worst = 0,
  properties = c("surv", "req.pred", "req.truth", "req.model", "req.task"),
  name = "Uno's estimator of cumulative AUC for right censored time-to-event data",
  note = "To set an upper time limit, set argument max.time (defaults to max time in complete task). Implemented in survAUC::AUC.uno.",
  fun = function(task, model, pred, feats, extra.args) {
    requirePackages("_survAUC")
    max.time = assertNumber(extra.args$max.time, null.ok = TRUE) %??% max(getTaskTargets(task)[, 1L])
    times = seq(from = 0, to = max.time, length.out = extra.args$resolution)
    surv.train = getTaskTargets(task, recode.target = "surv")[model$subset]
    y = getPredictionResponse(pred)
    if (anyMissing(y)) {
      return(NA_real_)
    }
    survAUC::AUC.uno(Surv.rsp = surv.train, Surv.rsp.new = getPredictionTruth(pred), times = times, lpnew = y)$iauc
  },
  extra.args = list(max.time = NULL, resolution = 1000L)
)

#' @export ibrier
#' @rdname measures
#' @format none
ibrier = makeMeasure(id = "ibrier", minimize = TRUE, best = 0, worst = 1,
  properties = c("surv", "req.truth", "req.model", "req.task"),
  name = "Integrated brier score using Kaplan-Meier estimator for weighting",
  note = "Only works for methods for which probabilities are provided via pec::predictSurvProb. Currently these are only coxph and randomForestSRC. To set an upper time limit, set argument max.time (defaults to max time in test data). Implemented in pec::pec",
  fun = function(task, model, pred, feats, extra.args) {

    requirePackages(c("survival", "pec"))
    targets = getTaskTargets(task)
    tn = getTaskTargetNames(task)
    f = as.formula(sprintf("Surv(%s, %s) ~ 1", tn[1L], tn[2L]))
    newdata = getTaskData(task)[model$subset, ]
    max.time = extra.args$max.time %??% max(newdata[[tn[1L]]])
    grid = seq(0, max.time, length.out = extra.args$resolution)

    probs = predictSurvProb(getLearnerModel(model, more.unwrap = TRUE), newdata = newdata, times = grid)
    perror = pec(probs, f, data = newdata[, tn], times = grid, exact = FALSE, exactness = 99L,
      maxtime = max.time, verbose = FALSE, reference = FALSE)


    # FIXME: this might be the wrong number!
    crps(perror, times = max.time)[1L, ]
  },
  extra.args = list(max.time = NULL, resolution = 1000L)
)

###############################################################################
### cost-sensitive ###
###############################################################################
#' @export meancosts
#' @rdname measures
meancosts = makeMeasure(id = "meancosts", minimize = TRUE, best = 0, worst = Inf,
  properties = c("costsens", "req.pred", "req.task"),
  name = "Mean costs of the predicted choices",
  note = "Defined as: mean(y), where y is the vector of costs for the predicted classes.",
  fun = function(task, model, pred, feats, extra.args) {
    classes = as.character(pred$data$response)
    ids = pred$data$id
    costs = getTaskCosts(task)
    y = mapply(function(id, cl) {
      costs[id, cl]
    }, ids, classes, SIMPLIFY = TRUE, USE.NAMES = FALSE)
    mean(y)
  })

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
  })

###############################################################################
### clustering ###
###############################################################################
#' @export db
#' @rdname measures
#' @format none
db = makeMeasure(id = "db", minimize = TRUE, best = 0, worst = Inf,
  properties = c("cluster", "req.pred", "req.feats"),
  name = "Davies-Bouldin cluster separation measure",
  note = "Ratio of the within cluster scatter, to the between cluster separation, averaged over the clusters. See `?clusterSim::index.DB`.",
  fun = function(task, model, pred, feats, extra.args) {
    if (length(unique(pred$data$response)) > 1L) {
      requirePackages("clusterSim", default.method = "load")
      r = as.integer(as.factor(pred$data$response))
      clusterSim::index.DB(feats, r)$DB
    } else {
      NA
    }
  })

#' @export dunn
#' @rdname measures
#' @format none
dunn = makeMeasure(id = "dunn", minimize = FALSE, best = Inf, worst = 0,
  properties = c("cluster", "req.pred", "req.feats"),
  name = "Dunn index",
  note = "Defined as the ratio of the smallest distance between observations not in the same cluster to the largest intra-cluster distance. See `?clValid::dunn`.",
  fun = function(task, model, pred, feats, extra.args) {
    # produced a confusing note in some cases, see issue #232
    suppressMessages(requirePackages("clValid", default.method = "load"))
    r = as.integer(as.factor(pred$data$response))
    clValid::dunn(Data = feats, clusters = r)
  })

#' @export G1
#' @rdname measures
#' @format none
G1 = makeMeasure(id = "G1", minimize = FALSE, best = Inf, worst = 0, # nolint
  properties = c("cluster", "req.pred", "req.feats"),
  name = "Calinski-Harabasz pseudo F statistic",
  note = "Defined as ratio of between-cluster variance to within cluster variance. See `?clusterSim::index.G1`.",
  fun = function(task, model, pred, feats, extra.args) {
    requirePackages("clusterSim", default.method = "load")
    r = as.integer(as.factor(pred$data$response))
    clusterSim::index.G1(feats, r)
  })

#' @export G2
#' @rdname measures
#' @format none
G2 = makeMeasure(id = "G2", minimize = FALSE, best = 1, worst = 0, # nolint
  properties = c("cluster", "req.pred", "req.feats"),
  name = "Baker and Hubert adaptation of Goodman-Kruskal's gamma statistic",
  note = "Defined as: (number of concordant comparisons - number of discordant comparisons) / (number of concordant comparisons + number of discordant comparisons). See `?clusterSim::index.G2`.",
  fun = function(task, model, pred, feats, extra.args) {
    requirePackages("clusterSim", default.method = "load")
    r = as.integer(as.factor(pred$data$response))
    clusterSim::index.G2(clusterSim::dist.GDM(feats), r)
  })

#' @export silhouette
#' @rdname measures
#' @format none
silhouette = makeMeasure(id = "silhouette", minimize = FALSE, best = Inf, worst = 0,
  properties = c("cluster", "req.pred", "req.feats"),
  name = "Rousseeuw's silhouette internal cluster quality index",
  note = "Silhouette value of an observation is a measure of how similar an object is to its own cluster compared to other clusters. The measure is calculated as the average of all silhouette values. See `?clusterSim::index.S`.",
  fun = function(task, model, pred, feats, extra.args) {
    requirePackages("clusterSim", default.method = "load")
    r = as.integer(as.factor(pred$data$response))
    clusterSim::index.S(clusterSim::dist.GDM(feats), r)
  })

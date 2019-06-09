#' @export
as.data.frame.Prediction = function(x, row.names = NULL, optional = FALSE, ...) {
  x$data
}

#' Get probabilities for some classes.
#'
#' @template arg_pred
#' @param cl ([character])\cr
#'   Names of classes.
#'   Default is either all classes for multi-class / multilabel problems or the positive class for binary classification.
#' @return ([data.frame]) with numerical columns or a numerical vector if length of `cl` is 1.
#'   Order of columns is defined by `cl`.
#' @export
#' @family predict
#' @examples
#' task = makeClassifTask(data = iris, target = "Species")
#' lrn = makeLearner("classif.lda", predict.type = "prob")
#' mod = train(lrn, task)
#' # predict probabilities
#' pred = predict(mod, newdata = iris)
#'
#' # Get probabilities for all classes
#' head(getPredictionProbabilities(pred))
#'
#' # Get probabilities for a subset of classes
#' head(getPredictionProbabilities(pred, c("setosa", "virginica")))
getPredictionProbabilities = function(pred, cl) {
  assertClass(pred, classes = "Prediction")
  ttype = pred$task.desc$type
  if (ttype %nin% c("classif", "cluster", "multilabel")) {
    stop("Prediction was not generated from a ClassifTask, MultilabelTask or ClusterTask!")
  }
  if (missing(cl)) {
    if (ttype == "classif") {
      if (length(pred$task.desc$class.levels) == 2L) {
        cl = pred$task.desc$positive
      } else {
        cl = pred$task.desc$class.levels
      }
    } else if (ttype == "multilabel") {
      cl = pred$task.desc$class.levels
    }
  } else {
    if (ttype == "cluster") {
      stopf("You can only ask for probs of all classes currently in clustering!")
    } else {
      assertCharacter(cl, any.missing = FALSE)
    }
  }
  if (pred$predict.type != "prob") {
    stop("Probabilities not present in Prediction object!")
  }
  cns = colnames(pred$data)
  if (ttype %in% c("classif", "multilabel")) {
    cl2 = stri_paste("prob", cl, sep = ".")
    if (!all(cl2 %in% cns)) {
      stopf("Trying to get probabilities for nonexistant classes: %s", collapse(cl))
    }
    y = pred$data[, cl2]
    if (length(cl) > 1L) {
      colnames(y) = cl
    }
  } else if (ttype == "cluster") {
    y = pred$data[, stri_detect_regex(cns, "prob\\.")]
    colnames(y) = seq_col(y)
  }
  return(y)
}

#' @title Get summarizing task description from prediction.
#'
#' @description See title.
#'
#' @template arg_pred
#' @return ret_taskdesc
#' @export
#' @family predict
getPredictionTaskDesc = function(pred) {
  assertClass(pred, "Prediction")
  pred$task.desc
}

#' Deprecated, use `getPredictionProbabilities` instead.
#' @param pred Deprecated.
#' @param cl Deprecated.
#' @export
getProbabilities = function(pred, cl) {
  .Deprecated("getPredictionProbabilities")
  getPredictionProbabilities(pred, cl)
}

# c.Prediction = function(...) {
#  preds = list(...)
#  id = Reduce(c, lapply(preds, function(x) x@id))
#  response = Reduce(c, lapply(preds, function(x) x@response))
#  target = Reduce(c, lapply(preds, function(x) x@target))
#  weights = Reduce(c, lapply(preds, function(x) x@weights))
#  prob = Reduce(rbind, lapply(preds, function(x) x@prob))
#  return(new("Prediction", task.desc = preds[[1]]@desc, id = id, response = response, target = target, weights = weights, prob = prob));
# }


#' @title Get response / truth from prediction object.
#'
#' @description
#' The following types are returned, depending on task type:
#'  \tabular{ll}{
#'    classif     \tab factor\cr
#'    regr        \tab numeric\cr
#'    se          \tab numeric\cr
#'    cluster     \tab integer\cr
#'    surv        \tab numeric\cr
#'    multilabel  \tab logical matrix, columns named with labels\cr
#' }
#'
#' @template arg_pred
#' @return See above.
#' @export
#' @family predict
getPredictionResponse = function(pred) {
  UseMethod("getPredictionResponse")
}

#' @export
getPredictionResponse.default = function(pred) {
  # this should work for classif, regr and cluster and surv
  pred$data[["response"]]
}

#' @export
getPredictionResponse.PredictionMultilabel = function(pred) {
  i = stri_detect_regex(colnames(pred$data), "^response\\.")
  m = as.matrix(pred$data[, i])
  setColNames(m, pred$task.desc$class.levels)
}

#' @rdname getPredictionResponse
#' @export
getPredictionSE = function(pred) {
  UseMethod("getPredictionSE")
}

#' @export
getPredictionSE.default = function(pred) {
  pred$data[["se"]]
}

#' @rdname getPredictionResponse
#' @export
getPredictionTruth = function(pred) {
  UseMethod("getPredictionTruth")
}

#' @export
getPredictionTruth.default = function(pred) {
  pred$data[["truth"]]
}

#' @export
getPredictionTruth.PredictionCluster = function(pred) {
  stop("There is no truth for cluster tasks")
}

#' @export
getPredictionTruth.PredictionSurv = function(pred) {
  Surv(pred$data$truth.time, pred$data$truth.event, type = "right")
}

#' @export
getPredictionTruth.PredictionMultilabel = function(pred) {
  i = stri_detect_regex(colnames(pred$data), "^truth\\.")
  m = as.matrix(pred$data[, i])
  setColNames(m, pred$task.desc$class.levels)
}

#' @title Return the error dump of a failed Prediction.
#'
#' @description
#' Returns the error dump that can be used with `debugger()` to evaluate errors.
#' If [configureMlr] configuration `on.error.dump` is `FALSE` or if the
#' prediction did not fail, this returns `NULL`.
#'
#' @template arg_pred
#' @return (`last.dump`).
#' @family debug
#' @export
getPredictionDump = function(pred) {
  pred$dump
}

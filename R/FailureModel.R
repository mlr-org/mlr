#' @title Failure model.
#'
#' @description
#'
#' A subclass of \code{\link{WrappedModel}}. It is created
#' - if you set the respective option in \code{\link{configureMlr}} -
#' when a model internally crashed during training.
#' The model always predicts NAs.
#'
#' The if mlr option \code{on.error.dump} is \code{TRUE}, the
#' \code{FailureModel} contains the debug trace of the error.
#' It can be accessed with \code{getFailureModelDump} and
#' inspected with \code{debugger}.
#'
#' Its encapsulated \code{learner.model} is simply a string:
#' The error message that was generated when the model crashed.
#' The following code shows how to access the message.
#'
#' @name FailureModel
#' @family debug
#' @rdname FailureModel
#' @examples
#' configureMlr(on.learner.error = "warn")
#' data = iris
#' data$newfeat = 1 # will make LDA crash
#' task = makeClassifTask(data = data, target = "Species")
#' m = train("classif.lda", task) # LDA crashed, but mlr catches this
#' print(m)
#' print(m$learner.model) # the error message
#' p = predict(m, task) # this will predict NAs
#' print(p)
#' print(performance(p))
#' configureMlr(on.learner.error = "stop")
NULL

predictFailureModel = function(model, newdata) {
  lrn = model$learner
  type = lrn$type
  ptype = lrn$predict.type
  n = nrow(newdata)
  if (type == "classif") {
    levs = model$task.desc$class.levels
    res = if (ptype == "response")
      factor(rep(NA_character_, n), levels = levs)
    else
      matrix(NA_real_, nrow = n, ncol = length(levs), dimnames = list(NULL, levs))
  } else if (type == "regr") {
    res = if (ptype == "response")
      rep(NA_real_, n)
    else
      matrix(NA_real_, nrow = n, ncol = 2L, dimnames = list(NULL, c("response", "se")))
  } else if (type == "fcregr") {
    res = if (ptype == "response")
      rep(NA_real_, n)
    else
      matrix(NA_real_, nrow = n, ncol = 2L, dimnames = list(NULL, c("response", "quantile")))
  } else if (type == "surv") {
    if (ptype == "response")
      res = rep.int(NA_real_, n)
    else
      stop("Predict type 'prob' for survival not yet supported")
  } else if (type == "costsens") {
    levs = model$task.desc$class.levels
    res = factor(rep(NA_character_, n), levels = levs)
  } else if (type == "cluster") {
    res = rep(NA_character_, n)
  }
  return(res)
}

#' @export
print.FailureModel = function(x, ...) {
  print.WrappedModel(x)
  catf("Training failed: %s", getFailureModelMsg(x))
}

#' @export
isFailureModel.FailureModel = function(model) {
  return(TRUE)
}

#' @export
getFailureModelMsg.FailureModel = function(model) {
  return(as.character(model$learner.model))
}

#' @export
getFailureModelDump.FailureModel = function(model) {
  return(model$dump)
}

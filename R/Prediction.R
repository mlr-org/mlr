#' @title Prediction object.
#'
#' @description
#' Result from \code{\link{predict.WrappedModel}}.
#' Use \code{as.data.frame} to access all information in a convenient format.
#' The function \code{\link{getPredictionProbabilities}} is useful to access predicted probabilities.
#'
#' The \code{data} member of the object contains always the following columns:
#' \code{id}, index numbers of predicted cases from the task, \code{response}
#' either a numeric or a factor, the predicted response values, \code{truth},
#' either a numeric or a factor, the true target values.
#' If probabilities were predicted, as many numeric columns as there were classes named
#' \code{prob.classname}. If standard errors were predicted, a numeric column named \code{se}.
#'
#'
#' Object members:
#' \describe{
#' \item{predict.type [\code{character(1)}]}{Type set in \code{\link{setPredictType}}.}
#' \item{data [\code{data.frame}]}{See details.}
#' \item{threshold [\code{numeric(1)}]}{Threshold set in predict function.}
#' \item{task.desc [\code{\link{TaskDesc}}]}{Task description object.}
#' \item{time [\code{numeric(1)}]}{Time learner needed to generate predictions.}
#' }
#' @name Prediction
#' @rdname Prediction
NULL

makePrediction = function(task.desc, row.names, id, truth, predict.type, predict.threshold = NULL, y, time) {
  UseMethod("makePrediction")
}

#' @export
makePrediction.TaskDescRegr = function(task.desc, row.names, id, truth, predict.type, predict.threshold = NULL, y, time) {
  data = namedList(c("id", "truth", "response", "se"))
  data$id = id
  data$truth = truth
  if (predict.type == "response") {
    data$response = y
  } else {
    data$response = y[, 1L]
    data$se = y[, 2L]
  }

  makeS3Obj(c("PredictionRegr", "Prediction"),
    predict.type = predict.type,
    data = setRowNames(as.data.frame(filterNull(data)), row.names),
    threshold = NA_real_,
    task.desc = task.desc,
    time = time
  )
}

#' @export
makePrediction.TaskDescClassif = function(task.desc, row.names, id, truth, predict.type, predict.threshold = NULL, y, time) {
  data = namedList(c("id", "truth", "response", "prob"))
  data$id = id
  # truth can come from a simple "newdata" df. then there might not be all factor levels present
  if (!is.null(truth))
    levels(truth) = union(levels(truth), task.desc$class.levels)
  data$truth = truth
  if (predict.type == "response") {
    data$response = y
    data = as.data.frame(filterNull(data))
  } else {
    data$prob = y
    data = as.data.frame(filterNull(data))
    # fix columnnames for prob if strange chars are in factor levels
    i = grep("prob.", names(data), fixed = TRUE)
    if (length(i))
      names(data)[i] = paste0("prob.", colnames(y))
  }

  p = makeS3Obj(c("PredictionClassif", "Prediction"),
    predict.type = predict.type,
    data = setRowNames(data, row.names),
    threshold = NA_real_,
    task.desc = task.desc,
    time = time
  )

  if (predict.type == "prob") {
    # set default threshold to 1/k
    if (is.null(predict.threshold)) {
      predict.threshold = rep(1/length(task.desc$class.levels), length(task.desc$class.levels))
      names(predict.threshold) = task.desc$class.levels
    }
    p = setThreshold(p, predict.threshold)
  }
  return(p)
}

#' @export
makePrediction.TaskDescMultilabel = function(task.desc, row.names, id, truth, predict.type, predict.threshold = NULL, y, time) {
  data = namedList(c("id", "truth", "response", "prob"))
  data$id = id
  data$truth = truth
  if (predict.type == "response") {
    data$response = y
  } else {
    data$prob = y
  }

  p = makeS3Obj(c("PredictionMultilabel", "Prediction"),
    predict.type = predict.type,
    data = setRowNames(as.data.frame(filterNull(data)), row.names),
    threshold = NA_real_,
    task.desc = task.desc,
    time = time
  )
  if (predict.type == "prob") {
    # set default threshold to 0.5
    if (is.null(predict.threshold)) {
      predict.threshold = rep(0.5, length(task.desc$class.levels))
      names(predict.threshold) = task.desc$class.levels
    }
    p = setThreshold(p, predict.threshold)
  }
  return(p)
}

#' @export
makePrediction.TaskDescSurv = function(task.desc, row.names, id, truth, predict.type, predict.threshold = NULL, y, time) {
  data = namedList(c("id", "truth.time", "truth.event", "response"))
  data$id = id
  # FIXME: recode times
  data$truth.time = truth[, 1L]
  data$truth.event = truth[, 2L]
  data$response = y

  makeS3Obj(c("PredictionSurv", "Prediction"),
    predict.type = predict.type,
    data = setRowNames(as.data.frame(filterNull(data)), row.names),
    threshold = NA_real_,
    task.desc = task.desc,
    time = time
  )
}

#' @export
makePrediction.TaskDescCluster = function(task.desc, row.names, id, truth, predict.type, predict.threshold = NULL, y, time) {
  data = namedList(c("id", "response", "prob"))
  data$id = id
  if (predict.type == "response") {
    data$response = y
    data = as.data.frame(filterNull(data))
  } else {
    # this is a bit uncool, but as long we only use cl_predict we are OK I guess
    class(y) = "matrix"
    data$prob = y
    data$response = getMaxIndexOfRows(y)
    data = as.data.frame(filterNull(data))
  }
  p = makeS3Obj(c("PredictionCluster", "Prediction"),
    predict.type = predict.type,
    data = setRowNames(data, row.names),
    threshold = NA_real_,
    task.desc = task.desc,
    time = time
  )
  return(p)
}

#' @export
makePrediction.TaskDescCostSens = function(task.desc, row.names, id, truth, predict.type, predict.threshold = NULL, y, time) {
  data = namedList(c("id", "response"))
  data$id = id
  data$response = y

  makeS3Obj(c("PredictionCostSens", "Prediction"),
    predict.type = predict.type,
    data = setRowNames(as.data.frame(filterNull(data)), row.names),
    threshold = NA_real_,
    task.desc = task.desc,
    time = time
  )
}

#' @export
print.Prediction = function(x, ...) {
  catf("Prediction: %i observations", nrow(x$data))
  catf("predict.type: %s", x$predict.type)
  catf("threshold: %s", collapse(sprintf("%s=%.2f", names(x$threshold), x$threshold)))
  catf("time: %.2f", x$time)
  print(head(as.data.frame(x)))
}


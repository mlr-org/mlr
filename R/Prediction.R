#' Prediction object.
#'
#' Result from \code{\link{predict.WrappedModel}}.
#' Use \code{as.data.frame} to access all information in a convenient format.
#' The function \code{\link{getProbabilities}} is useful to access predicted probabilities.
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

makePrediction = function(task.desc, id, truth, predict.type, y, time) {
	data = list()
	# if null no col in data present
	data[["id"]] = id
	data[["truth"]] = truth
  if (predict.type == "response") {
    data[["response"]] = y
  } else if (predict.type == "prob") {
		data[["prob"]] = y
  } else if (predict.type == "se"){
    data[["response"]] = y[,1L]
    data[["se"]] = y[,2L]
  }
  data = as.data.frame(data)
  # fix columnnames for prob if strage chars are in factor levels
	i = grep("prob.", colnames(data))
	if (length(i))
		colnames(data)[i] = paste("prob.", colnames(y), sep="")

  p = structure(list(
    predict.type = predict.type,
    data = data,
    threshold = NA_real_,
    task.desc = task.desc,
    time = time
  ), class="Prediction")

  if (predict.type == "prob") {
    th = rep(1/length(task.desc$class.levels), length(task.desc$class.levels))
    names(th) = task.desc$class.levels
    p = setThreshold(p, th)
  }
  return(p)
}

#' @S3method print Prediction
print.Prediction = function(x, ...) {
  catf("Prediction:")
  catf("predict.type: %s", x$predict.type)
  catf("threshold: %s", collapse(sprintf("%s=%.2f", names(x$threshold), x$threshold)))
  catf("time: %.2f", x$time)
  catf(printStrToChar(as.data.frame(x)))
}


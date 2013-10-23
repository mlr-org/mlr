\name{Prediction}
\alias{Prediction}
\title{Prediction object.}
\description{
  Result from \code{\link{predict}}. Use
  \code{as.data.frame} to access all information in a
  convenient format. The function
  \code{\link{getProbabilities}} is useful to access
  predicted probabilities..
}
\details{
  The \code{data} member of the object contains always the
  following columns: \code{id}, index numbers of predicted
  cases from the task, \code{response} either a numeric or
  a factor, the predicted response values, \code{truth},
  either a numeric or a factor, the true target values. If
  probabilities were predicted, as many numeric columns as
  there were classes named \code{prob.classname}. If
  standard errors were predicted, a numeric column named
  \code{se}.

  Object members: \describe{ \item{predict.type
  [\code{character(1)}]}{Type set in
  \code{\link{setPredictType}}.} \item{data
  [\code{data.frame}]}{See details.} \item{threshold
  [\code{numeric(1)}]}{Threshold set in predict function.}
  \item{task.desc [\code{\link{TaskDesc}}]}{Task
  description object.} \item{time [\code{numeric(1)}]}{Time
  learner needed to generate predictions.} }
}


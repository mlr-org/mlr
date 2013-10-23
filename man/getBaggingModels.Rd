\name{getBaggingModels}
\alias{getBaggingModels}
\title{Returns the list of models fitted in bagging.}
\usage{
  getBaggingModels(model, learner.models = FALSE)
}
\arguments{
  \item{model}{[\code{\link[mlr]{WrappedModel}}]\cr Model
  produced by training a bagging learner.}

  \item{learner.models}{[\code{logical(1)}]\cr Return
  underlying R models (e.g., rpart models) or wrapped mlr
  models (\code{\link[mlr]{WrappedModel}}).  Default is
  \code{FALSE}.}
}
\value{
  [\code{list}].
}
\description{
  Returns the list of models fitted in bagging.
}


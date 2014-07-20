\name{getCostSensRegrModels}
\alias{getCostSensRegrModels}
\title{Returns the list of fitted models.}
\usage{
getCostSensRegrModels(model, learner.models = FALSE)
}
\arguments{
  \item{model}{[\code{\link[mlr]{WrappedModel}}]\cr Model
  produced by training a cost-sensitive regression
  learner.}

  \item{learner.models}{[\code{logical(1)}]\cr Return
  underlying R models or wrapped mlr models
  (\code{\link[mlr]{WrappedModel}}).  Default is
  \code{FALSE}.}
}
\value{
[\code{list}].
}
\description{
Returns the list of fitted models.
}


\name{getTuneResult}
\alias{getTuneResult}
\title{Returns the optimal hyperparameters and optimization path.}
\usage{
  getTuneResult(model)
}
\arguments{
  \item{model}{[\code{\link[mlr]{WrappedModel}}]\cr Trained
  Model created with \code{\link{makeTuneWrapper}}.}
}
\value{
  [\code{\link{TuneResult}}].
}
\description{
  Returns the optimal hyperparameters and optimization
  path.
}


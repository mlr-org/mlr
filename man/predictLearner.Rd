\name{predictLearner}
\alias{predictLearner}
\title{Predict new data with an R learner.}
\usage{
predictLearner(.learner, .model, .newdata, ...)
}
\arguments{
  \item{.learner}{[\code{\link{RLearner}}]\cr Wrapped
  learner.}

  \item{.model}{[\code{\link{WrappedModel}}]\cr Model
  produced by training.}

  \item{.newdata}{[\code{data.frame}]\cr New data to
  predict. Does not include target column.}

  \item{...}{[any]\cr Additional parameters, which need to
  be passed to the underlying predict function.}
}
\value{
For classification: Either a factor for type \dQuote{response} or a matrix for
  type \dQuote{prob}. In the latter case the columns must be named with the class labels.
  For regressions: Either a numeric for type \dQuote{response} or a matrix with two columns
  for type \dQuote{se}. In the latter case first column is the estimated response (mean value)
  and the second column the estimated standard errors.
}
\description{
Mainly for internal use. Predict new data with a fitted model.
You have to implement this method if you want to add another learner to this package.
}
\details{
You implementation must adhere to the following:
The model must be fitted on the subset of \code{.task} given by \code{.subset}. All parameters
in \code{...} must be passed to the underlying training function.
}


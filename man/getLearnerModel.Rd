\name{getLearnerModel}
\alias{getLearnerModel}
\title{Get underlying R model of learner integrated into mlr.}
\usage{
getLearnerModel(model)
}
\arguments{
  \item{model}{[\code{\link{WrappedModel}}]\cr The model,
  returned by e.g., \code{\link{train}}.}
}
\value{
[any]. A fitted model, depending the learner / wrapped
package. E.g., a model of class \code{\link[rpart]{rpart}}
for learner \dQuote{classif.rpart}.
}
\description{
Get underlying R model of learner integrated into mlr.
}


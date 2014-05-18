\name{makeMeasure}
\alias{Measure}
\alias{makeMeasure}
\title{Construct performance measure.}
\usage{
makeMeasure(id, minimize, classif = FALSE, regr = FALSE, costsens = FALSE,
  only.binary = FALSE, allowed.pred.types = character(0L), fun,
  extra.args = list(), aggr = test.mean)
}
\arguments{
  \item{id}{[\code{character(1)}]\cr Name of measure.}

  \item{minimize}{[\code{logical(1)}]\cr Should the measure
  be minimized?  Default is in \code{TRUE}.}

  \item{classif}{[\code{logical(1)}]\cr Is the measure
  applicable for classification?  Default is \code{FALSE}.}

  \item{regr}{[\code{logical(1)}]\cr Is the measure
  applicable for regression?  Default is \code{FALSE}.}

  \item{costsens}{[\code{logical(1)}]\cr Is the measure
  applicable for cost-sensitive learning?  Default is
  \code{FALSE}.}

  \item{only.binary}{[\code{logical(1)}]\cr Is the measure
  only applicable to binary classification?  Only
  reasonable if \code{classif} is \code{TRUE}.  Default is
  \code{FALSE}.}

  \item{allowed.pred.types}{[\code{character}]\cr Which
  prediction types are allowed for this measure?  Subset of
  \dQuote{response},\dQuote{prob}, \dQuote{se}.  Default is
  \code{character(0)}.}

  \item{fun}{[\code{function(task, model, pred,
  extra.args)}]\cr Calculates performance value.}

  \item{extra.args}{[\code{list}]\cr List of extra
  arguments which will always be passed to \code{fun}.
  Default is empty list.}

  \item{aggr}{[\code{\link{Aggregation}}]\cr Aggregation
  funtion, which is used to aggregate the values measured
  on test / training sets of the measure to a single value.
  Default is \code{\link{test.mean}}.}
}
\value{
[\code{\link{Measure}}].
}
\description{
A measure object encapsulates a function to evaluate the performance of a prediction.
Information about already implemented measures can be obtained here: \code{\link{measures}}.
}
\details{
A learner is trained on a a training set d1, results in a model m, predicts another set d2
(which may be a different one or the training set), resulting in the prediction.
The performance measure can now be defined using all of the information of the original task,
the fitted model and the prediction.

Object slots:
\describe{
\item{id [\code{character(1)}]}{See argument.}
\item{minimize [\code{logical(1)}]}{See argument.}
\item{classif [\code{logical(1)}]}{See argument.}
\item{regr [\code{logical(1)}]}{See argument.}
\item{only.binary [\code{logical(1)}]}{See argument.}
\item{allowed.pred.types [\code{character}]}{See argument.}
\item{req.pred [\code{logical(1)}]}{Is prediction object required in calculation?}
\item{req.task [\code{logical(1)}]}{Is task object required in calculation?.}
\item{req.model [\code{logical(1)}]}{Is model object required in calculation?}
\item{fun [\code{function}]}{See argument.}
\item{extra.args [\code{list}]}{See argument.}
\item{aggr [\code{\link{Aggregation}}]}{See argument.}.
}
}
\examples{
f <- function(task, model, pred, extra.args)
  sum((pred$data$response - pred$data$truth)^2)
makeMeasure(id = "my.sse", minimize = TRUE, regr = TRUE, allowed.pred.types = "response", fun = f)
}


\name{resample}
\alias{resample}
\title{Fit models according to a resampling strategy.}
\usage{
resample(learner, task, resampling, measures, weights, models = FALSE,
  extract, show.info = TRUE)
}
\arguments{
  \item{learner}{[\code{\link{Learner}}]\cr The learner.}

  \item{task}{[\code{\link{SupervisedTask}}]\cr The task.}

  \item{resampling}{[\code{\link{ResampleDesc}} or
  \code{\link{ResampleInstance}}]\cr Resampling strategy.
  If a description is passed, it is instantiated
  automatically.}

  \item{measures}{[\code{\link{Measure}} | list of
  \code{\link{Measure}}]\cr Performance measure(s) to
  evaluate.}

  \item{weights}{[\code{numeric}]\cr Optional, non-negative
  case weight vector to be used during fitting.  If given,
  must be of same length as observations in task and in
  corresponding order.  Overwrites weights specified in the
  \code{task}.  By default missing which means no weights
  are used unless specified in the task.}

  \item{models}{[\code{logical(1)}]\cr Should all fitted
  models be returned?  Default is \code{FALSE}.}

  \item{extract}{[\code{function(model)}]\cr Function used
  to extract information from a fitted model during
  resampling.  Is applied to every
  \code{\link{WrappedModel}} resulting from calls to
  \code{\link{train}} during resampling.  Default is to
  extract nothing.}

  \item{show.info}{[\code{logical(1)}]\cr Should a few
  informative lines about the current resampling iteration
  and the result be logged to the R console?  Default is
  \code{TRUE}.}
}
\value{
List of: \item{measures.test [\code{data.frame}]}{Rows
correspond to test sets in resampling iterations, columns
to performance measures.} \item{measures.train
[\code{data.frame}]}{Rows correspond to training sets in
resampling iterations, columns to performance measures.}
\item{aggr [\code{numeric}]}{Named vector of aggregated
performance values. Names are coded like this
<measure>.<aggregation>.} \item{pred
[\code{\link{ResamplePrediction}}]}{Container for all
predictions during resampling.} \item{models [list of
\code{\link{WrappedModel}}]}{List of fitted models or
\code{NULL}.} \item{extract [\code{list}]}{List of
extracted parts from fitted models or \code{NULL}.}
}
\description{
Given a resampling strategy, which defines sets of training
and test indices, fits the selected learner using the
training sets and performs predictions for the
training/test sets. This depends on what you selected in
the resampling strategy, see parameter \code{predict} in
\code{\link{makeResampleDesc}}.

Then performance measures are calculated on all respective
data sets and aggregated.

You are able to return all fitted models (parameter
\code{models}) or extract specific parts of the models
(parameter \code{extract}) as returning all of them
completely might be memory intensive.

For construction of the resampling strategies use the
factory methods \code{\link{makeResampleDesc}} and
\code{\link{makeResampleInstance}}.
}
\examples{
task <- makeClassifTask(data = iris, target = "Species")
rdesc <- makeResampleDesc("CV", iters = 2)
r <- resample(makeLearner("classif.qda"), task, rdesc)
print(r$aggr)
print(r$measures.test)
print(r$pred)
}
\seealso{
\code{\link{makeResampleDesc}},
\code{\link{makeResampleInstance}}
}


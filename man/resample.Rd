\name{crossval}
\alias{bootstrapB632}
\alias{bootstrapB632plus}
\alias{bootstrapOOB}
\alias{crossval}
\alias{holdout}
\alias{repcv}
\alias{resample}
\alias{subsample}
\title{Fit models according to a resampling strategy.}
\usage{
crossval(learner, task, iters = 10L, stratify = FALSE, measures,
  models = FALSE, ..., show.info = TRUE)

repcv(learner, task, folds = 10L, reps = 10L, stratify = FALSE, measures,
  models = FALSE, ..., show.info = TRUE)

holdout(learner, task, split = 2/3, stratify = FALSE, measures,
  models = FALSE, ..., show.info = TRUE)

subsample(learner, task, iters = 30, split = 2/3, stratify = FALSE,
  measures, models = FALSE, ..., show.info = TRUE)

bootstrapOOB(learner, task, iters = 30, stratify = FALSE, measures,
  models = FALSE, ..., show.info = TRUE)

bootstrapB632(learner, task, iters = 30, stratify = FALSE, measures,
  models = FALSE, ..., show.info = TRUE)

bootstrapB632plus(learner, task, iters = 30, stratify = FALSE, measures,
  models = FALSE, ..., show.info = TRUE)

resample(learner, task, resampling, measures, weights = NULL,
  models = FALSE, extract, show.info = TRUE)
}
\arguments{
  \item{learner}{[\code{\link{Learner}} |
  \code{character(1)}]\cr The learner.  If you pass a
  string the learner will be created via
  \code{\link{makeLearner}}.}

  \item{task}{[\code{\link{SupervisedTask}}]\cr The task.}

  \item{resampling}{[\code{\link{ResampleDesc}} or
  \code{\link{ResampleInstance}}]\cr Resampling strategy.
  If a description is passed, it is instantiated
  automatically.}

  \item{iters}{[\code{integer(1)}]\cr See
  \code{\link{ResampleDesc}}.}

  \item{folds}{[\code{integer(1)}]\cr See
  \code{\link{ResampleDesc}}.}

  \item{reps}{[\code{integer(1)}]\cr See
  \code{\link{ResampleDesc}}.}

  \item{split}{[\code{numeric(1)}]\cr See
  \code{\link{ResampleDesc}}.}

  \item{stratify}{[\code{logical(1)}]\cr See
  \code{\link{ResampleDesc}}.}

  \item{measures}{[\code{\link{Measure}} | list of
  \code{\link{Measure}}]\cr Performance measure(s) to
  evaluate.}

  \item{weights}{[\code{numeric}]\cr Optional, non-negative
  case weight vector to be used during fitting.  If given,
  must be of same length as observations in task and in
  corresponding order.  Overwrites weights specified in the
  \code{task}.  By default \code{NULL} which means no
  weights are used unless specified in the task.}

  \item{models}{[\code{logical(1)}]\cr Should all fitted
  models be returned?  Default is \code{FALSE}.}

  \item{extract}{[\code{function(model)}]\cr Function used
  to extract information from a fitted model during
  resampling.  Is applied to every
  \code{\link{WrappedModel}} resulting from calls to
  \code{\link{train}} during resampling.  Default is to
  extract nothing.}

  \item{...}{[any]\cr Further hyperparameters passed to
  \code{learner}.}

  \item{show.info}{[\code{logical(1)}]\cr Verbose output on
  console?  Default is \code{TRUE}.}
}
\value{
List of: \item{measures.test [\code{data.frame}]}{Gives you
access to performance measurements on the individual test
sets. Rows correspond to sets in resampling iterations,
columns to performance measures.} \item{measures.train
[\code{data.frame}]}{Gives you access to performance
measurements on the individual training sets. Rows
correspond to sets in resampling iterations, columns to
performance measures. Usually not available, only if
specifically requested, see general description above.}
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
\code{resample}: Given a resampling strategy, which defines
sets of training and test indices, fits the selected
learner using the training sets and performs predictions
for the training/test sets. This depends on what you
selected in the resampling strategy, see parameter
\code{predict} in \code{\link{makeResampleDesc}}.

Then performance measures are calculated on all respective
data sets and aggregated.

You are able to return all fitted models (parameter
\code{models}) or extract specific parts of the models
(parameter \code{extract}) as returning all of them
completely might be memory intensive.

For construction of the resampling strategies use the
factory methods \code{\link{makeResampleDesc}} and
\code{\link{makeResampleInstance}}.

The remaining functions on this page are convenience
wrappers for the various existing resampling strategies.
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


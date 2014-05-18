\name{selectFeatures}
\alias{selectFeatures}
\title{Feature selection by wrapper approach.}
\usage{
selectFeatures(learner, task, resampling, control, measures, bit.names,
  bits.to.features, show.info = TRUE)
}
\arguments{
  \item{learner}{[\code{\link{Learner}}]\cr The learner.}

  \item{task}{[\code{\link{SupervisedTask}}]\cr The task.}

  \item{resampling}{[\code{\link{ResampleInstance}} |
  \code{\link{ResampleDesc}}]\cr Resampling strategy to
  feature sets. If you pass a description, it is
  instantiated once at the beginning by default, so all
  points are evaluated on the same training/test sets.  If
  you want to change that behaviour, look at
  \code{\link{FeatSelControl}}.}

  \item{control}{[see \code{\link{FeatSelControl}}] Control
  object for search method. Also selects the optimization
  algorithm for feature selection.}

  \item{measures}{[list of \code{\link{Measure}}]\cr
  Performance measures to evaluate. The first measure,
  aggregated by the first aggregation function is optimized
  during selection, others are simply evaluated.}

  \item{bit.names}{[character]\cr Names of bits encoding
  the solutions. Also defines the total number of bits in
  the encoding.  Per default these are the feature names of
  the task.}

  \item{bits.to.features}{[function(x, task)]\cr Function
  which transforms an integer-0-1 vector into a character
  vector of selected features.  Per default a value of 1 in
  the ith bit selects the ith feature to be in the
  candidate solution.}

  \item{show.info}{[\code{logical(1)}]\cr A logical value,
  indicating whether information should be printed.  The
  default is TRUE.}
}
\value{
[\code{\link{FeatSelResult}}].
}
\description{
Optimizes the features for a classification or regression problem by choosing a variable selection wrapper approach.
Allows for different optimization methods, such as forward search or a genetic algorithm.
You can select such an algorithm (and its settings)
by passing a corresponding control object. For a complete list of implemented algorithms look at the
subclasses of [\code{\link{FeatSelControl}}].
}
\details{
All algorithms operate on a 0-1-bit encoding of candidate solutions. Per default a single bit corresponds
to a single feature, but you are able to change this by using the arguments \code{bit.names}
and \code{bits.to.features}. Thus allowing you to switch on whole groups of features with a single bit.
}
\examples{
task <- makeClassifTask(data=iris, target="Species")
lrn <- makeLearner("classif.rpart")
rdesc <- makeResampleDesc("Holdout")

## Now create control-objects for each of the possible feature selection algorithms:
ctrlSeq <- makeFeatSelControlSequential(method="sfs", maxit=NA)
ctrlGA <- makeFeatSelControlGA(maxit=5, max.features=NA, crossover.rate=0.5,
  mutation.rate=0.1, mu=10, lambda=5)
ctrlRand <- makeFeatSelControlRandom(maxit=10, max.features=NA, prob=0.5)
ctrlExh <- makeFeatSelControlExhaustive(maxit=NA, max.features=NA)

## Let's run the feature selction algorithm:

sfSeq <- selectFeatures(lrn, task, rdesc, control=ctrlSeq)
sfSeq
sfGA <- selectFeatures(lrn, task, rdesc, control=ctrlGA)
sfGA
sfRand <- selectFeatures(lrn, task, rdesc, control=ctrlRand)
sfRand
sfExh <- selectFeatures(lrn, task, rdesc, control=ctrlExh)
sfExh
}


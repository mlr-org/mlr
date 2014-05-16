\name{makeBaggingWrapper}
\alias{makeBaggingWrapper}
\title{Fuse learner with the bagging technique.}
\usage{
makeBaggingWrapper(learner, bag.iters = 10L, bag.replace = TRUE, bag.size,
  bag.feats = 1, predict.type = "response")
}
\arguments{
  \item{learner}{[\code{\link{Learner}}]\cr The learner.
  Prediction type of the basic learner must be
  \dQuote{response}.}

  \item{bag.iters}{[\code{integer(1)}]\cr Iterations =
  number of fitted models in bagging.  Default is 10.}

  \item{bag.replace}{[\code{logical(1)}]\cr Sample bags
  with replacement (bootstrapping)?  Default is TRUE.}

  \item{bag.size}{[\code{numeric(1)}]\cr Percentage size of
  sampled bags.  Default is 1 for bootstrapping and 0.632
  for subsampling.}

  \item{bag.feats}{[\code{numeric(1)}]\cr Percentage size
  of randomly selected features in bags.  Default is 1.}

  \item{predict.type}{[\code{character(1)}]\cr
  Classification: \dQuote{response} (= labels) or
  \dQuote{prob} (= probabilities and labels by selecting
  the ones with maximal probability).  Regression:
  \dQuote{response} (= mean response) or \dQuote{se} (=
  standard errors and mean response).  Default is
  \dQuote{response}.}
}
\value{
[\code{\link{Learner}}].
}
\description{
Fuses a learner with the bagging method (i.e., similar to
what a \code{randomForest} does). Creates a learner object,
which can be used like any other learner object. Models can
easily be accessed via \code{\link{getBaggingModels}}.
}
\details{
Bagging is implemented as follows: For each iteration a
random data subset is sampled (with or without replacement)
and potentially the number of features is also restricted
to a random subset. Note that this is usually handled in a
slightly different way in the random forest where features
are sampled at each tree split).

Prediction works as follows: For classification we do
majority voting to create a discrete label and
probabilities are predicted by considering the proportions
of all predicted labels. For regression the mean value and
the standard deviations across predictions is computed.
}


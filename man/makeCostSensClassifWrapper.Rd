\name{makeCostSensClassifWrapper}
\alias{makeCostSensClassifWrapper}
\title{Wraps a classification learner for use in cost-sensitive learning.}
\usage{
makeCostSensClassifWrapper(learner)
}
\arguments{
  \item{learner}{[\code{\link[mlr]{Learner}}]\cr The basic
  classification learner.}
}
\value{
[\code{\link[mlr]{Learner}}].
}
\description{
Creates a wrapper, which can be used like any other learner
object. The classification model can easily be accessed via
\code{\link{getCostSensClassifModel}}.

This is a very naive learner, where the costs are
transformed into classification labels - the label for each
case is the name of class with minimal costs. Then the
classifier is fitted to that data and subsequently used for
prediction.
}


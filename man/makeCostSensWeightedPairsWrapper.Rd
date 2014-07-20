\name{makeCostSensWeightedPairsWrapper}
\alias{CostSensWeightedPairsModel}
\alias{CostSensWeightedPairsWrapper}
\alias{makeCostSensWeightedPairsWrapper}
\title{Wraps a classifier for cost-sensitive learning to produce a weighted pairs model.}
\usage{
makeCostSensWeightedPairsWrapper(learner)
}
\arguments{
  \item{learner}{[\code{\link{Learner}} |
  \code{character(1)}]\cr The classification learner.  If
  you pass a string the learner will be created via
  \code{\link{makeLearner}}.}
}
\value{
[\code{\link[mlr]{Learner}}].
}
\description{
Creates a wrapper, which can be used like any other learner object.
Models can easily be accessed via \code{\link{getCostSensWeightedPairsModels}}.

For each pair of labels, we fit a binary classifier.
For each observation we define the label to be the element of the pair with minimal costs.
During fitting, we also weight the observation with the absolute difference in costs.
Prediction is performed by simple voting.

This approach is sometimes called cost-sensitive one-vs-one (CS-OVO),
because it is obviously very similar to the
one-vs-one approach where one reduces a normal multi-class problem to
multiple binary ones and aggregates by voting.
}
\seealso{
Other costsens: \code{\link{ClassifTask}},
  \code{\link{ClusterTask}}, \code{\link{CostSensTask}},
  \code{\link{RegrTask}}, \code{\link{SurvTask}},
  \code{\link{Task}}, \code{\link{makeClassifTask}},
  \code{\link{makeClusterTask}},
  \code{\link{makeCostSensTask}},
  \code{\link{makeRegrTask}}, \code{\link{makeSurvTask}};
  \code{\link{CostSensClassifModel}},
  \code{\link{CostSensClassifWrapper}},
  \code{\link{makeCostSensClassifWrapper}};
  \code{\link{CostSensRegrModel}},
  \code{\link{CostSensRegrWrapper}},
  \code{\link{makeCostSensRegrWrapper}}
}


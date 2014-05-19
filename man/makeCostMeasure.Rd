\name{makeCostMeasure}
\alias{makeCostMeasure}
\title{Creates a measure for non-standard misclassification costs.}
\usage{
makeCostMeasure(id = "costs", minimize = TRUE, costs, task,
  aggregate = mean)
}
\arguments{
  \item{id}{[\code{character(1)}]\cr Name of measure.
  Default is \dQuote{costs}.}

  \item{minimize}{[\code{logical(1)}]\cr Should the measure
  be minimized? Default is TRUE. Otherwise you are
  effectively specifying a benefits matrix.}

  \item{costs}{[\code{matrix}]\cr Matrix of
  misclassification costs. Rows and columns have to be
  named with class labels, order does not matter.  Rows
  indicate true classes, columns predicted classes.}

  \item{task}{[\code{\link{ClassifTask}}]\cr Classification
  task. Has to be passed, so validity of matrix names can
  be checked.}

  \item{aggregate}{[\code{logical(1)}]\cr How to aggregate
  costs over all cases in one test set prediction?  Default
  is \code{\link{mean}}.}
}
\value{
[\code{\link{Measure}}].
}
\description{
Creates a measure for non-standard misclassification costs.
}
\seealso{
\code{\link{measures}}, \code{\link{makeMeasure}}
}


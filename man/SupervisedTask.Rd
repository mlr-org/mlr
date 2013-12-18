\name{makeClassifTask}
\alias{ClassifTask}
\alias{makeClassifTask}
\alias{makeRegrTask}
\alias{RegrTask}
\alias{SupervisedTask}
\title{Create a classification / regression task for a given data set.}
\usage{
  makeClassifTask(id, data, target, weights, blocking,
    positive, check.data = TRUE)

  makeRegrTask(id, data, target, weights, blocking,
    check.data = TRUE)
}
\arguments{
  \item{id}{[\code{character(1)}]\cr Id string for object.
  Default is the name of R variable passed to \code{data}.}

  \item{data}{[\code{data.frame}]\cr A data frame
  containing the features and target variable.}

  \item{target}{[\code{character(1)}]\cr Name of the target
  variable.}

  \item{weights}{[\code{numeric}]\cr Optional, non-negative
  case weight vector to be used during fitting.}

  \item{blocking}{[\code{factor}]\cr An optional factor of
  the same length as the number of observations.
  Observations with the same blocking level \dQuote{belong
  together}.  Specifically, they are either put all in the
  training or the test set during a resampling iteration.
  Default is no blocking.}

  \item{positive}{[\code{character(1)}]\cr Positive class
  for binary classification.  Default is the first factor
  level of the target attribute.}

  \item{check.data}{[\code{logical(1)}]\cr Should sanity of
  data be checked initially at task creation?  You should
  have good reasons to turn this off.  Default is
  \code{TRUE}}
}
\value{
  [\code{\link{SupervisedTask}}].
}
\description{
  The task encapsulates the data and specifies - through
  its subclasses - the type of the task (either
  classification or regression), and contains a description
  object detailing further aspects of the data.
}
\details{
  Useful operators are: \code{\link{getTaskFormula}},
  \code{\link{getTaskFormulaAsString}},
  \code{\link{getTaskFeatureNames}},
  \code{\link{getTaskData}}, \code{\link{getTaskTargets}},
  \code{\link{subsetTask}}.

  Object members: \describe{ \item{env
  [\code{environment}]}{Environment where data for the task
  are stored. Use \code{\link{getTaskData}} in order to
  access it.} \item{blocking [\code{factor}]}{See argument
  above. \code{factor(0)} if not present.} \item{task.desc
  [\code{\link{TaskDesc}}]}{Encapsulates further
  information about the task.} }
}
\examples{
library(mlbench)
data(BostonHousing)
RegrTask <- makeRegrTask(data = BostonHousing, target = "medv")

ClassifTask <- makeClassifTask(data = iris, target = "Species")

## an example of a classification task with more than those standard arguments:
library(mlbench)
data(Ionosphere)
blocks <- factor(c(rep(1, 51), rep(2, 300)))
ClassifTask <- makeClassifTask(id = "myIonosphere", data = Ionosphere, target = "Class",
                               positive = "good", blocking = blocks)
}


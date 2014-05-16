\name{getTaskFeatureNames}
\alias{getTaskFeatureNames}
\title{Get feature names of task.}
\usage{
getTaskFeatureNames(task)
}
\arguments{
  \item{task}{[\code{\link{SupervisedTask}}]\cr The task.}
}
\value{
[\code{character}].
}
\description{
Target column name is not included.
}
\examples{
task = makeClassifTask(data = iris, target = "Species")
getTaskFeatureNames(task)
}


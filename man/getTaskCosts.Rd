\name{getTaskCosts}
\alias{getTaskCosts}
\title{Extract costs in task.}
\usage{
getTaskCosts(task, subset)
}
\arguments{
  \item{task}{[\code{\link{CostSensTask}}]\cr The task.}

  \item{subset}{[\code{integer}]\cr Selected cases.
  Default is all cases.}
}
\value{
[\code{matrix} | \code{NULL}].
}
\description{
Retuns \dQuote{NULL} if the task is not of type \dQuote{costsens}.
}


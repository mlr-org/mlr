\name{getTaskFormula}
\alias{getTaskFormula}
\title{Get formula of a task.}
\usage{
getTaskFormula(x, target = getTargetNames(x), env = NULL)
}
\arguments{
  \item{x}{[\code{\link{SupervisedTask}} |
  \code{\link{TaskDesc}}]\cr Task or its description
  object.}

  \item{target}{[\code{character(1)}]\cr Left hand side of
  formula.  Default is defined by task \code{x}.}

  \item{env}{[\code{environment}]\cr Environment of the
  formula. Set this to \code{parent.frame()} for the
  default behaviour.  Default is \code{NULL} which deletes
  the environment.}
}
\value{
[\code{formula}].
}
\description{
This is simply the \code{target ~ .} formula.
}
\examples{
task <- makeClassifTask(data = iris, target = "Species")
getTaskFormula(task)
}


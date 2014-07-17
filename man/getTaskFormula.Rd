\name{getTaskFormulaAsString}
\alias{getTaskFormula}
\alias{getTaskFormulaAsString}
\title{Get formula of a task.}
\usage{
getTaskFormulaAsString(x, target = getTargetNames(x))

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
[\code{formula} | \code{character(1)}].
}
\description{
This is simply \dQuote{<target> ~ .}.
}
\seealso{
Other task: \code{\link{getTaskCosts}};
  \code{\link{getTaskData}};
  \code{\link{getTaskFeatureNames}};
  \code{\link{getTaskNFeats}};
  \code{\link{getTaskTargets}}; \code{\link{subsetTask}}
}


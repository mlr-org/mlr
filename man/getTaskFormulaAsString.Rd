\name{getTaskFormulaAsString}
\alias{getTaskFormulaAsString}
\title{Get formula of a task as a string.}
\usage{
getTaskFormulaAsString(x, target = getTargetNames(x))
}
\arguments{
  \item{x}{[\code{\link{SupervisedTask}} |
  \code{\link{TaskDesc}}]\cr Task or its description
  object.}

  \item{target}{[\code{character(1)}]\cr Left hand side of
  formula.  Default is defined by task \code{x}.}
}
\value{
[\code{character(1)}].
}
\description{
This is simply \dQuote{<target> ~ .}.
}
\examples{
task <- makeClassifTask(data = iris, target = "Species")
getTaskFormulaAsString(task)
}


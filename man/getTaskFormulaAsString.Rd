\name{getTaskFormulaAsString}
\alias{getTaskFormulaAsString}
\title{Get formula of a task as a string.}
\usage{
  getTaskFormulaAsString(x)
}
\arguments{
  \item{x}{[\code{\link{SupervisedTask}} |
  \code{\link{TaskDesc}}]\cr Task or its description
  object.}
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


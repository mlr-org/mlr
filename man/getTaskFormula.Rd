\name{getTaskFormula}
\alias{getTaskFormula}
\title{Get formula of a task.}
\usage{
  getTaskFormula(x, delete.env = TRUE)
}
\arguments{
  \item{x}{[\code{\link{SupervisedTask}} |
  \code{\link{TaskDesc}}]\cr Task or its description
  object.}

  \item{delete.env}{[\code{delete.env}]\cr Delete
  enviroment attached to returned formula?  Don't ask why
  this option exists, R sucks.  Default is \code{TRUE}.}
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


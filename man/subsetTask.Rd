\name{subsetTask}
\alias{subsetTask}
\title{Subset data in task.}
\usage{
  subsetTask(task, subset, features)
}
\arguments{
  \item{task}{[\code{\link{SupervisedTask}}]\cr The task.}

  \item{subset}{[\code{integer}]\cr Selected cases.
  Default is all cases.}

  \item{features}{[character]\cr Selected inputs. Note that
  target feature is always included in the resulting task,
  you should not pass it here.  Default is all features.}
}
\value{
  [\code{\link{SupervisedTask}}]. Task with subsetted data.
}
\description{
  Subset data in task.
}
\examples{
task <- makeClassifTask(data = iris, target = "Species")
subsetTask(task, subset = 1:100)
}


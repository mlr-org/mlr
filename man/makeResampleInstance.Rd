\name{makeResampleInstance}
\alias{ResampleInstance}
\alias{makeResampleInstance}
\title{Instantiates a resampling strategy object.}
\usage{
makeResampleInstance(desc, task, size)
}
\arguments{
  \item{desc}{[\code{\link{ResampleDesc}}]\cr Resampling
  description object.}

  \item{task}{[\code{\link{SupervisedTask}}]\cr Data of
  task to resample from.  Prefer to pass this instead of
  \code{size}.}

  \item{size}{[\code{\link{integer}}]\cr Size of the data
  set to resample.  Can be used instead of \code{task}.}
}
\value{
[\code{\link{ResampleInstance}}].
}
\description{
This class encapsulates training and test sets generated
from the data set for a number of iterations. It mainly
stores a set of integer vectors indicating the training and
test examples for each iteration.
}
\details{
Object slots: \describe{ \item{desc
[\code{\link{ResampleDesc}}]}{See argument.} \item{size
[integer(1)]}{See argument.} \item{train.inds [list of
\code{integer}]}{List of of training indices for all
iterations.} \item{test.inds [list of \code{integer}]}{List
of of test indices for all iterations.} \item{group
[\code{factor}]}{Optional grouping of resampling
iterations. This encodes whether specfic iterations 'belong
together' (e.g. repeated CV), and it can later be used to
aggregate performance values accordingly. Default is
'factor()'.} }
}
\examples{
task <- makeClassifTask(data = iris, target = "Species")
rdesc <- makeResampleDesc("Bootstrap", iters = 10)
rin <- makeResampleInstance(rdesc, task = task)

# Alternatively provide the size argument instead of a task object
rdesc <- makeResampleDesc("CV", iters = 50)
rin <- makeResampleInstance(rdesc, size = nrow(iris))
}
\seealso{
\code{\link{makeResampleDesc}}, \code{\link{resample}}
}


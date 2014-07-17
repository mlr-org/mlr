\name{Task}
\alias{Task}
\title{Create a machine learning task.}
\arguments{
  \item{id}{[\code{character(1)}]\cr Id string for object.
  Default is the name of R variable passed to \code{data}.}

  \item{data}{[\code{data.frame}]\cr A data frame
  containing the features.}

  \item{costs}{[\code{data.frame}]\cr A numeric matrix or
  data frame containing the costs of misclassification.  We
  assume the general case of observation specific costs.
  This means we have n rows, corresponding to the
  observations, in the same order as \code{data}.  The
  columns correspond to classes and their names are the
  class labels (if unnamed we use y1 to yk as labels).
  Each entry (i,j) of the matrix specifies the cost of
  predicting class j for observation i.}

  \item{weights}{[\code{numeric}]\cr Optional, non-negative
  case weight vector to be used during fitting.  Cannot be
  set for cost-sensitive learning.  Default is \code{NULL}
  which means no (= equal) weights.}

  \item{blocking}{[\code{factor}]\cr An optional factor of
  the same length as the number of observations.
  Observations with the same blocking level \dQuote{belong
  together}.  Specifically, they are either put all in the
  training or the test set during a resampling iteration.
  Default is \code{NULL} which means no blocking.}

  \item{fixup.data}{[\code{character(1)}]\cr Should some
  basic cleaning up of data be performed?  Currently this
  means removing empty factor levels for the columns.
  Possible coices are: \dQuote{no} = Don't do it.
  \dQuote{warn} = Do it but warn about it.  \dQuote{quiet}
  = Do it but keep silent.  Default is \dQuote{warn}.}

  \item{check.data}{[\code{logical(1)}]\cr Should sanity of
  data be checked initially at task creation?  You should
  have good reasons to turn this off (one might be speed).
  Default is \code{TRUE}}
}
\value{
[\code{\link{Task}}].
}
\description{
The task encapsulates the data and specifies - through its subclasses -
the type of the task.
It also contains a description object detailing further aspects of the data.

Object members:
\describe{
\item{env [\code{environment}]}{Environment where data for the task are stored.
  Use \code{\link{getTaskData}} in order to access it.}
\item{weights [\code{numeric}]}{See argument above. \code{NULL} if not present.}
\item{blocking [\code{factor}]}{See argument above. \code{NULL} if not present.}
\item{task.desc [\code{\link{TaskDesc}}]}{Encapsulates further information about the task.}
}
Create an unsupervised (e.g. clustering) task.
}
\examples{
makeClusterTask(data = iris)
}


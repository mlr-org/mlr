\name{makeWrappedModel}
\alias{WrappedModel}
\alias{makeWrappedModel}
\title{Induced model of learner.}
\usage{
makeWrappedModel(learner, model, task.desc, subset, features, time)
}
\arguments{
  \item{learner}{[\code{\link{Learner}}]\cr The learner.}

  \item{model}{[any]\cr Underlying model.}

  \item{task.desc}{[\code{\link{TaskDesc}}]\cr Task
  description object.}

  \item{subset}{[\code{integer}]\cr Subset used for
  training.}

  \item{features}{[\code{character}]\cr Features used for
  training.}

  \item{time}{[\code{numeric(1)}]\cr Computation time for
  model fit in seconds.}
}
\value{
[\code{\link{WrappedModel}}].
}
\description{
Result from \code{\link{train}}. It internally stores the
underlying fitted model, the subset used for training,
features used for training and computtation time for
training.
}
\details{
The constructer \code{makeWrappedModel} is only for
internal use.

Object members: See arguments.
}


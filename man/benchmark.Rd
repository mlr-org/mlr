\name{benchmark}
\alias{benchmark}
\title{Benchmark experiment for multiple learners and tasks.}
\usage{
benchmark(learners, tasks, resamplings, measures,
  same.resampling.instance = TRUE)
}
\arguments{
  \item{learners}{[\code{\link{Learner}} | list of them]\cr
  Learning algorithms which should be compared.}

  \item{tasks}{[\code{\link{SupervisedTask}} | list of
  them]\cr Tasks that learners should be run on.}

  \item{resamplings}{[\code{\link{ResampleDesc}} |
  \code{\link{ResampleInstance}} | list of them]\cr
  Resampling strategies for tasks.}

  \item{measures}{[\code{\link{Measure}} | list of them]\cr
  Performance measures.}

  \item{models}{[logical] \cr Should all fitted models be
  stored?  Default is FALSE.}

  \item{same.resampling.instance}{[logical(1)]\cr Should
  the same resampling instance be used for all learners
  (per task) to reduce variance?  Default is \code{TRUE}.}
}
\value{
\code{\linkS4class{bench.result}}.
}
\description{
Complete benchmark experiment to compare different learning
algorithms across one or more tasks w.r.t. a given
resampling strategy. Experiments are paired, meaning always
the same training / test sets are used for the different
learners.

You can also get automatic, internal tuning by using
\code{\link{makeTuneWrapper}} with your learner.
}


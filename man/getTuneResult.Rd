\name{getTuneResult}
\alias{getTuneResult}
\title{Returns the optimal hyperparameters and optimization path after training or benchmarking.}
\usage{
getTuneResult(object)
}
\arguments{
  \item{object}{[\code{\link{WrappedModel}} |
  \code{BenchmarkResult}]\cr Trained Model created with
  \code{\link{makeTuneWrapper}} or benchmark result created
  with \code{\link{benchmark}}.}
}
\value{
[\code{\link{TuneResult}} or list of \code{\link{TuneResult}}s].
  \code{NULL}, if no tuning was performed.
}
\description{
Returns the optimal hyperparameters and optimization path after training or benchmarking.
}
\seealso{
Other benchmark: \code{\link{BenchmarkResult}};
  \code{\link{FilterResult}},
  \code{\link{getFilterResult}}; \code{\link{benchmark}};
  \code{\link{getAggrPerformances}};
  \code{\link{getFeatSelResult}};
  \code{\link{getPerformances}};
  \code{\link{getPredictions}}

Other tune: \code{\link{ModelMultiplexer}},
  \code{\link{makeModelMultiplexer}};
  \code{\link{TuneControl}},
  \code{\link{TuneControlCMAES}},
  \code{\link{TuneControlGrid}},
  \code{\link{TuneControlIrace}},
  \code{\link{TuneControlOptim}},
  \code{\link{TuneControlRandom}},
  \code{\link{makeTuneControlCMAES}},
  \code{\link{makeTuneControlGenSA}},
  \code{\link{makeTuneControlGrid}},
  \code{\link{makeTuneControlIrace}},
  \code{\link{makeTuneControlRandom}};
  \code{\link{makeModelMultiplexerParamSet}};
  \code{\link{makeTuneWrapper}}; \code{\link{tuneParams}};
  \code{\link{tuneThreshold}}
}


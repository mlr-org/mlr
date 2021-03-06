% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TuneMultiCritControl.R,
%   R/TuneMultiCritControlGrid.R, R/TuneMultiCritControlMBO.R,
%   R/TuneMultiCritControlNSGA2.R, R/TuneMultiCritControlRandom.R
\name{TuneMultiCritControl}
\alias{TuneMultiCritControl}
\alias{TuneMultiCritControlGrid}
\alias{TuneMultiCritControlRandom}
\alias{TuneMultiCritControlNSGA2}
\alias{TuneMultiCritControlMBO}
\alias{makeTuneMultiCritControlGrid}
\alias{makeTuneMultiCritControlMBO}
\alias{makeTuneMultiCritControlNSGA2}
\alias{makeTuneMultiCritControlRandom}
\title{Create control structures for multi-criteria tuning.}
\usage{
makeTuneMultiCritControlGrid(
  same.resampling.instance = TRUE,
  resolution = 10L,
  log.fun = "default",
  final.dw.perc = NULL,
  budget = NULL
)

makeTuneMultiCritControlMBO(
  n.objectives = mbo.control$n.objectives,
  same.resampling.instance = TRUE,
  impute.val = NULL,
  learner = NULL,
  mbo.control = NULL,
  tune.threshold = FALSE,
  tune.threshold.args = list(),
  continue = FALSE,
  log.fun = "default",
  final.dw.perc = NULL,
  budget = NULL,
  mbo.design = NULL
)

makeTuneMultiCritControlNSGA2(
  same.resampling.instance = TRUE,
  impute.val = NULL,
  log.fun = "default",
  final.dw.perc = NULL,
  budget = NULL,
  ...
)

makeTuneMultiCritControlRandom(
  same.resampling.instance = TRUE,
  maxit = 100L,
  log.fun = "default",
  final.dw.perc = NULL,
  budget = NULL
)
}
\arguments{
\item{same.resampling.instance}{(\code{logical(1)})\cr
Should the same resampling instance be used for all evaluations to reduce variance?
Default is \code{TRUE}.}

\item{resolution}{(\link{integer})\cr
Resolution of the grid for each numeric/integer parameter in \code{par.set}.
For vector parameters, it is the resolution per dimension.
Either pass one resolution for all parameters, or a named vector.
See \link[ParamHelpers:generateGridDesign]{ParamHelpers::generateGridDesign}.
Default is 10.}

\item{log.fun}{(\code{function} | \code{character(1)})\cr
Function used for logging. If set to \dQuote{default} (the default), the evaluated design points, the resulting
performances, and the runtime will be reported.
If set to \dQuote{memory} the memory usage for each evaluation will also be displayed, with \code{character(1)} small increase
in run time.
Otherwise \code{character(1)} function with arguments \code{learner}, \code{resampling}, \code{measures},
\code{par.set}, \code{control}, \code{opt.path}, \code{dob}, \code{x}, \code{y}, \code{remove.nas},
\code{stage} and \code{prev.stage} is expected.
The default displays the performance measures, the time needed for evaluating,
the currently used memory and the max memory ever used before
(the latter two both taken from \link{gc}).
See the implementation for details.}

\item{final.dw.perc}{(\code{boolean})\cr
If a Learner wrapped by a \link{makeDownsampleWrapper} is used, you can define the value of \code{dw.perc} which is used to train the Learner with the final parameter setting found by the tuning.
Default is \code{NULL} which will not change anything.}

\item{budget}{(\code{integer(1)})\cr
Maximum budget for tuning. This value restricts the number of function
evaluations. In case of \code{makeTuneMultiCritControlGrid} this number
must be identical to the size of the grid. For
\code{makeTuneMultiCritControlRandom} the \code{budget} equals the number
of iterations (\code{maxit}) performed by the random search algorithm.
In case of \code{makeTuneMultiCritControlNSGA2} the \code{budget}
corresponds to the product of the maximum number of generations
(\code{max(generations)}) + 1 (for the initial population) and the size of
the population (\code{popsize}). For \code{makeTuneMultiCritControlMBO} the
\code{budget} equals the number of objective function evaluations, i.e. the
number of MBO iterations + the size of the initial design. If not \code{NULL},
this will overwrite existing stopping conditions in \code{mbo.control}.}

\item{n.objectives}{(\code{integer(1)})\cr
Number of objectives, i.e. number of \link{Measure}s to optimize.}

\item{impute.val}{(\link{numeric})\cr
If something goes wrong during optimization (e.g. the learner crashes),
this value is fed back to the tuner, so the tuning algorithm does not abort.
Imputation is only active if \code{on.learner.error} is configured not to stop in \link{configureMlr}.
It is not stored in the optimization path, an NA and a corresponding error message are
logged instead.
Note that this value is later multiplied by -1 for maximization measures internally, so you
need to enter a larger positive value for maximization here as well.
Default is the worst obtainable value of the performance measure you optimize for when
you aggregate by mean value, or \code{Inf} instead.
For multi-criteria optimization pass a vector of imputation values, one for each of your measures,
in the same order as your measures.}

\item{learner}{(\link{Learner} | \code{NULL})\cr
The surrogate learner: A regression learner to model performance landscape.
For the default, \code{NULL}, \pkg{mlrMBO} will automatically create a suitable learner based on the rules described in \link[mlrMBO:makeMBOLearner]{mlrMBO::makeMBOLearner}.}

\item{mbo.control}{(\link[mlrMBO:makeMBOControl]{mlrMBO::MBOControl} | \code{NULL})\cr
Control object for model-based optimization tuning.
For the default, \code{NULL}, the control object will be created with all the defaults as described in \link[mlrMBO:makeMBOControl]{mlrMBO::makeMBOControl}.}

\item{tune.threshold}{(\code{logical(1)})\cr
Should the threshold be tuned for the measure at hand, after each hyperparameter evaluation,
via \link{tuneThreshold}?
Only works for classification if the predict type is \dQuote{prob}.
Default is \code{FALSE}.}

\item{tune.threshold.args}{(\link{list})\cr
Further arguments for threshold tuning that are passed down to \link{tuneThreshold}.
Default is none.}

\item{continue}{(\code{logical(1)})\cr
Resume calculation from previous run using \link[mlrMBO:mboContinue]{mlrMBO::mboContinue}?
Requires \dQuote{save.file.path} to be set.
Note that the \link[ParamHelpers:OptPath]{ParamHelpers::OptPath} in the \link[mlrMBO:OptResult]{mlrMBO::OptResult}
will only include the evaluations after the continuation.
The complete \link{OptPath} will be found in the slot \verb{$mbo.result$opt.path}.}

\item{mbo.design}{(\link{data.frame} | \code{NULL})\cr
Initial design as data frame.
If the parameters have corresponding trafo functions,
the design must not be transformed before it is passed!
For the default, \code{NULL}, a default design is created like described in \link[mlrMBO:mbo]{mlrMBO::mbo}.}

\item{...}{(any)\cr
Further control parameters passed to the \code{control} arguments of
\link[cmaes:cma_es]{cmaes::cma_es} or \link[GenSA:GenSA]{GenSA::GenSA}, as well as
towards the \code{tunerConfig} argument of \link[irace:irace]{irace::irace}.}

\item{maxit}{(\code{integer(1)})\cr
Number of iterations for random search.
Default is 100.}
}
\value{
(\link{TuneMultiCritControl}). The specific subclass is one of
\link{TuneMultiCritControlGrid}, \link{TuneMultiCritControlRandom},
\link{TuneMultiCritControlNSGA2}, \link{TuneMultiCritControlMBO}.
}
\description{
The following tuners are available:
\describe{
\item{makeTuneMultiCritControlGrid}{Grid search. All kinds of parameter types can be handled.
You can either use their correct param type and \code{resolution},
or discretize them yourself by always using \link[ParamHelpers:Param]{ParamHelpers::makeDiscreteParam}
in the \code{par.set} passed to \link{tuneParams}.}
\item{makeTuneMultiCritControlRandom}{Random search. All kinds of parameter types can be handled.}
\item{makeTuneMultiCritControlNSGA2}{Evolutionary method \link[mco:nsga2]{mco::nsga2}.
Can handle numeric(vector) and integer(vector) hyperparameters, but no dependencies.
For integers the internally proposed numeric values are automatically rounded.}
\item{makeTuneMultiCritControlMBO}{Model-based/ Bayesian optimization. All kinds of
parameter types can be handled.}
}
}
\seealso{
Other tune_multicrit: 
\code{\link{plotTuneMultiCritResult}()},
\code{\link{tuneParamsMultiCrit}()}
}
\concept{tune_multicrit}

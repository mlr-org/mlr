#' @title Create control structures for tuning.
#'
#' @description
#' The following tuners are available:
#' \describe{
#'   \item{makeTuneControlGrid}{Grid search. All kinds of parameter types can be handled.
#'     You can either use their correct param type and \code{resolution},
#'     or discretize them yourself by always using \code{\link[ParamHelpers]{makeDiscreteParam}}
#'     in the \code{par.set} passed to \code{\link{tuneParams}}.}
#'   \item{makeTuneControlRandom}{Random search. All kinds of parameter types can be handled.}
#'   \item{makeTuneControlDesign}{Completely pre-specifiy a data.frame of design points to be evaluated
#'     during tuning. All kinds of parameter types can be handled.}
#'   \item{makeTuneControlCMAES}{CMA Evolution Strategy with method \code{\link[cmaes]{cma_es}}.
#'     Can handle numeric(vector) and integer(vector) hyperparameters, but no dependencies.
#'     For integers the internally proposed numeric values are automatically rounded.
#'     The sigma variance parameter is initialized to 1/4 of the span of box-constraints per
#'     parameter dimension.}
#'   \item{makeTuneControlGenSA}{Generalized simulated annealing with method \code{\link[GenSA]{GenSA}}.
#'     Can handle numeric(vector) and integer(vector) hyperparameters, but no dependencies.
#'     For integers the internally proposed numeric values are automatically rounded.}
#'   \item{makeTuneControlIrace}{Tuning with iterated F-Racing with method \code{\link[irace]{irace}}.
#'     All kinds of parameter types can be handled. We return the best of the final elite
#'     candidates found by irace in the last race. Its estimated performance is the mean of all
#'     evaluations ever done for that candidate.}
#'   \item{makeTuneControlGA}{Genetic algorithm (\code{\link[GA]{GA}}) tuning method. Can handle numeric(vector) and 
#'     integer(vector) hyperparameters, but no dependencies. For integers the internally proposed numeric
#'     values are automatically rounded. }
#'   \item{makeTuneControlPSO}{Particle Swarm Optimization (\code{\link[PSO]{PSO}}) implementation consistent with the
#'     standard PSO 2007/2011 by Maurice Clerc et al.
#'     Can handle numeric(vector) and integer(vector) hyperparameters, but no dependencies.
#'     For integers the internally proposed numeric values are automatically rounded.}
#'   \item{makeTuneControlEDA}{Tuning with Estimation Distribution Algorithms (\code{\link[EDA]{EDAs}}) based on copula and vines functions.
#'     Can handle numeric(vector) and integer(vector) hyperparameters, but no dependencies.
#'     For integers the internally proposed numeric values are automatically rounded.}
#' }
#'
#' Some notes on irace: For resampling you have to pass a \code{\link{ResampleDesc}},
#' not a \code{\link{ResampleInstance}}.
#' The resampling strategy is randomly instantiated \code{n.instances} times and
#' these are the instances in the sense of irace (\code{instances} element of \code{tunerConfig}
#' in \code{\link[irace]{irace}}). Also note that irace will always
#' store its tuning results in a file on disk, see the package documentation for details on this
#' and how to change the file path.
#'
#' @param same.resampling.instance [\code{logical(1)}]\cr
#'   Should the same resampling instance be used for all evaluations to reduce variance?
#'   Default is \code{TRUE}.
#' @template arg_imputey
#' @param start [\code{list}]\cr
#'   Named list of initial parameter values.
#' @param tune.threshold [\code{logical(1)}]\cr
#'   Should the threshold be tuned for the measure at hand, after each hyperparameter evaluation,
#'   via \code{\link{tuneThreshold}}?
#'   Only works for classification if the predict type is \dQuote{prob}.
#'   Default is \code{FALSE}.
#' @param tune.threshold.args [\code{list}]\cr
#'   Further arguments for threshold tuning that are passed down to \code{\link{tuneThreshold}}.
#'   Default is none.
#' @param log.fun [\code{function} | \code{NULL}]\cr
#'   Function used for logging. If set to \code{NULL}, the internal default will be used.
#'   Otherwise a function with arguments \code{learner}, \code{resampling}, \code{measures},
#'   \code{par.set}, \code{control}, \code{opt.path}, \code{dob}, \code{x}, \code{y}, \code{remove.nas},
#'   and \code{stage} is expected.
#'   The default displays the performance measures, the time needed for evaluating,
#'   the currently used memory and the max memory ever used before
#'   (the latter two both taken from \code{\link{gc}}).
#'   See the implementation for details.
#' @param final.dw.perc [\code{boolean}]\cr
#'   If a Learner wrapped by a \code{\link{makeDownsampleWrapper}} is used, you can define the value of \code{dw.perc} which is used to train the Learner with the final parameter setting found by the tuning.
#'   Default is \code{NULL} which will not change anything.
#' @param budget [\code{integer(1)}]\cr
#'   Maximum budget for tuning. This value restricts the number of function
#'   evaluations. In case of \code{makeTuneControlGrid} this number must be identical
#'   to the size of the grid. For \code{makeTuneControlRandom} the
#'   \code{budget} equals the number of iterations (\code{maxit}) performed by
#'   the random search algorithm. Within the \code{\link[cmaes]{cma_es}} the
#'   \code{budget} corresponds to the product of the number of generations
#'   (\code{maxit}) and the number of offsprings per generation
#'   (\code{lambda}). \code{\link[GA]{GA}} defines the \code{budget} as the product of
#'   the number of generations (\code{maxit}) and the population's size (\code{popSize}).
#'   \code{\link[EDA]{EDAs}} does the same. The \code{\link[PSO]{PSO}} defines it as the
#'   product of the number of generations (\code{maxit}) and the number of swarm particles (\code{nParticles}).
#'   \code{\link[GenSA]{GenSA}} defines the \code{budget} via
#'   the argument \code{max.call}. However, one should note that this algorithm
#'   does not stop its local search before its end. This behaviour might lead
#'   to an extension of the defined budget and will result in a warning. In
#'   \code{irace}, \code{budget} is passed to \code{maxExperiments}.
#' @param ... [any]\cr
#'   Further control parameters passed to the \code{control} arguments of
#'   \code{\link[cmaes]{cma_es}}, \code{\link[GA]{GA}}, \code{\link[PSO]{PSO}}, 
#'   \code{\link[EDA]{EDA}}, \code{\link[GenSA]{GenSA}}, as well as
#'   towards the \code{tunerConfig} argument of \code{\link[irace]{irace}}.
#' @return [\code{\link{TuneControl}}]. The specific subclass is one of
#'   \code{\link{TuneControlGrid}}, \code{\link{TuneControlRandom}},
#'   \code{\link{TuneControlCMAES}}, \code{\link{TuneControlGenSA}},
#'   \code{\link{TuneControlIrace}}, \code{\link{TuneControlGA}}, 
#'   \code{\link{TuneControlPSO}}, \code{\link{TuneControlEDA}}.
#' @family tune
#' @name TuneControl
#' @rdname TuneControl
#' @aliases TuneControlGrid TuneControlRandom TuneControlCMAES TuneControlGenSA TuneControlIrace TuneControlGA TuneControlPSO TuneControlEDA
NULL

makeTuneControl = function(same.resampling.instance, impute.val = NULL,
  start = NULL, tune.threshold = FALSE, tune.threshold.args = list(),
  log.fun = NULL, final.dw.perc = NULL, budget = NULL, ..., cl) {

  if (!is.null(start))
    assertList(start, min.len = 1L, names = "unique")
  if (is.null(log.fun))
    log.fun = logFunTune
  if (!is.null(budget))
    budget = asCount(budget)
  if (!is.null(final.dw.perc))
    assertNumeric(final.dw.perc, lower = 0, upper = 1)
  x = makeOptControl(same.resampling.instance, impute.val, tune.threshold, tune.threshold.args, log.fun, final.dw.perc, ...)
  x$start = start
  x$budget = budget
  addClasses(x, c(cl, "TuneControl"))
}

#' @export
print.TuneControl = function(x, ...) {
  catf("Tune control: %s", class(x)[1])
  catf("Same resampling instance: %s", x$same.resampling.instance)
  catf("Imputation value: %s", ifelse(is.null(x$impute.val), "<worst>", sprintf("%g", x$impute.val)))
  catf("Start: %s", convertToShortString(x$start))
  catf("Budget: %i", x$budget)
  catf("Tune threshold: %s", x$tune.threshold)
  catf("Further arguments: %s", convertToShortString(x$extra.args))
}


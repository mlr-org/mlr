#' Train a learning algorithm.
#'
#' Given a \code{\link{Task}}, creates a model for the learning machine
#' which can be used for predictions on new data.
#'
#' @template arg_learner
#' @template arg_task
#' @template arg_subset
#' @param weights [\code{numeric}]\cr
#'   Optional, non-negative case weight vector to be used during fitting.
#'   If given, must be of same length as \code{subset} and in corresponding order.
#'   By default \code{NULL} which means no weights are used unless specified in the task (\code{\link{Task}}).
#'   Weights from the task will be overwritten.
#' @return [\code{\link{WrappedModel}}].
#' @export
#' @seealso \code{\link{predict.WrappedModel}}
#' @examples
#' training.set = sample(seq_len(nrow(iris)), nrow(iris) / 2)
#'
#' ## use linear discriminant analysis to classify iris data
#' task = makeClassifTask(data = iris, target = "Species")
#' learner = makeLearner("classif.lda", method = "mle")
#' mod = train(learner, task, subset = training.set)
#' print(mod)
#'
#' ## use random forest to classify iris data
#' task = makeClassifTask(data = iris, target = "Species")
#' learner = makeLearner("classif.rpart", minsplit = 7, predict.type = "prob")
#' mod = train(learner, task, subset = training.set)
#' print(mod)
train = function(learner, task, subset, weights = NULL) {
  learner = checkLearner(learner)
  assertClass(task, classes = "Task")
  if (missing(subset) || is.null(subset)) {
    subset = seq_len(getTaskSize(task))
  } else {
    if (is.logical(subset))
      subset = which(subset)
    else
      subset = asInteger(subset)
  }

  # make sure that pack for learner is loaded, probably needed when learner is exported
  requireLearnerPackages(learner)

  tn = getTaskTargetNames(task)

  # make pars list for train call
  pars = list(.learner = learner, .task = task, .subset = subset)

  # FIXME: code is bad here, set weights, the simply check it in checktasklearner
  if (!is.null(weights)) {
    assertNumeric(weights, len = length(subset), any.missing = FALSE, lower = 0)
  } else {
    weights = getTaskWeights(task)
  }

  checkLearnerBeforeTrain(task, learner, weights)
  pars$.weights = weights

  # only pass train hyper pars as basic rlearner in ...
  pars = c(pars, getHyperPars(learner, c("train", "both")))

  vars = getTaskFeatureNames(task)
  # no vars? then use no vars model

  #NOTE: Most forecasting tasks are univariate (only using y), so this will check does not work here
  if (length(vars) == 0L && getLearnerType(learner) != "fcregr") {
    learner.model = makeNoFeaturesModel(targets = getTaskData(task)[subset, tn], task.desc = getTaskDesc(task))
    time.train = 0
  } else {
    opts = getLearnerOptions(learner, c("show.learner.output", "on.learner.error", "on.learner.warning", "on.error.dump"))
    # set the seed
    debug.seed = getMlrOption("debug.seed", NULL)
    if (!is.null(debug.seed))
      set.seed(debug.seed)
    # for optwrappers we want to see the tuning / varsel logging
    # FIXME: is case really ok for optwrapper? can we supppress then too?
    fun1 = if (opts$show.learner.output || inherits(learner, "OptWrapper")) identity else capture.output
    fun2 = if (opts$on.learner.error == "stop") identity else function(x) try(x, silent = TRUE)
    fun3 = if (opts$on.learner.error == "stop" || !opts$on.error.dump) identity else function(x) {
        withCallingHandlers(x, error = function(c) utils::dump.frames())
      }
    if (opts$on.learner.warning == "quiet") {
      old.warn.opt = getOption("warn")
      on.exit(options(warn = old.warn.opt))
      options(warn = -1L)
    }
    time.train = measureTime(fun1({learner.model = fun2(fun3(do.call(trainLearner, pars)))}))
    # was there an error during training? maybe warn then
    if (is.error(learner.model) && opts$on.learner.error == "warn")
      warningf("Could not train learner %s: %s", learner$id, as.character(learner.model))
  }
  factor.levels = getTaskFactorLevels(task)
  makeWrappedModel(learner, learner.model, getTaskDesc(task), subset, vars, factor.levels, time.train)
}

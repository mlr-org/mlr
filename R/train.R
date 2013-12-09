#' Train a learning algorithm.
#'
#' Given a \code{\link{SupervisedTask}}, creates a model for the learning machine
#' which can be used for predictions on new data.
#'
#' @param learner [\code{\link{Learner}}]\cr
#'   The learner.
#' @param task [\code{\link{SupervisedTask}}]\cr
#'   The task.
#' @param subset [\code{integer}]\cr
#'   An index vector specifying the training cases to be used for fitting.
#'   By default the complete data set is used.
#' @param weights [\code{numeric}]\cr
#'   Optional, non-negative case weight vector to be used during fitting.
#'   If given, must be of same length as \code{subset} and in corresponding order.
#'   By default missing which means no weights are used.
#'   Overwrites weights specified in the task ([\code{\link{SupervisedTask}}]).
#'   By default missing which means no weights are used unless specified in the task.
#' @return [\code{\link{WrappedModel}}].
#' @export
#' @seealso \code{\link{predict.WrappedModel}}
#' @examples
#' training.set <- sample(1:nrow(iris), nrow(iris) / 2)
#'
#' ## use linear discriminant analysis to classify iris data
#' task <- makeClassifTask(data = iris, target = "Species")
#' learner <- makeLearner("classif.lda", method = "mle")
#' mod <- train(learner, task, subset = training.set)
#' print(mod)
#'
#' ## use random forest to classify iris data
#' task <- makeClassifTask(data = iris, target = "Species")
#' learner <- makeLearner("classif.rpart", minsplit = 7, predict.type = "prob")
#' mod <- train(learner, task, subset = training.set)
#' print(mod)
train = function(learner, task, subset, weights) {
  checkArg(learner, "Learner")
  checkArg(task, "SupervisedTask")
  if (missing(subset)) {
    subset = seq_len(task$task.desc$size)
  } else {
    subset = convertIntegers(subset)
    checkArg(subset, "integer", na.ok=FALSE)
  }

  # make sure that pack for learner is loaded, probably needed when learner is exported
  requireLearnerPackages(learner)


  tn = task$task.desc$target

  # make pars list for train call
  pars = list(.learner=learner, .task=task, .subset=subset)

  if(!missing(weights)) {
    checkArg(weights, "numeric", len=length(subset), na.ok=FALSE, lower=0)
    pars$.weights = weights
  } else {
    # we do not check if learer supports weights if weights are part of the task (yet)!
    pars$.weights = task$weights
  }

  checkTaskLearner(task, learner, weights)

  # only pass train hyper pars as basic rlearner in ...
  pars = c(pars, getHyperPars(learner, "train"))

  vars = getTaskFeatureNames(task)
  # no vars? then use no vars model

  if (length(vars) == 0L) {
    learner.model = makeNoFeaturesModel(targets=task$env$data[subset, tn], task.desc=task$task.desc)
    time.train = 0
  } else {
    opt.slo = getMlrOption("show.learner.output")
    opt.ole = getMlrOption("on.learner.error")
    # set the seed
    debug.seed = getMlrOption("debug.seed", NULL)
    if(!is.null(debug.seed))
      set.seed(debug.seed)
    # for optwrappers we want to see the tuning / varsel logging
    # FIXME is case really ok for optwrapper? can we supppress then too?
    if (opt.slo || inherits(learner, "OptWrapper"))
      fun1 = identity
    else
      fun1 = capture.output
    if (opt.ole == "stop")
      fun2 = identity
    else
      fun2 = function(x) try(x, silent=TRUE)
    old.warn.opt = getOption("warn")
    on.exit(options(warn = old.warn.opt))
    if (getMlrOption("on.learner.warning") == "quiet") {
      options(warn = -1L)
    }
    st = system.time(or <- fun1(learner.model <- fun2(do.call(trainLearner, pars))), gcFirst = FALSE)
    # was there an error during training? maybe warn then
    if(is.error(learner.model) && opt.ole == "warn")
      warningf("Could not train learner %s: %s", learner$id, as.character(learner.model))
    time.train = as.numeric(st[3L])
  }
  makeWrappedModel(learner, learner.model, task$task.desc, subset, vars, time.train)
}

#' @export
makeRLearner.classif.neuralnet = function() {
  makeRLearnerClassif(
    cl = "classif.neuralnet",
    package = "neuralnet",
    par.set = makeParamSet(
      makeIntegerVectorLearnerParam(id = "hidden", default = 1L, lower = 1L),
      makeNumericLearnerParam(id = "threshold", default = 0.01),
      makeNumericLearnerParam(id = "stepmax", default = 1e+05),
      makeIntegerLearnerParam(id = "rep", default = 1L, lower = 1L),
      makeNumericVectorLearnerParam(id = "startweights"),
      makeNumericVectorLearnerParam(id = "learningrate.limit"),
      makeUntypedLearnerParam(id = "learningrate.factor",
        default = list(minus = 0.5, plus = 1.2)),
      makeNumericLearnerParam(id = "learningrate"),
      makeDiscreteLearnerParam(id = "lifesign", default = "none",
        values = c("none", "minimal", "full")),
      makeIntegerLearnerParam(id = "lifesign.step", default = 1000L),
      makeDiscreteLearnerParam(id = "algorithm", default = "rprop+",
        values = c("backprop", "rprop+", "rprop-", "sag", "slr")),
      makeDiscreteLearnerParam(id = "err.fct", default = "ce",
        values = c("sse", "ce")),
      # FIXME default in neuralnet() or err.fct is "sse"
      makeDiscreteLearnerParam(id = "act.fct", default = "logistic",
        values = c("logistic", "tanh")),
      makeNumericVectorLearnerParam(id = "exclude"),
      makeNumericVectorLearnerParam(id = "constant.weights"),
      makeLogicalLearnerParam(id = "likelihood", default = FALSE)
    ),
    par.vals = list(err.fct = "ce"),
    properties = c("twoclass", "numerics", "prob"),
    name = "Neural Network from neuralnet",
    short.name = "neuralnet",
    note = "`err.fct` has been set to `ce` and `linear.output` to FALSE to do classification.",
    callees = "neuralnet"
  )
}

#' @export
trainLearner.classif.neuralnet = function(.learner, .task, .subset, .weights = NULL, ...) {

  f = getTaskFormula(.task)
  cf = as.character(f)
  taskdat = getTaskData(.task, .subset)
  nms = names(taskdat)
  formula.head = as.character(f)[2]
  if (is.character(taskdat[[formula.head]])) {
    taskdat[[formula.head]] = as.factor(taskdat[[formula.head]])
    taskdat[[formula.head]] = as.numeric(taskdat[[formula.head]])
  }
  if (is.factor(taskdat[[formula.head]])) {
    taskdat[[formula.head]] = as.numeric(taskdat[[formula.head]])
  }
  lvls = length(unique(taskdat[[formula.head]]))
  if (length(lvls) > 2) {
    stop("Use neuralnet to do binary classification")
  }
  if (!all(taskdat[[formula.head]] == 0 | taskdat[[formula.head]] == 1)) {
    taskdat[[formula.head]] = taskdat[[formula.head]] - 1
  }
  if (sum(stri_detect_regex(cf, "\\.")) > 0) {
    varnames = nms[nms != formula.head]
    formula.head = stri_paste("as.numeric(", formula.head, ")~", sep = " ")
    formula.expand = stri_paste(formula.head,
      stri_paste(varnames, collapse = "+", sep = " "),
      sep = " ")
    formula.expand = as.formula(formula.expand)
    f = formula.expand
  }

  neuralnet::neuralnet(f, data = taskdat, linear.output = FALSE, ...)
}

#' @export
predictLearner.classif.neuralnet = function(.learner, .model, .newdata, ...) {

  type = switch(.learner$predict.type, response = "class", prob = "raw")

  p = neuralnet::compute(x = .model$learner.model, covariate = .newdata, ...)
  p = p[[2]]

  p = cbind(1 - p, p)
  colnames(p) = .model$factor.levels[[1]]

  if (type == "class") {
    classes = colnames(p)[max.col(p)]
    return(as.factor(classes))
    # p = factor(as.numeric(p>0.5), labels = .model$factor.levels$Class)
  }
  return(p)
}

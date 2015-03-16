#' @export
makeRLearner.classif.neuralnet = function() {
    makeRLearnerClassif(
        cl = "classif.neuralnet",
        package = "neuralnet",
        par.set = makeParamSet(
            makeIntegerLearnerParam(id = "hidden", default = 1L, lower = 1L),
            makeIntegerLearnerParam(id = "threshold", default = 0.01),
            makeLogicalLearnerParam(id = "stepmax", default = 1e+05),
            makeNumericLearnerParam(id = "rep", default = 1, lower = 1L),
            makeNumericLearnerParam(id = "startweights", default = NULL),
            makeLogicalLearnerParam(id = "learningrate.factor", 
                                    default = list(minus = 0.5, plus = 1.2)),
            makeLogicalLearnerParam(id = "learningrate", default = NULL),
            makeIntegerLearnerParam(id = "lifesign", default = "none"),
            makeNumericLearnerParam(id = "lifesign.step", default = 1000),
            makeNumericLearnerParam(id = "algorithm", default = "rprop+")
            makeNumericLearnerParam(id = "err.fct", default = "sse")
            makeNumericLearnerParam(id = "act.fct", default = "logistic")
            makeNumericLearnerParam(id = "linear.output", default = TRUE)
            makeNumericLearnerParam(id = "exclude", default = NULL)
            makeNumericLearnerParam(id = "constant.weights", default = NULL)
            makeNumericLearnerParam(id = "likelihood", default = FALSE)
        ),
        properties = c("twoclass", "multiclass", "numerics", "factors", "prob"),
        name = "Neural Network from neuralnet",
        short.name = "neuralnet",
    )
}

#' @export
trainLearner.classif.neuralnet = function(.learner, .task, .subset, .weights = NULL,  ...) {
        f = as.formula(getTaskFormulaAsString(.task))
        neuralnet::neuralnet(f, data = getTaskData(.task, .subset), ...)
}

#' @export
predictLearner.classif.neuralnet = function(.learner, .model, .newdata, ...) {
    p = neuralnet::compute(x = .model$learner.model, newdata = .newdata, ...)
    return(p)
}

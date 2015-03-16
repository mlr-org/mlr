#' @export
makeRLearner.classif.neuralnet = function() {
    makeRLearnerClassif(
        cl = "regr.neuralnet",
        package = "neuralnet",
        par.set = makeParamSet(
            makeIntegerVectorLearnerParam(id = "hidden", default = 1L, lower = 1L),
            makeNumericLearnerParam(id = "threshold", default = 0.01),
            makeIntegerLearnerParam(id = "stepmax", default = 1e+05),
            makeIntegerLearnerParam(id = "rep", default = 1, lower = 1L),
            makeNumericVectorLearnerParam(id = "startweights", default = NULL),
            makeNumericVectorLearnerParam(id = "learningrate.limit", default = NULL)
            makeUntypedLearnerParam(id = "learningrate.factor", 
                                    default = list(minus = 0.5, plus = 1.2)),
            makeNumericLearnerParam(id = "learningrate", default = NULL),
            makeDiscreteLearnerParam(id = "lifesign", default = "none",
                                     values = c("none","minimal","full")),
            makeIntegerLearnerParam(id = "lifesign.step", default = 1000),
            makeDiscreteLearnerParam(id = "algorithm", default = "rprop+",
                                     values=c("backprop","rprop+","rprop-","sag","slr"))
            makeDiscreteLearnerParam(id = "err.fct", default = "sse",
                                     values=c("sse","ce"))
            makeDiscreteLearnerParam(id = "act.fct", default = "logistic",
                                     values=c("logistic","tanh"))
            makeLogicalLearnerParam(id = "linear.output", default = TRUE)
            makeNumericVectorLearnerParam(id = "exclude", default = NULL)
            makeNumericVectorLearnerParam(id = "constant.weights", default = NULL)
            makeLogicalLearnerParam(id = "likelihood", default = FALSE)
        ),
        par.vals = list(err.fct = "sse")
        properties = c("numerics"),
        name = "Neural Network from neuralnet",
        short.name = "neuralnet",
        note = "`err.fct` has been set to `sse` to do regression."
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

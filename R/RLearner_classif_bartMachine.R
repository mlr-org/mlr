#FIXME: I have no idea which routine internally prints to which fucking stream
# but neither verbose=FALSE can sicth off the iteration  output in all case, nor
# can I suppress it with capture.output or suppressMessages

#' @export
makeRLearner.classif.bartMachine = function() {
  makeRLearnerClassif(
    cl = "classif.bartMachine",
    package = "bartMachine",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "num_trees", default = 50L, lower = 1L),
      makeIntegerLearnerParam(id = "num_burn_in", default = 250L, lower = 0L),
      makeIntegerLearnerParam(id = "num_interations_after_burn_in", default = 1000L, lower = 0L),
      makeNumericLearnerParam(id = "alpha", default = 0.95, lower = 0),
      makeNumericLearnerParam(id = "beta", default = 2, lower = 0),
      makeNumericLearnerParam(id = "k", default = 2, lower = 0),
      makeNumericLearnerParam(id = "q", default = 0.9, lower = 0, upper = 1),
      makeNumericLearnerParam(id = "prob_rule_class", default = 0.5, lower = 0, upper = 1),
      makeNumericVectorLearnerParam(id = "mh_prob_steps", default = c(2.5, 2.5, 4)/9, len = 3L),
      makeLogicalLearnerParam(id = "debug_log", default = FALSE),
      makeLogicalLearnerParam(id = "run_in_sample", default = TRUE),
      makeNumericVectorLearnerParam(id = "cov_prior_vec"),
      makeLogicalLearnerParam(id = "use_missing_data", default = FALSE),
      makeIntegerLearnerParam(id = "num_rand_samps_in_library", default = 10000, lower = 1),
      makeLogicalLearnerParam(id = "use_missing_data_dummies_as_covars", default = FALSE),
      makeLogicalLearnerParam(id = "replace_missing_data_with_x_j_bar", default = FALSE),
      makeLogicalLearnerParam(id = "impute_missingness_with_rf_impute", default = FALSE),
      makeLogicalLearnerParam(id = "impute_missingness_with_x_j_bar_for_lm", default = TRUE),
      makeLogicalLearnerParam(id = "mem_cache_for_speed", default = TRUE),
      makeLogicalLearnerParam(id = "verbose", default = TRUE)
    ),
    properties = c("numerics", "prob", "twoclass", "factors", "missings"),
    name = "Bayesian Additive Regression Trees",
    short.name = "bartmachine",
    note = "The learner can handle missing values in features, but you need to actively switch that behavior on by setting e.g. 'use_missing_data'. See its doc page for further details."
  )
}

#' @export
trainLearner.classif.bartMachine = function(.learner, .task, .subset, .weights = NULL, ...) {
  d = getTaskData(.task, .subset, target.extra = TRUE)
  bartMachine::bartMachine(X = d$data, y = d$target, ...)
}

#' @export
predictLearner.classif.bartMachine = function(.learner, .model, .newdata, ...) {
  levs = c(.model$task.desc$negative, .model$task.desc$positive)
  if(.learner$predict.type == "prob"){
    p = predict(.model$learner.model, new_data = .newdata, type = "prob", ...)
    y = matrix(0, ncol = 2, nrow = nrow(.newdata))
    colnames(y) = levs
    y[, 1L] = 1-p
    y[, 2L] = p
  } else {
    y = predict(.model$learner.model, new_data = .newdata, type = "class", ...)
    y = factor(y, levs)
  }
  bartMachine::destroy_bart_machine(.model$learner.model)
  return(y)
}

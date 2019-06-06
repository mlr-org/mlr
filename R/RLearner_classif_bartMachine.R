# FIXME: I have no idea which routine internally prints to which fucking stream
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
      makeIntegerLearnerParam(id = "num_iterations_after_burn_in", default = 1000L, lower = 0L),
      makeNumericLearnerParam(id = "alpha", default = 0.95, lower = 0),
      makeNumericLearnerParam(id = "beta", default = 2, lower = 0),
      makeNumericLearnerParam(id = "k", default = 2, lower = 0),
      makeNumericLearnerParam(id = "q", default = 0.9, lower = 0, upper = 1),
      makeNumericLearnerParam(id = "prob_rule_class", default = 0.5, lower = 0, upper = 1),
      makeNumericVectorLearnerParam(id = "mh_prob_steps", default = c(2.5, 2.5, 4) / 9, len = 3L),
      makeLogicalLearnerParam(id = "debug_log", default = FALSE, tunable = FALSE),
      makeLogicalLearnerParam(id = "run_in_sample", default = TRUE),
      makeNumericVectorLearnerParam(id = "cov_prior_vec"),
      makeLogicalLearnerParam(id = "use_missing_data", default = FALSE),
      makeIntegerLearnerParam(id = "num_rand_samps_in_library", default = 10000, lower = 1),
      makeLogicalLearnerParam(id = "use_missing_data_dummies_as_covars", default = FALSE),
      makeLogicalLearnerParam(id = "replace_missing_data_with_x_j_bar", default = FALSE),
      makeLogicalLearnerParam(id = "impute_missingness_with_rf_impute", default = FALSE),
      makeLogicalLearnerParam(id = "impute_missingness_with_x_j_bar_for_lm", default = TRUE),
      makeLogicalLearnerParam(id = "mem_cache_for_speed", default = TRUE),
      makeLogicalLearnerParam(id = "serialize", default = FALSE),
      makeIntegerLearnerParam(id = "seed", tunable = FALSE),
      makeLogicalLearnerParam(id = "verbose", default = TRUE, tunable = FALSE)
    ),
    par.vals = list("use_missing_data" = TRUE),
    properties = c("numerics", "prob", "twoclass", "factors", "missings"),
    name = "Bayesian Additive Regression Trees",
    short.name = "bartmachine",
    note = "`use_missing_data` has been set to `TRUE` by default to allow missing data support.",
    callees = c("bartMachine", "predict.bartMachine")
  )
}

#' @export
trainLearner.classif.bartMachine = function(.learner, .task, .subset, .weights = NULL, ...) {
  d = getTaskData(.task, .subset, target.extra = TRUE)
  y = d$target
  td = getTaskDesc(.task)
  levs = c(td$positive, td$negative)
  y = factor(y, levels = levs)
  bartMachine::bartMachine(X = d$data, y = y, ...)
}

#' @export
predictLearner.classif.bartMachine = function(.learner, .model, .newdata, ...) {
  td = .model$task.desc
  levs = c(td$positive, td$negative)
  if (.learner$predict.type == "prob") {
    p = predict(.model$learner.model, new_data = .newdata, type = "prob", ...)
    y = propVectorToMatrix(1 - p, levs)
  } else {
    y = predict(.model$learner.model, new_data = .newdata, type = "class", ...)
    y = factor(y, levs)
  }
  return(y)
}

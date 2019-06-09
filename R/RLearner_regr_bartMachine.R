#' @export
makeRLearner.regr.bartMachine = function() {
  makeRLearnerRegr(
    cl = "regr.bartMachine",
    package = "bartMachine",
    par.set = makeParamSet(
      makeIntegerLearnerParam(id = "num_trees", default = 50L, lower = 1L),
      makeIntegerLearnerParam(id = "num_burn_in", default = 250L, lower = 0L),
      makeIntegerLearnerParam(id = "num_iterations_after_burn_in", default = 1000L, lower = 0L),
      makeNumericLearnerParam(id = "alpha", default = 0.95, lower = 0),
      makeNumericLearnerParam(id = "beta", default = 2, lower = 0),
      makeNumericLearnerParam(id = "k", default = 2, lower = 0),
      makeNumericLearnerParam(id = "q", default = 0.9, lower = 0, upper = 1),
      makeIntegerLearnerParam(id = "nu", default = 3L, lower = 1L),
      makeNumericLearnerParam(id = "prob_rule_class", default = 0.5, lower = 0, upper = 1),
      makeNumericVectorLearnerParam(id = "mh_prob_steps", default = c(2.5, 2.5, 4) / 9, len = 3L),
      makeLogicalLearnerParam(id = "debug_log", default = FALSE, tunable = FALSE),
      makeLogicalLearnerParam(id = "run_in_sample", default = TRUE),
      makeDiscreteLearnerParam(id = "s_sq_y", default = "mse", values = c("mse", "var")),
      makeNumericVectorLearnerParam(id = "cov_prior_vec"),
      makeLogicalLearnerParam(id = "use_missing_data", default = TRUE),
      makeIntegerLearnerParam(id = "num_rand_samps_in_library", default = 10000, lower = 1),
      makeLogicalLearnerParam(id = "use_missing_data_dummies_as_covars", default = FALSE),
      makeLogicalLearnerParam(id = "replace_missing_data_with_x_j_bar", default = FALSE),
      makeLogicalLearnerParam(id = "impute_missingness_with_rf_impute", default = FALSE),
      makeLogicalLearnerParam(id = "impute_missingness_with_x_j_bar_for_lm", default = TRUE),
      makeLogicalLearnerParam(id = "mem_cache_for_speed", default = TRUE),
      makeLogicalLearnerParam(id = "serialize", default = FALSE, tunable = FALSE),
      makeIntegerLearnerParam(id = "seed", tunable = FALSE),
      makeLogicalLearnerParam(id = "verbose", default = TRUE, tunable = FALSE)
    ),
    par.vals = list("use_missing_data" = TRUE),
    properties = c("numerics", "factors", "missings"),
    name = "Bayesian Additive Regression Trees",
    short.name = "bartmachine",
    note = "`use_missing_data` has been set to `TRUE` by default to allow missing data support.",
    callees = c("bartMachine", "predict.bartMachine")
  )
}

#' @export
trainLearner.regr.bartMachine = function(.learner, .task, .subset, .weights = NULL, ...) {
  d = getTaskData(.task, .subset, target.extra = TRUE)
  bartMachine::bartMachine(X = d$data, y = d$target, ...)
}

#' @export
predictLearner.regr.bartMachine = function(.learner, .model, .newdata, ...) {
  predict(.model$learner.model, new_data = .newdata, ...)
}

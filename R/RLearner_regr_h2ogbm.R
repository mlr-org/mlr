#' @export
makeRLearner.regr.h2o.gbm = function() {
  makeRLearnerRegr(
    cl = "regr.h2o.gbm",
    package = "h2o",
    par.set = makeParamSet(
      # See http://docs.h2o.ai/h2o/latest-stable/h2o-docs/data-science/gbm.html
      makeIntegerLearnerParam("ntrees", lower = 1L, default = 50L),
      makeIntegerLearnerParam("max_depth", lower = 1L, default = 5L),
      makeIntegerLearnerParam("min_rows", lower = 1L, default = 10L),
      makeIntegerLearnerParam("nbins", lower = 1L, default = 20L),
      makeIntegerLearnerParam("nbins_cats", lower = 1L, default = 1024),
      makeIntegerLearnerParam("nbins_top_level", lower = 1L, default = 1024),
      makeIntegerLearnerParam("seed", default = -1L, tunable = FALSE),
      makeNumericLearnerParam("learn_rate", lower = 0, upper = 1, default = 0.1),
      makeNumericLearnerParam("learn_rate_annealing", lower = 0, upper = 1, default = 1),
      makeDiscreteLearnerParam("distribution",
        values = c("poisson", "laplace", "tweedie", "gaussian", "huber", "gamma", "quantile"),
        default = "gaussian"),
      makeNumericLearnerParam("sample_rate", lower = 0, upper = 1, default = 1),
      # makeNumericLearnerParam("sample_rate_per_class", lower = 0, upper = 1, default = NULL, special.vals = list(NULL)),
      makeNumericLearnerParam("col_sample_rate", lower = 0, upper = 1, default = 1),
      makeNumericLearnerParam("col_sample_rate_change_per_level", lower = 0, upper = 1, default = 1),
      makeNumericLearnerParam("col_sample_rate_per_tree", lower = 0, upper = 1, default = 1),
      makeNumericLearnerParam("max_abs_leafnode_pred", lower = 0, default = Inf, allow.inf = TRUE),
      makeNumericLearnerParam("pred_noise_bandwidth", lower = 0, default = 0),
      makeDiscreteLearnerParam("categorical_encoding",
        values = c("AUTO", "Enum", "OneHotInternal", "OneHotExplicit", "Binary",
          "Eigen", "LabelEncoder", "SortByResponse"),
        default = "AUTO"),
      makeNumericLearnerParam("min_split_improvement", lower = 0, default = 1e-05),
      makeDiscreteLearnerParam("histogram_type",
        values = c("AUTO", "UniformAdaptive", "Random", "QuantilesGlobal", "RoundRobin"),
        default = "AUTO"),
      makeLogicalLearnerParam("score_each_iteration", default = FALSE, tunable = FALSE),
      makeIntegerLearnerParam("score_tree_interval", lower = 0L, default = 0L, tunable = FALSE),
      makeIntegerLearnerParam("stopping_rounds", lower = 0L, default = 0L, tunable = FALSE),
      makeDiscreteLearnerParam("stopping_metric",
        values = c("AUTO", "deviance", "logloss", "MSE", "RMSE", "MAE", "RMSLE"),
        default = "AUTO", tunable = FALSE),
      makeNumericLearnerParam("stopping_tolerance", lower = 0, upper = Inf, default = 0.001, tunable = FALSE),
      makeNumericLearnerParam("quantile_alpha", lower = 0, upper = 100, default = 0.5,
        requires = expression(distribution == "quantile")),
      makeNumericLearnerParam("tweedie_power", lower = 1, upper = 2, default = 1.5, special.vals = list(0),
        requires = expression(distribution == "tweedie")),
      makeNumericLearnerParam("huber_alpha", lower = 0, upper = 1, default = 0.9,
        requires = expression(distribution == "huber"))
    ),
    properties = c("numerics", "factors", "missings"),
    name = "h2o.gbm",
    short.name = "h2o.gbm",
    note = "",
    callees = "h2o.gbm"
  )
}

#' @export
trainLearner.regr.h2o.gbm = function(.learner, .task, .subset, .weights = NULL, ...) {

  params = list(...)
  # check if h2o connection already exists, otherwise start one
  conn.up = tryCatch(h2o::h2o.getConnection(), error = function(err) {
    return(FALSE)
  })
  if (!inherits(conn.up, "H2OConnection")) {
    h2o::h2o.init()
    options("h2o.use.data.table" = TRUE)
  }
  params$y = getTaskTargetNames(.task)
  params$x = getTaskFeatureNames(.task)
  params$training_frame = getTaskData(.task, subset = .subset)

  if (!is.null(.weights)) {
    params$weights_column = .weights
  }

  params$training_frame = h2o::as.h2o(params$training_frame)

  model = do.call(h2o::h2o.gbm, params)
  return(model)
}

#' @export
predictLearner.regr.h2o.gbm = function(.learner, .model, .newdata, ...) {
  m = .model$learner.model
  h2of = h2o::as.h2o(.newdata)
  p = h2o::h2o.predict(m, newdata = h2of, ...)
  p.df = as.data.frame(p)

  h2o::h2o.rm(h2of)
  h2o::h2o.rm(p)
  return(p.df$predict)
}

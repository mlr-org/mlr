# use_all_factor_levels
# Logical. Use all factor levels of categorical variance. Otherwise the first factor level is omittted (without loss of accuracy). Useful for variable imporotances and auto-enabled for autoencoder.

# train_samples_per_iteration
# Number of training samples (globally) per MapReduce iteration. Special values are: 0 one epoch; -1 all available data (e.g., replicated training data); or -2 auto-tuning (default)

# seed
# Seed for random numbers (affects sampling) - Note: only reproducible when running single threaded

# adaptive_rate
# Logical. Adaptive learning rate (ADAELTA)

# rho
# Adaptive learning rate time decay factor (similarity to prior updates)

# epsilon
# Adaptive learning rate parameter, similar to learn rate annealing during initial training phase. Typical values are between 1.0e-10 and 1.0e-4

# rate
# Learning rate (higher => less stable, lower => slower convergence)

# rate_annealing
# Learning rate annealing: (rate)/(1 + rate_annealing*samples)

# rate_decay
# Learning rate decay factor between layers (N-th layer: rate*α^(N-1))

# momentum_start
# Initial momentum at the beginning of traning (try 0.5)

# momentum_ramp
# Number of training samples for which momentum increases

# momentum_stable
# Final momentum after ther amp is over (try 0.99)

# nesterov_accelerated_gradient
# Logical. Use Nesterov accelerated gradient (recommended)

# input_dropout_ratio
# A fraction of the features for each training row to be omitted from training in order to improve generalization (dimension sampling).

# hidden_dropout_ratios
# Input layer dropout ration (can improve generalization) specify one value per hidden layer, defaults to 0.5

# l1
# L1 regularization (can add stability and improve generalization, cause many weights to become 0)

# l2
# L2 regularization (can add stability and improve generalization, causes many weights to be small)

# max_w2
# Constraint for squared sum of incoming weights per unit (e.g. Rectifier)

# initial_weight_distribution
# Can be "Uniform", "UniformAdaptive", or "Normal"

# initial_weight_scale
# Unifrom: -value ... value, Normal: stddev

# loss
# Loss function: Automatic, CrossEntropy (for classification only), MeanSquare, Absolute (experimental) or Huber (experimental)

# score_interval
# Shortest time interval (in secs) between model scoring

# score_training_samples
# Number of training set samples for scoring (0 for all)

# score_validation_samples
# Number of validation set samples for scoring (0 for all)

# score_duty_cycle
# Maximum duty cycle fraction for scoring (lower: more training, higher: more scoring)

# quiet_mode
# Enable quiet mode for less output to standard output

# class_sampling_factors
# Desired over/under-sampling ratios per class (in lexicographic order). If not specified, sampling factors will be automatically computed to obtain class balance during training. Requires balance_classes.

# max_after_balance_size
# Maximum relative size of the training data after balancing class counts (can be less than 1.0)

# score_validation_sampling
# Method used to sample validation dataset for scoring

# diagnostics
# Enable diagnostics for hidden layers

# variable_importances
# Compute variable importances for input features (Gedeon method) - can be slow for large networks)

# fast_mode
# Enable fast mode (minor approximations in back-propagation)

# ignore_const_cols
# Ignore constant columns (no information can be gained anwyay)

# force_load_balance
# Force extra load balancing to increase training speed for small datasets (to keep all cores busy)

# replicate_training_data
# Replicate the entire training dataset onto every node for faster training

# single_node_mode
# Run on a single node for fine-tuning of model parameters

# shuffle_training_data
# Enable shuffling of training data (recommended if training data is replicated and train_samples_per_iteration is close to numRows*numNodes

# col_major
# Use a column major weight matrix for input layer. Can speed up forward proagation, but might slow down backpropagation (Experimental)

# average_activation
# Average activation for sparse auto-encoder (Experimental)

# sparsity_beta
# Sparsity regularization (Experimental)

# reproducible
# Force reproducibility on small data (will be slow - only uses 1 thread)

# export_weights_and_biases
# Whether to export Neural Network weights and biases to H2O Frames"

# ...
# extra parameters to pass onto functions (not implemented)

###% take code from regr_h2deeplearning

# Details: https://leanpub.com/deeplearning/read

#'@export
makeRLearner.oneclass.h2o.autoencoder = function() {
  makeRLearnerOneClass(
    cl = "oneclass.h2o.autoencoder",
    package = "h2o",
    par.set = makeParamSet(
      # FIXME: hidden can also be a list of integer vectors for grid search
      # instead of makeIntegerVectorLearnerParam("hidden", default = c(200L, 200L), len = NA_integer_, lower = 1L) parametrise hidden
      makeIntegerLearnerParam(id = "layers", lower = 1L, upper = 5L, default = 3L),
      makeIntegerLearnerParam(id = "nodes1", lower = 1L, default = 1L),
      makeIntegerLearnerParam(id = "nodes2", lower = 1L, requires = quote(layers > 1)),
      makeIntegerLearnerParam(id = "nodes3", lower = 1L, requires = quote(layers > 2)),
      makeIntegerLearnerParam(id = "nodes4", lower = 1L, requires = quote(layers > 3)),
      makeIntegerLearnerParam(id = "nodes5", lower = 1L, requires = quote(layers > 4)),

      makeLogicalLearnerParam("use_all_factor_level", default = TRUE),
      makeDiscreteLearnerParam("activation", values = c("Rectifier", "Tanh",
        "TanhWithDropout", "RectifierWithDropout", "MaxoutWithDropout"), default = "Rectifier"), #"Maxout",  not for autoencoder
      makeNumericLearnerParam("epochs", default = 10L, lower = 1), # doc says can be fractional
      makeNumericLearnerParam("train_samples_per_iteration", default = -2, lower = -2),
      makeIntegerLearnerParam("seed", tunable = FALSE),
      makeLogicalLearnerParam("adaptive_rate", default = TRUE),
      makeNumericLearnerParam("rho", default = 0.99, lower = 0), # is there a upper limit for this?
      makeNumericLearnerParam("epsilon", default = 1e-08, lower = 1e-10, upper = 1e-4),
      makeNumericLearnerParam("rate", default = 0.005, lower = 0, upper = 1),
      makeNumericLearnerParam("rate_annealing", default = 1e-06, lower = 0),
      makeNumericLearnerParam("rate_decay", default = 1, lower = 0),
      makeNumericLearnerParam("momentum_start", default = 0),
      makeNumericLearnerParam("momentum_ramp", default = 1e+06),
      makeNumericLearnerParam("momentum_stable", default = 0),
      makeLogicalLearnerParam("nesterov_accelerated_gradient", default = TRUE),
      makeNumericLearnerParam("input_dropout_ratio", default = 0),
      makeNumericVectorLearnerParam("hidden_dropout_ratios", default = 0.5),
      makeNumericLearnerParam("l1", default = 0),
      makeNumericLearnerParam("l2", default = 0),
      makeNumericLearnerParam("max_w2", default = Inf, allow.inf = TRUE),
      makeDiscreteLearnerParam("initial_weight_distribution",
        values = c("UniformAdaptive", "Uniform", "Normal"), default = "UniformAdaptive"),
      makeNumericLearnerParam("initial_weight_scale", default = 1),
      makeDiscreteLearnerParam("loss", values = c("Automatic", "Quadratic",
        "Absolute", "Huber", "Quantile")), # "CrossEntropy" not for autoencoder
      makeDiscreteLearnerParam("distribution", values = c("AUTO", "gaussian",
        "bernoulli", "multinomial", "poisson", "gamma", "tweedie", "laplace",
        "huber", "quantile"), default = "AUTO"),
      makeNumericLearnerParam("quantile_alpha", default = 0.5, lower = 0, upper = 1,
        requires = quote(distribution == "quantile")),
      makeNumericLearnerParam("score_interval", default = 5),
      makeIntegerLearnerParam("score_training_samples", default = 10000),
      makeIntegerLearnerParam("score_validation_samples", default = 0),
      makeNumericLearnerParam("score_duty_cycle", default = 0.1),
      makeIntegerLearnerParam("stopping_rounds", default = 5L, lower = 0L),
      makeDiscreteLearnerParam("stopping_metric", values = c("AUTO", "deviance", "logloss",
        "MSE", "AUC", "r2", "misclassification"), default = "AUTO",
        requires = quote(stopping_rounds > 0L)),
      makeNumericLearnerParam("stopping_tolerance", default = 0, lower = 0),
      makeNumericLearnerParam("max_runtime_secs", default = 0, lower = 0),
      makeLogicalLearnerParam("quiet_mode", tunable = FALSE),
      makeNumericLearnerParam("class_sampling_factors", requires = quote(balance_classes == TRUE)),
      makeNumericLearnerParam("max_after_balance_size", default = 5),
      makeDiscreteLearnerParam("score_validation_sampling", values = c("Uniform",
        "Stratified"), default = "Uniform"),
      makeLogicalLearnerParam("diagnostics", default = TRUE, tunable = FALSE),
      makeLogicalLearnerParam("variable_importances", default = TRUE, tunable = FALSE),
      makeLogicalLearnerParam("fast_mode", default = TRUE, tunable = FALSE),
      makeLogicalLearnerParam("ignore_const_cols", default = TRUE, tunable = FALSE),
      makeLogicalLearnerParam("force_load_balance", default = TRUE, tunable = FALSE),
      makeLogicalLearnerParam("replicate_training_data", default = TRUE, tunable = FALSE),
      makeLogicalLearnerParam("single_node_mode", default = FALSE, tunable = FALSE),
      makeLogicalLearnerParam("shuffle_training_data", tunable = FALSE),
      makeLogicalLearnerParam("sparse", default = FALSE, tunable = FALSE),
      makeLogicalLearnerParam("col_major", default = FALSE, tunable = FALSE),
      makeLogicalLearnerParam("average_activation", tunable = FALSE),
      makeLogicalLearnerParam("sparsity_beta", tunable = FALSE),
      makeLogicalLearnerParam("reproducible", default = FALSE, tunable = FALSE),
      makeLogicalLearnerParam("export_weights_and_biases", default = FALSE, tunable = FALSE),
      makeIntegerLearnerParam("mini_batch_size", default = 1)
    ),
    properties = c("oneclass", "numerics", "factors", "weights", "prob"),
    note = "Input variable 'autoencoder' is set to 'TRUE'. And input variable
    'hidden' is parameterise by 'layers', 'nodes1', 'nodes2', 'nodes3', 'nodes4', 'nodes5'.
    'layers' is the number of layer and the 'nodes' variable are indicating the
    number of nodes in each layer (1:5). For now maximal three layers are possible",
    name = "h2o.autoencoder",
    short.name = "h2o.ae",
    callees = c("h2o.deeplearning", "h2o.predict", "h2o.anomaly")
  )
}

#' @export
trainLearner.oneclass.h2o.autoencoder = function(.learner, .task, .subset, .weights = NULL,
  layers = 1L, nodes1 = 200L, nodes2 = NULL, nodes3 = NULL, nodes4 = NULL, nodes5 = NULL, ...) {
  hidden = c(nodes1, nodes2, nodes3, nodes4, nodes5)[1:layers]
  # check if h2o connection already exists, otherwise start one
  conn.up = tryCatch(h2o::h2o.getConnection(), error = function(err) return(FALSE))
  if (!inherits(conn.up, "H2OConnection")) {
    h2o::h2o.init()
  }
  x = getTaskFeatureNames(.task)
  d = getTaskData(.task, subset = .subset)[, x]
  wcol = NULL
  if (!is.null(.weights)) {
    d$.mlr.weights = .weights
    wcol = ".mlr.weights"
  }
  h2of = h2o::as.h2o(d, destination_frame = "train.hex")
  h2o::h2o.deeplearning(x = x, training_frame = h2of, weights_column = wcol, autoencoder = TRUE, hidden = hidden, ...)
}

#' @export
predictLearner.oneclass.h2o.autoencoder = function(.learner, .model, .newdata, ...) {
  m = .model$learner.model
  h2of = h2o::as.h2o(.newdata)
  # usually low scores are indicator for anomaly
  # here: low scores = low mse reconstruction error = indicator for normal data
  p = h2o::h2o.anomaly(m, data = h2of, per_feature = FALSE)
  p.df = as.matrix(p)
  td = getTaskDesc(.model)
  label = c(td$positive, td$negative)
  if (.learner$predict.type == "response"){
    # per default assume 5% anomalies
    indices.threshold = order(p.df)[round(length(p.df) * 0.95)]  #mse reconstruction error in [0,inf[
    predict.threshold = p.df[indices.threshold]
    p = p.df >= predict.threshold
    p = factor(p, levels = c("TRUE", "FALSE"), labels = label)
  } else {
    # usually low scores are indicator for anomaly
    # here: low scores = low mse reconstruction error = indicator for normal data
    p = convertingScoresToProbability(p.df)$probability
    p = cbind(p, 1 - p) # p.df = mse.reconstruction error = high = anomaly (same for prob)
    colnames(p) = label
  }
  return(p)
}

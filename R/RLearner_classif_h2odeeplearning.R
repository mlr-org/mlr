




# autoencoder
# Enable auto-encoder for model building.

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

# classification_stop
# Stopping criterion for classification error fraction on training data (-1 to disable)

# regression_stop
# Stopping criterion for regression error (MSE) on training data (-1 to disable)

# quiet_mode
# Enable quiet mode for less output to standard output

# max_confusion_matrix_size
# Max. size (number of classes) for confusion matrices to be shown

# max_hit_ratio_k
# Max number (top K) of predictions to use for hit ration computation(for multi-class only, 0 to disable)

# balance_classes
# Balance training data class counts via over/under-sampling (for imbalanced data)

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

# max_categorical_features
# Max. number of categorical features, enforced via hashing Experimental)

# reproducible
# Force reproducibility on small data (will be slow - only uses 1 thread)

# export_weights_and_biases
# Whether to export Neural Network weights and biases to H2O Frames"

# ...
# extra parameters to pass onto functions (not implemented)



# h2o.deeplearning(x, y, training_frame, model_id = "",
#   overwrite_with_best_model, n_folds = 0, validation_frame, checkpoint,
#   autoencoder = FALSE, use_all_factor_levels = TRUE,
#   activation = c("Rectifier", "Tanh", "TanhWithDropout",
#   "RectifierWithDropout", "Maxout", "MaxoutWithDropout"), hidden = c(200,
#   200), epochs = 10, train_samples_per_iteration = -2, seed,
#   adaptive_rate = TRUE,
#   rho = 0.99,
#   epsilon = 1e-08,
#   rate = 0.005,
#   rate_annealing = 1e-06,
#   rate_decay = 1,
#   momentum_start = 0,
#   momentum_ramp = 1e+06,
#   momentum_stable = 0,
#   nesterov_accelerated_gradient = TRUE,
#   input_dropout_ratio = 0,
#   hidden_dropout_ratios,
#   l1 = 0,
#   l2 = 0,
#   max_w2 = Inf,
#   initial_weight_distribution = c("UniformAdaptive", "Uniform", "Normal"),
#   initial_weight_scale = 1,
#   loss = c("Automatic", "CrossEntropy", "MeanSquare", "Absolute", "Huber"),
#   score_interval = 5,
#   score_training_samples,
#   score_validation_samples,
#   score_duty_cycle,
#   classification_stop,
#   regression_stop,
#   quiet_mode,
#   max_confusion_matrix_size,
#   max_hit_ratio_k,
#   balance_classes = FALSE,
#   class_sampling_factors,
#   max_after_balance_size,
#   score_validation_sampling,
#   diagnostics,
#   variable_importances,
#   fast_mode,
#   ignore_const_cols,
#   force_load_balance,
#   replicate_training_data,
#   single_node_mode,
#   shuffle_training_data,
#   sparse,
#   col_major,
#   average_activation,
#   sparsity_beta,
#   max_categorical_features,
#   reproducible = FALSE,
#   export_weights_and_biases = FALSE, ...)



makeRLearner.classif.h2odeeplearning = function() {
  makeRLearnerClassif(
    cl = "classif.h2odeeplearning",
    package = "h2o",
    par.set = makeParamSet(
    makeLogicalLearnerParam("autoencoder", default = FALSE),
    makeLogicalLearnerParam("use_all_factor_level", default = TRUE),
    makeDiscreteParam(activation = c("Rectifier", "Tanh", "TanhWithDropout", "RectifierWithDropout", "Maxout", "MaxoutWithDropout")),
    makeIntegerVectorLearnerParam("hidden", default = c(200L, 200)),
    makeNumericLearnerParam("epochs", default = 10L), # doc says can be fractional
    train_samples_per_iteration = -2, seed,
#   adaptive_rate = TRUE, rho = 0.99, epsilon = 1e-08, rate = 0.005,
#   rate_annealing = 1e-06,
    #   rate_decay = 1,
    #   momentum_start = 0,
#   momentum_ramp = 1e+06,
    #   momentum_stable = 0,
#   nesterov_accelerated_gradient = TRUE,
    #   input_dropout_ratio = 0,
#   hidden_dropout_ratios,
    makeNumericLearnerParam("l1", default = 0),
    makeNumericLearnerParam("l2", default = 0),
    makeNumericLearnerParam("max_w2", default = Inf),
    makeDiscreteLearnerParam("initial_weight_distribution", values = c("UniformAdaptive", "Uniform", "Normal"), default = "UniformAdaptive"),
    #   initial_weight_scale = 1,
    makeDiscreteParam("loss", default = c("Automatic", "CrossEntropy", "MeanSquare", "Absolute", "Huber")),
      score_interval = 5,
#   score_training_samples, score_validation_samples, score_duty_cycle,
#   classification_stop,
    #   regression_stop,
    # makeLogicalLearnerParam("quiet_mode", default = ??),

    #   max_confusion_matrix_size,
#   max_hit_ratio_k,
    makeLogicalLearnerParam("balance_classes", default = FALSE),

    #   class_sampling_factors,
#   max_after_balance_size,
    #   score_validation_sampling,
    #   diagnostics,
#   variable_importances,
    #   fast_mode,
    #   ignore_const_cols,
    #   force_load_balance,
    ),
    properties = c("twoclass", "multiclass", "numerics", "factors", "prob"),
    name = "h2o.deeplearning",
    short.name = "h2o.dl",
    note = ""
  )
}

#' @export
trainLearner.classif.h2odeeplearning = function(.learner, .task, .subset, .weights = NULL,  ...) {
  y = getTaskTargetNames(.task)
  x = getTaskFeatureNames(.task)
  d = getTaskData(.task, subset = .subset)
  h2of = as.h2o(d)
  h2o.deeplearning(y = y, x = x, training_frame = h2of, ...)
}

#' @export
predictLearner.classif.h2odeeplearning = function(.learner, .model, .newdata, ...) {
  m = .model$learner.model
  h2of = as.h2o(.newdata)
  p = h2o::h2o.predict(m, newdata = h2of, ...)
  p.df = as.data.frame(p)
  if (.learner$predict.type == "response") {
    return(p.df$predict)
  } else {
    p.df$predict = NULL
    return(p.df)
  }
}



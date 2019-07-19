#' @title Generate feature importance.
#'
#' @description
#' Estimate how important individual features or groups of features are by contrasting prediction performances. For method \dQuote{permutation.importance} compute the change in performance from permuting the values of a feature (or a group of features) and compare that to the predictions made on the unmcuted data.
#'
#' @family generate_plot_data
#' @aliases FeatureImportanceData
#'
#' @template arg_task
#' @param method (`character(1)`)\cr
#'   The method used to compute the feature importance.
#'   The only method available is \dQuote{permutation.importance}.
#'   Default is \dQuote{permutation.importance}.
#' @template arg_learner
#' @param features ([character])\cr
#'   The features to compute the importance of.
#'   The default is all of the features contained in the [Task].
#' @param interaction (`logical(1)`)\cr
#'   Whether to compute the importance of the `features` argument jointly.
#'   For `method = "permutation.importance"` this entails permuting the values of
#'   all `features` together and then contrasting the performance with that of
#'   the performance without the features being permuted.
#'   The default is `FALSE`.
#' @template arg_measure
#' @param contrast (`function`)\cr
#'   A difference function that takes a numeric vector and returns a numeric vector
#'   of the same length.
#'   The default is element-wise difference between the vectors.
#' @param aggregation (`function`)\cr
#'   A function which aggregates the differences.
#'   This function must take a numeric vector and return a numeric vector of length 1.
#'   The default is `mean`.
#' @param nmc (`integer(1)`)\cr
#'   The number of Monte-Carlo iterations to use in computing the feature importance.
#'   If `nmc == -1` and `method = "permutation.importance"` then all
#'   permutations of the `features` are used.
#'   The default is 50.
#' @param replace (`logical(1)`)\cr
#'   Whether or not to sample the feature values with or without replacement.
#'   The default is `TRUE`.
#' @param local (`logical(1)`)\cr
#'   Whether to compute the per-observation importance.
#'   The default is `FALSE`.
#' @param show.info (`logical(1)`)\cr
#'   Whether progress output (feature name, time elapsed) should be displayed.
#'
#' @return (`FeatureImportance`). A named list which contains the computed feature importance and the input arguments.
#'
#' Object members:
#' \item{res}{([data.frame])\cr
#'   Has columns for each feature or combination of features (colon separated) for which the importance is computed.
#'   A row coresponds to importance of the feature specified in the column for the target.
#' }
#' \item{interaction}{(`logical(1)`)\cr
#'   Whether or not the importance of the `features` was computed jointly rather than individually.
#' }
#' \item{measure}{([Measure])}\cr
#'   The measure used to compute performance.
#' \item{contrast}{(`function`)\cr
#'   The function used to compare the performance of predictions.
#' }
#' \item{aggregation}{(`function`)\cr
#'   The function which is used to aggregate the contrast between the performance of predictions across Monte-Carlo iterations.
#' }
#' \item{replace}{(`logical(1)`)\cr
#'   Whether or not, when `method = "permutation.importance"`, the feature values
#'   are sampled with replacement.
#' }
#' \item{nmc}{(`integer(1)`)\cr
#'   The number of Monte-Carlo iterations used to compute the feature importance.
#'   When `nmc == -1` and `method = "permutation.importance"` all permutations are used.
#' }
#' \item{local}{(`logical(1)`)\cr
#'   Whether observation-specific importance is computed for the `features`.
#' }
#'
#' @examples
#'
#' lrn = makeLearner("classif.rpart", predict.type = "prob")
#' fit = train(lrn, iris.task)
#' imp = generateFeatureImportanceData(iris.task, "permutation.importance",
#'   lrn, "Petal.Width", nmc = 10L, local = TRUE)
#' @references Jerome Friedman; Greedy Function Approximation: A Gradient Boosting Machine, Annals of Statistics, Vol. 29, No. 5 (Oct., 2001), pp. 1189-1232.
#' @export
generateFeatureImportanceData = function(task, method = "permutation.importance",
  learner, features = getTaskFeatureNames(task), interaction = FALSE, measure,
  contrast = function(x, y) x - y, aggregation = mean, nmc = 50L, replace = TRUE,
  local = FALSE, show.info = FALSE) {

  learner = checkLearner(learner)
  measure = checkMeasures(measure, learner)
  if (length(measure) > 1L) {
    stop("only one measure is allowed.")
  }
  if (getTaskType(task) != learner$type) {
    stopf("Expected task of type '%s', not '%s'", getTaskType(task), learner$type)
  }
  assertCount(nmc)
  test.contrast = contrast(1, 1)
  if (!(is.numeric(test.contrast))) {
    stop("the contrast function must return a numeric vector.")
  }
  if (!length(test.contrast) == 1L) {
    stop("the contrast function must return a numeric vector the same length as the input.")
  }
  test.aggregation = aggregation(1:2)
  if (!is.numeric(test.aggregation)) {
    stop("aggregation argument doesn't return a numeric vector.")
  }
  if (!(length(test.aggregation) == 1L)) {
    stop("aggregation function must either return 1 number or a numeric vector of the same length as the number of rows in the task data.frame.")
  }

  out = switch(method,
    "permutation.importance" = doPermutationImportance(
      task, learner, features, interaction, measure, contrast, aggregation, nmc, replace, local, show.info)
  )

  makeS3Obj(
    "FeatureImportance",
    res = out,
    task.desc = getTaskDesc(task),
    interaction = interaction,
    learner = learner,
    measure = measure,
    contrast = contrast,
    aggregation = aggregation,
    nmc = nmc,
    replace = replace,
    local = local
  )
}

doPermutationImportance = function(task, learner, features, interaction, measure,
  contrast, aggregation, nmc, replace, local, show.info) {

  ## train learner to get baseline performance
  fit = train(learner, task)

  # compute unmcuted performance
  pred = predict(fit, task = task)
  if (local) {
    # subset the prediction data element to compute the per-observation performance
    perf = vnapply(1:getTaskSize(task), function(i) {
      pred$data = pred$data[i, ]
      performance(pred, measure)
    })
    perf = as.numeric(perf)
  } else {
    perf = performance(pred, measure)
  }

  data = getTaskData(task)

  ## indices for resampled data to be used for permuting features
  if (nmc == -1L) {
    ## from http://stackoverflow.com/questions/11095992/generating-all-distinct-permutations-of-a-list-in-r
    permutations = function(n) {
      if (n == 1L) {
        return(matrix(1L))
      } else {
        sp = permutations(n - 1L)
        p = nrow(sp)
        A = matrix(nrow = n, ncol = n * p)
        for (i in 1:n) {
          A[, (i - 1) * p + 1:p] = rbind(i, sp + (sp >= i))
        }
        return(A)
      }
    }
    indices = permutations(getTaskSize(task))
  } else {
    indices = replicate(nmc, sample.int(getTaskSize(task), replace = replace))
  }

  args = list(measure = measure, contrast = contrast, data = data,
    perf = perf, fit = fit, indices = indices)

  doPermutationImportanceIteration = function(perf, fit, data, measure,
    contrast, indices, i, x, progress) {
    data[, x] = data[indices[, i], x]

    if (local) {
      perf.permuted = lapply(seq_len(getTaskSize(task)), function(i, pred) {
        pred$data = pred$data[i, ]
        performance(pred, measure)
      }, pred = predict(fit, newdata = data))
      perf.permuted = as.numeric(perf.permuted)
    } else {
      perf.permuted = performance(predict(fit, newdata = data), measure)
    }
    contrast(perf.permuted, perf)
  }

  if (interaction) {
    args$x = features
    out = parallelMap(doPermutationImportanceIteration, i = seq_len(nmc), more.args = args)
    out = do.call("rbind", out)
    out = as.matrix(apply(out, 2, aggregation))
    out = as.data.frame(out)
    colnames(out) = stri_paste(features, collapse = ":")
  } else {
    if (isTRUE(show.info)) {
      time = Sys.time()
    }
    out = lapply(features, function(x) {
      if (isTRUE(show.info)) {
        cat(sprintf("Feature: '%s' [%s/%s, %s min]\n", x, match(x, features),
          length(features), round(difftime(Sys.time(), time, units = "mins"), 2)))
      }
      parallelMap(doPermutationImportanceIteration, i = seq_len(nmc), more.args = c(args, x = x))
    })
    out = lapply(out, function(x) apply(do.call("rbind", x), 2, aggregation))
    out = t(do.call("rbind", out))
    out = as.data.frame(out)
    colnames(out) = features
  }
  out
}

#' @export
print.FeatureImportance = function(x, ...) {

  catf("FeatureImportance:")
  catf("Task: %s", x$task.desc$id)
  catf("Interaction: %s", x$interaction)
  catf("Learner: %s", x$learner$id)
  catf("Measure: %s", ifelse(!is.na(x$measure), x$measure[[1]]$id, NA))
  catf("Contrast: %s", stri_paste(format(x$contrast), collapse = " "))
  catf("Aggregation: %s", stri_paste(format(x$aggregation), collapse = " "))
  catf("Replace: %s", x$replace)
  catf("Number of Monte-Carlo iterations: %s", x$nmc)
  catf("Local: %s", x$local)
  print(head(x$res))
}

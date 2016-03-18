.FilterRegister = new.env()

#' Create a feature filter.
#'
#' Creates and registers custom feature filters. Implemented filters
#' can be listed with \code{\link{listFilterMethods}}.
#'
#' @param name [\code{character(1)}]\cr
#'  Identifier for the filter.
#' @param desc [\code{character(1)}]\cr
#'  Short description of the filter.
#' @param pkg [\code{character(1)}]\cr
#'  Source package where the filter is implemented.
#' @param supported.tasks [\code{character}]\cr
#'  Task types supported.
#' @param supported.features [\code{character}]\cr
#'  Feature types supported.
#' @param fun [\code{function(task, nselect, ...}]\cr
#'  Function which takes a task and returns a named numeric vector of scores,
#'  one score for each feature of \code{task}.
#'  Higher scores mean higher importance of the feature.
#'  At least \code{nselect} features must be calculated, the remaining may be
#'  set to \code{NA} or omitted, and thus will not be selected.
#'  the original order will be restored if necessary.
#' @return Object of class \dQuote{Filter}.
#' @export
makeFilter = function(name, desc, pkg, supported.tasks, supported.features, fun) {
  assertString(name)
  assertString(desc)
  assertCharacter(pkg, any.missing = FALSE)
  assertCharacter(supported.tasks, any.missing = FALSE)
  assertCharacter(supported.features, any.missing = FALSE)
  assertFunction(fun, c("task", "nselect"))
  obj =  makeS3Obj("Filter",
    name = name,
    desc = desc,
    pkg = pkg,
    supported.tasks = supported.tasks,
    supported.features = supported.features,
    fun = fun
  )
  .FilterRegister[[name]] = obj
  obj
}

#' List filter methods.
#'
#' Returns a subset-able dataframe with filter information.
#'
#' @param desc [\code{logical(1)}]\cr
#'  Provide more detailed information about filters.
#' @param tasks [\code{logical(1)}]\cr
#'  Provide information on supported tasks.
#' @param features [\code{logical(1)}]\cr
#'  Provide information on supported features.
#' @return [\code{data.frame}].
#' @export
listFilterMethods = function(desc = TRUE, tasks = FALSE, features = FALSE) {
  tag2df = function(tags, prefix = "") {
    unique.tags = sort(unique(unlist(tags)))
    res = asMatrixRows(lapply(tags, "%in%", x = unique.tags))
    colnames(res) = paste0(prefix, unique.tags)
    rownames(res) = NULL
    as.data.frame(res)
  }
  assertFlag(desc)
  assertFlag(tasks)
  assertFlag(features)

  filters = as.list(.FilterRegister)
  df = data.frame(
    id = names(filters),
    package = vcapply(extractSubList(filters, "pkg"), collapse)
  )
  if (desc)
    df$desc = extractSubList(filters, "desc")
  if (tasks)
    df = cbind(df, tag2df(extractSubList(filters, "supported.tasks"), prefix = "task."))
  if (features)
    df = cbind(df, tag2df(extractSubList(filters, "supported.features"), prefix = "feature."))
  setRowNames(sortByCol(df, "id"), NULL)
}

#' @export
print.Filter = function(x, ...) {
  catf("Filter: '%s'", x$name)
  if (!isScalarNA(x$pkg))
    catf("Packages: '%s'", collapse(cleanupPackageNames(x$pkg)))
  catf("Supported tasks: %s", collapse(x$supported.tasks))
  catf("Supported features: %s", collapse(x$supported.features))
}

makeFilter(
  name = "mrmr",
  desc = "Minimum redundancy, maximum relevance filter",
  pkg  = "mRMRe",
  supported.tasks = c("classif", "regr", "surv"),
  supported.features = c("numerics", "factors"),
  fun = function(task, nselect, ...) {
    if (inherits(task, "SurvTask")) {
      data = getTaskData(task, target.extra = TRUE, recode.target = "rcens")
      data = cbind(..surv = data$target, data$data)
      target.ind = 1L
    } else {
      data = getTaskData(task)
      target.ind = match(getTaskTargetNames(task), colnames(data))
    }

    # some required conversions
    ind = vlapply(data, is.factor)
    data[ind] = lapply(data[ind], as.ordered)
    ind = which(vlapply(data, is.integer))
    data[ind] = lapply(data[ind], as.double)
    data = mRMRe::mRMR.data(data = data)

    threads.before = mRMRe::get.thread.count()
    on.exit(mRMRe::set.thread.count(threads.before))
    mRMRe::set.thread.count(1L)
    res = mRMRe::mRMR.classic(data = data, target_indices = target.ind, feature_count = nselect, ...)
    scores = as.numeric(mRMRe::scores(res)[[1L]])
    setNames(scores, res@feature_names[as.integer(mRMRe::solutions(res)[[1L]])])
})

makeFilter(
  name = "carscore",
  desc = "CAR scores",
  pkg  = "care",
  supported.tasks = "regr",
  supported.features = "numerics",
  fun = function(task, nselect, ...) {
    data = getTaskData(task, target.extra = TRUE)
    y = care::carscore(Xtrain = data$data, Ytrain = data$target, verbose = FALSE, ...)^2
    setNames(as.double(y), names(y))
  }
)

makeFilter(
  name = "rf.importance",
  desc = "Importance of random forests fitted in package 'randomForestSRC'. Importance is calculated using argument 'permute'.",
  pkg  = "randomForestSRC",
  supported.tasks = c("classif", "regr", "surv"),
  supported.features = c("numerics", "factors"),
  fun = function(task, nselect, ...) {
    im = randomForestSRC::rfsrc(getTaskFormula(task), data = getTaskData(task), proximity = FALSE, forest = FALSE, importance = "permute", ...)$importance
    if (inherits(task, "ClassifTask")) {
      ns = rownames(im)
      y = im[, "all"]
    } else {
      ns = names(im)
      y = unname(im)
    }
    setNames(y, ns)
  }
)

makeFilter(
  name = "rf.min.depth",
  desc = "Minimal depth of random forest fitted in package 'randomForestSRC'",
  pkg  = "randomForestSRC",
  supported.tasks = c("classif", "regr", "surv"),
  supported.features = c("numerics", "factors"),
  fun = function(task, nselect, ...) {
    im = randomForestSRC::var.select(getTaskFormula(task), getTaskData(task),
      method = "md", verbose = FALSE, ...)$md.obj$order
    setNames(-im[, 1L], rownames(im))
  }
)

makeFilter(
  name = "cforest.importance",
  desc = "Permutation importance of random forest fitted in package 'party'",
  pkg = "party",
  supported.tasks = c("classif", "regr", "surv"),
  supported.features = c("numerics", "factors"),
  fun = function(task, nselect, mtry = 5L, ...) {
    args = list(...)
    # we need to set mtry, which is 5 by default in cforest, to p if p < mtry
    # otherwise we get a warning
    p = getTaskNFeats(task)
    if (p < mtry)
      args$mtry = p
    cforest_args = as.list(base::args(party::cforest))
    cforest_args = args[names(args) %in% names(cforest_args)]
    control_args = as.list(base::args(party::cforest_control))
    control_args = args[names(args) %in% names(control_args)]
    varimp_args = as.list(base::args(party::varimp))
    varimp_args = args[names(args) %in% names(varimp_args)]
    ctrl = do.call(party::cforest_unbiased, control_args)
    fit = do.call(party::cforest, c(list(formula = getTaskFormula(task), data = getTaskData(task), controls = ctrl),
                                    cforest_args))
    im = do.call(party::varimp, c(list(obj = fit), varimp_args))
    im
  }
)

makeFilter(
  name = "linear.correlation",
  desc = "Pearson correlation between feature and target",
  pkg  = "FSelector",
  supported.tasks = "regr",
  supported.features = "numerics",
  fun = function(task, nselect, ...) {
    y = FSelector::linear.correlation(getTaskFormula(task), data = getTaskData(task))
    setNames(y[["attr_importance"]], getTaskFeatureNames(task))
  }
)

makeFilter(
  name = "rank.correlation",
  desc = "Spearman's correlation between feature and target",
  pkg  = "FSelector",
  supported.tasks = c("regr"),
  supported.features = c("numerics", "factors"),
  fun = function(task, nselect, ...) {
    y = FSelector::rank.correlation(getTaskFormula(task), data = getTaskData(task))
    setNames(y[["attr_importance"]], getTaskFeatureNames(task))
  }
)

makeFilter(
  name = "information.gain",
  desc = "Entropy-based information gain between feature and target",
  pkg  = "FSelector",
  supported.tasks = c("classif", "regr"),
  supported.features = c("numerics", "factors"),
  fun = function(task, nselect, ...) {
    y = FSelector::information.gain(getTaskFormula(task), data = getTaskData(task))
    setNames(y[["attr_importance"]], getTaskFeatureNames(task))
  }
)

makeFilter(
  name = "gain.ratio",
  desc = "Entropy-based gain ratio between feature and target",
  pkg  = "FSelector",
  supported.tasks = c("classif", "regr"),
  supported.features = c("numerics", "factors"),
  fun = function(task, nselect, ...) {
    y = FSelector::gain.ratio(getTaskFormula(task), data = getTaskData(task))
    setNames(y[["attr_importance"]], getTaskFeatureNames(task))
  }
)

makeFilter(
  name = "symmetrical.uncertainty",
  desc = "Entropy-based symmetrical uncertainty between feature and target",
  pkg  = "FSelector",
  supported.tasks = c("classif", "regr"),
  supported.features = c("numerics", "factors"),
  fun = function(task, nselect, ...) {
    y = FSelector::symmetrical.uncertainty(getTaskFormula(task), data = getTaskData(task))
    setNames(y[["attr_importance"]], getTaskFeatureNames(task))
  }
)

makeFilter(
  name = "chi.squared",
  desc = "Chi-squared statistic of independence between feature and target",
  pkg  = "FSelector",
  supported.tasks = c("classif", "regr"),
  supported.features = c("numerics", "factors"),
  fun = function(task, nselect, ...) {
    y = FSelector::chi.squared(getTaskFormula(task), data = getTaskData(task))
    setNames(y[["attr_importance"]], getTaskFeatureNames(task))
  }
)

makeFilter(
  name = "relief",
  desc = "RELIEF algorithm",
  pkg  = "FSelector",
  supported.tasks = c("classif", "regr"),
  supported.features = c("numerics", "factors"),
  fun = function(task, nselect, ...) {
    y = FSelector::relief(getTaskFormula(task), data = getTaskData(task))
    setNames(y[["attr_importance"]], getTaskFeatureNames(task))
  }
)

makeFilter(
  name = "oneR",
  desc = "oneR association rule",
  pkg  = "FSelector",
  supported.tasks = c("classif", "regr"),
  supported.features = c("numerics", "factors"),
  fun = function(task, nselect, ...) {
    y = FSelector::oneR(getTaskFormula(task), data = getTaskData(task))
    setNames(y[["attr_importance"]], getTaskFeatureNames(task))
  }
)

univariate = makeFilter(
  name = "univariate.model.score",
  desc = "Resamples an mlr learner for each input feature individually. The resampling performance is used as filter score, with rpart as default learner.",
  pkg  = character(0L),
  supported.tasks = c("classif", "regr", "surv"),
  supported.features = c("numerics", "factors"),
  fun = function(task, nselect, perf.learner = NULL, perf.measure = NULL, perf.resampling = NULL, ...) {
    typ = getTaskType(task)
    if (is.null(perf.learner))
      if (typ == "classif")
        perf.learner = "classif.rpart"
      else if (typ == "regr")
        perf.learner = "regr.rpart"
      else if (typ == "surv")
        perf.learner = "surv.rpart"
    if (is.null(perf.measure))
      perf.measure = getDefaultMeasure(task)
    perf.learner = checkLearner(perf.learner)
    perf.measure = checkMeasures(perf.measure, perf.learner)
    if (length(perf.measure) != 1L)
      stop("Exactly one measure must be provided")
    if (is.null(perf.resampling))
      perf.resampling = makeResampleDesc("Subsample", iters = 1L, split = 0.67)
    if (getTaskType(task) != perf.learner$type)
      stopf("Expected task of type '%s', not '%s'", getTaskType(task), perf.learner$type)

    fns = getTaskFeatureNames(task)
    res = double(length(fns))
    for (i in seq_along(fns)) {
      subtask = subsetTask(task, features = fns[i])
      res[i] = resample(learner = perf.learner, task = subtask, resampling = perf.resampling, measures = perf.measure, keep.pred = FALSE, show.info = FALSE)$aggr
    }
    if (perf.measure[[1L]]$minimize)
      res = -1.0 * res
    setNames(res, fns)
  }
)
.FilterRegister[["univariate"]] = univariate
.FilterRegister[["univariate"]]$desc = "Resamples an mlr learner for each input feature individually. The resampling performance is used as filter score, with rpart as default learner (DEPRECATED)."
.FilterRegister[["univariate"]]$fun = function(...) {
  .Deprecated(old = "Filter 'univariate'", new = "Filter 'univariate.model.score'")
  .FilterRegister[["univariate.model.score"]]$fun(...)
}

makeFilter(
  name = "anova.test",
  desc = "ANOVA Test for binary and multiclass classification tasks",
  pkg = character(0L),
  supported.tasks = c("classif"),
  supported.features = c("numerics"),
  fun = function(task, nselect, ...) {
    data = getTaskData(task)
    sapply(getTaskFeatureNames(task), function(feat.name) {
      f = as.formula(paste0(feat.name,"~",getTaskTargetNames(task)))
      aov.t = aov(f, data = data)
      summary(aov.t)[[1]][1,'F value']
    })
  }
)

makeFilter(
  name = "kruskal.test",
  desc = "Kruskal Test for binary and multiclass classification tasks",
  pkg = character(0L),
  supported.tasks = c("classif"),
  supported.features = c("numerics", "factors"),
  fun = function(task, nselect, ...) {
    data = getTaskData(task)
    sapply(getTaskFeatureNames(task), function(feat.name) {
      f = as.formula(paste0(feat.name,"~", getTaskTargetNames(task)))
      t = kruskal.test(f, data = data)
      unname(t$statistic)
    })
  }
)

makeFilter(
  name = "variance",
  desc = "A simple variance filter",
  pkg = character(0L),
  supported.tasks = c("classif", "regr", "surv"),
  supported.features = c("numerics"),
  fun = function(task, nselect, na.rm = FALSE, ...) {
    data = getTaskData(task)
    sapply(getTaskFeatureNames(task), function(feat.name) {
      var(data[[feat.name]], na.rm = na.rm)
    })
  }
)

makeFilter(
  name = "permutation.importance",
  desc = "Aggregated difference between feature permuted and unpermuted predictions",
  pkg = character(0L),
  supported.tasks = c("classif", "regr", "surv"),
  supported.features = c("numerics", "factors"),
  fun = function(task, learner, measure, contrast = function(x, y) x - y,
                 aggregation = mean, nperm = 1, nselect, replace = FALSE, ...) {
    learner = checkLearner(learner)
    measure = checkMeasures(measure, learner)
    if (getTaskType(task) != learner$type)
      stopf("Expected task of type '%s', not '%s'", getTaskType(task), learner$type)
    if (length(measure) != 1L)
      stop("Exactly one measure must be provided")
    assertCount(nperm)
    test.contrast = contrast(1, 1)
    assert(is.numeric(test.contrast) & length(test.contrast) == 1L)
    test.aggregation = aggregation(1:2)
    assert(is.numeric(test.aggregation) & length(test.aggregation) == 1L)

    doPermutationImportance = function(task, learner, measure, contrast, i) {
      fit = train(learner, task)
      pred = predict(fit, task = task)
      data = getTaskData(task)

      sapply(getTaskFeatureNames(task), function(x) {
        data[[x]] = sample(data[[x]], length(data[[x]]), replace)
        pred.permuted = predict(fit, newdata = data)
        perf = performance(pred, measure)
        perf.permuted = performance(pred.permuted, measure)
        contrast(perf.permuted, perf)
      }, USE.NAMES = FALSE)
    }

    args = list(task = task, learner = learner, measure = measure, contrast = contrast)
    out = parallelMap::parallelMap(doPermutationImportance, seq_len(nperm), more.args = args)
    out = do.call("rbind", out)
    out = apply(out, 2, aggregation)
    names(out) = getTaskFeatureNames(task)
    return(out)
  }
)

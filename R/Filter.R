.FilterRegister = new.env()

#' Create a feature filter
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
  assertString(pkg, na.ok = TRUE)
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

#' List filter methods
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
    package = extractSubList(filters, "pkg")
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
    catf("Package: '%s'", x$pkg)
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
      target.ind = match(getTargetNames(task), colnames(data))
    }

    # some required conversions
    ind = vlapply(data, is.factor)
    data[ind] = lapply(data[ind], as.ordered)
    ind = which(vlapply(data, is.integer))
    data[ind] = lapply(data[ind], as.double)
    data = mRMR.data(data = data)

    res = mRMRe::mRMR.classic(data = data, target_indices = target.ind, feature_count = nselect, ...)
    setNames(as.numeric(scores(res)[[1L]]), res@feature_names[as.integer(solutions(res)[[1L]])])
})

makeFilter(
  # FIXME: catscore is in package st
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
  desc = "Importance of random forests",
  pkg  = "randomForestSRC",
  supported.tasks = c("classif", "regr", "surv"),
  supported.features = c("numerics", "factors"),
  fun = function(task, nselect, ...) {
    im = randomForestSRC::rfsrc(getTaskFormula(task), data = getTaskData(task), proximity = FALSE, forest = FALSE, ...)$importance
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
  desc = "oneR assocation rule",
  pkg  = "FSelector",
  supported.tasks = c("classif", "regr"),
  supported.features = c("numerics", "factors"),
  fun = function(task, nselect, ...) {
    y = FSelector::oneR(getTaskFormula(task), data = getTaskData(task))
    setNames(y[["attr_importance"]], getTaskFeatureNames(task))
  }
)

makeFilter(
  name = "univariate",
  desc = "Construct a simple performance filter using a mlr learner",
  pkg  = NA_character_,
  supported.tasks = c("surv", "classif", "regr"),
  supported.features = c("numerics", "factors"),
  fun = function(task, nselect, learner, measure, resampling = NULL, ...) {
    learner = checkLearner(learner)
    measure = checkMeasures(measure, learner)
    if (length(measure) != 1L)
      stop("Exactly one measure must be provided")
    if (is.null(resampling))
      resampling = makeResampleDesc("Subsample", iters = 1L, split = 0.67)
    if (getTaskType(task) != learner$type)
      stopf("Expected task of type '%s', not '%s'", getTaskType(task), learner$type)

    fns = getTaskFeatureNames(task)
    res = double(length(fns))
    for (i in seq_along(fns)) {
      subtask = subsetTask(task, features = fns[-i])
      res[i] = resample(learner = learner, task = subtask, resampling = resampling, measures = measure)$aggr
    }
    if (measure[[1L]]$minimize)
      res = -1.0 * res
    setNames(res, fns)
  }
)


makeFilter(
  name = "anova.test",
  desc = "ANOVA Test for binary and multiclass classification tasks",
  pkg = NA_character_,
  supported.tasks = c("classif"),
  supported.features = c("numerics"),
  fun = function(task, nselect, ...) {
    data = getTaskData(task)
    sapply(getTaskFeatureNames(task), function(feat.name) {
      f = as.formula(paste0(feat.name,"~",getTargetNames(task)))
      aov.t = aov(f, data = data)
      summary(aov.t)[[1]][1,'F value']
    })
  }
)

makeFilter(
  name = "kruskal.test",
  desc = "Kurskal Test for binary and multiclass classification tasks",
  pkg = NA_character_,
  supported.tasks = c("classif"),
  supported.features = c("numerics", "factors"),
  fun = function(task, nselect, ...) {
    data = getTaskData(task)
    res = sapply(getTaskFeatureNames(task), function(feat.name) {
      f = as.formula(paste0(feat.name,"~",getTargetNames(task)))
      t = kruskal.test(f, data = data)
      t$statistic
    })
    setNames(res, getTaskFeatureNames(task))
  }
)

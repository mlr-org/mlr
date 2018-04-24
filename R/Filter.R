.FilterRegister = new.env()  # nolint

#' Create a feature filter.
#'
#' Creates and registers custom feature filters. Implemented filters
#' can be listed with [listFilterMethods]. Additional
#' documentation for the `fun` parameter specific to each filter can
#' be found in the description.
#'
#' @param name (`character(1)`)\cr
#'  Identifier for the filter.
#' @param desc (`character(1)`)\cr
#'  Short description of the filter.
#' @param pkg (`character(1)`)\cr
#'  Source package where the filter is implemented.
#' @param supported.tasks ([character])\cr
#'  Task types supported.
#' @param supported.features ([character])\cr
#'  Feature types supported.
#' @param fun (`function(task, nselect, ...`)\cr
#'  Function which takes a task and returns a named numeric vector of scores,
#'  one score for each feature of `task`.
#'  Higher scores mean higher importance of the feature.
#'  At least `nselect` features must be calculated, the remaining may be
#'  set to `NA` or omitted, and thus will not be selected.
#'  the original order will be restored if necessary.
#' @return Object of class \dQuote{Filter}.
#' @export
#' @family filter
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
#' @param desc (`logical(1)`)\cr
#'  Provide more detailed information about filters.
#'  Default is `TRUE`.
#' @param tasks (`logical(1)`)\cr
#'  Provide information on supported tasks.
#'  Default is `FALSE`.
#' @param features (`logical(1)`)\cr
#'  Provide information on supported features.
#'  Default is `FALSE`.
#' @param include.deprecated (`logical(1)`)\cr
#'  Should deprecated filter methods be included in the list.
#'  Default is `FALSE`.
#' @return ([data.frame]).
#' @export
#' @family filter
listFilterMethods = function(desc = TRUE, tasks = FALSE, features = FALSE, include.deprecated = FALSE) {
  tag2df = function(tags, prefix = "") {
    unique.tags = sort(unique(unlist(tags)))
    res = asMatrixRows(lapply(tags, "%in%", x = unique.tags))
    colnames(res) = stri_paste(prefix, unique.tags)
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

  description = extractSubList(filters, "desc")

  if (desc)
    df$desc = description
  if (tasks)
    df = cbind(df, tag2df(extractSubList(filters, "supported.tasks"), prefix = "task."))
  if (features)
    df = cbind(df, tag2df(extractSubList(filters, "supported.features"), prefix = "feature."))
  deprecated = stri_endswith(description, fixed = "(DEPRECATED)")
  if (include.deprecated)
    df$deprecated = deprecated
  else
    df = df[!deprecated, ]
  res = setRowNames(sortByCol(df, "id"), NULL)
  addClasses(res, "FilterMethodsList")
}

#' @export
print.FilterMethodsList = function(x, len = 40, ...) {
  if (!is.null(x$desc))
    x$desc = clipString(x$desc, len = len)
  NextMethod()
}

#' @export
print.Filter = function(x, ...) {
  catf("Filter: '%s'", x$name)
  if (!isScalarNA(x$pkg))
    catf("Packages: '%s'", collapse(cleanupPackageNames(x$pkg)))
  catf("Supported tasks: %s", collapse(x$supported.tasks))
  catf("Supported features: %s", collapse(x$supported.features))
}

#' Minimum redundancy, maximum relevance filter \dQuote{mrmr} computes the
#' mutual information between the target and each individual feature minus the
#' average mutual information of previously selected features and this feature
#' using the \pkg{mRMRe} package.
#'
#' @rdname makeFilter
#' @name makeFilter
makeFilter(
  name = "mrmr",
  desc = "Minimum redundancy, maximum relevance filter",
  pkg  = "mRMRe",
  supported.tasks = c("regr", "surv"),
  supported.features = c("numerics", "ordered"),
  fun = function(task, nselect, ...) {
    if (inherits(task, "SurvTask")) {
      data = getTaskData(task, target.extra = TRUE, recode.target = "surv")
      data = cbind(..surv = data$target, data$data)
      target.ind = 1L
    } else {
      data = getTaskData(task)
      target.ind = match(getTaskTargetNames(task), colnames(data))
    }

    # some required conversions
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

#' Filter \dQuote{carscore} determines the \dQuote{Correlation-Adjusted (marginal) coRelation
#' scores} (short CAR scores). The CAR scores for a set of features are defined as the
#' correlations between the target and the decorrelated features.
#'
#' @rdname makeFilter
#' @name makeFilter
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

#' Filter \dQuote{randomForestSRC.rfsrc} computes the importance of random forests
#' fitted in package \pkg{randomForestSRC}. The concrete method is selected via
#' the `method` parameter. Possible values are `permute` (default), `random`,
#' `anti`, `permute.ensemble`, `random.ensemble`, `anti.ensemble`.
#' See the VIMP section in the docs for [randomForestSRC::rfsrc] for
#' details.
#'
#' @rdname makeFilter
#' @name makeFilter
rf.importance = makeFilter(
  name = "randomForestSRC.rfsrc",
  desc = "Importance of random forests fitted in package 'randomForestSRC'. Importance is calculated using argument 'permute'.",
  pkg  = "randomForestSRC",
  supported.tasks = c("classif", "regr", "surv"),
  supported.features = c("numerics", "factors", "ordered"),
  fun = function(task, nselect, method = "permute", ...) {
    assertChoice(method, choices = c("permute", "random", "anti", "permute.ensemble", "random.ensemble",  "anti.ensemble"))
    im = randomForestSRC::rfsrc(getTaskFormula(task), data = getTaskData(task), proximity = FALSE,
      forest = FALSE, importance = method, ...)$importance
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
.FilterRegister[["rf.importance"]] = rf.importance
.FilterRegister[["rf.importance"]]$desc = "Importance of random forests fitted in package 'randomForestSRC'. Importance is calculated using argument 'permute'. (DEPRECATED)"
.FilterRegister[["rf.importance"]]$fun = function(...) {
  .Deprecated(old = "Filter 'rf.importance'", new = "Filter 'randomForest.importance' (package randomForest) or Filter 'randomForestSRC.rfsrc' (package randomForestSRC)")
  .FilterRegister[["randomForestSRC.rfsrc"]]$fun(...)
}

#' Filter \dQuote{randomForestSRC.var.select} uses the minimal depth variable
#' selection proposed by Ishwaran et al. (2010) (`method = "md"`) or a
#' variable hunting approach (`method = "vh"` or `method = "vh.vimp"`).
#' The minimal depth measure is the default.
#'
#' @rdname makeFilter
#' @name makeFilter
rf.min.depth = makeFilter(
  name = "randomForestSRC.var.select",
  desc = "Minimal depth of / variable hunting via method var.select on random forests fitted in package 'randomForestSRC'.",
  pkg  = "randomForestSRC",
  supported.tasks = c("classif", "regr", "surv"),
  supported.features = c("numerics", "factors", "ordered"),
  fun = function(task, nselect, method = "md", ...) {
    im = randomForestSRC::var.select(getTaskFormula(task), getTaskData(task),
      method = method, verbose = FALSE, ...)$md.obj$order
    setNames(-im[, 1L], rownames(im))
  }
)
.FilterRegister[["rf.min.depth"]] = rf.min.depth
.FilterRegister[["rf.min.depth"]]$desc = "Minimal depth of random forest fitted in package 'randomForestSRC. (DEPRECATED)"
.FilterRegister[["rf.min.depth"]]$fun = function(...) {
  .Deprecated(old = "Filter 'rf.min.depth'", new = "Filter 'randomForestSRC.var.select'")
  .FilterRegister[["randomForestSRC.var.select"]]$fun(...)
}

#' Permutation importance of random forests fitted in package \pkg{party}.
#' The implementation follows the principle of mean decrese in accuracy used
#' by the \pkg{randomForest} package (see description of \dQuote{randomForest.importance})
#' filter.
#'
#' @rdname makeFilter
#' @name makeFilter
makeFilter(
  name = "cforest.importance",
  desc = "Permutation importance of random forest fitted in package 'party'",
  pkg = "party",
  supported.tasks = c("classif", "regr", "surv"),
  supported.features = c("numerics", "factors", "ordered"),
  fun = function(task, nselect, mtry = 5L, ...) {
    args = list(...)
    # we need to set mtry, which is 5 by default in cforest, to p if p < mtry
    # otherwise we get a warning
    p = getTaskNFeats(task)
    if (p < mtry)
      args$mtry = p
    cforest.args = as.list(base::args(party::cforest))
    cforest.args = args[names(args) %in% names(cforest.args)]
    control.args = as.list(base::args(party::cforest_control))
    control.args = args[names(args) %in% names(control.args)]
    varimp.args = as.list(base::args(party::varimp))
    varimp.args = args[names(args) %in% names(varimp.args)]
    ctrl = do.call(party::cforest_unbiased, control.args)
    fit = do.call(party::cforest, c(list(formula = getTaskFormula(task), data = getTaskData(task), controls = ctrl),
                                    cforest.args))
    im = do.call(party::varimp, c(list(obj = fit), varimp.args))
    im
  }
)

#' Filter \dQuote{randomForest.importance} makes use of the [randomForest::importance]
#' from package \pkg{randomForest}. The importance measure to use is selected via
#' the `method` parameter:
#' \describe{
#'   \item{oob.accuracy}{Permutation of Out of Bag (OOB) data.}
#'   \item{node.impurity}{Total decrease in node impurity.}
#' }
#'
#' @rdname makeFilter
#' @name makeFilter
makeFilter(
  name = "randomForest.importance",
  desc = "Importance based on OOB-accuracy or node inpurity of random forest fitted in package 'randomForest'.",
  pkg = "randomForest",
  supported.tasks = c("classif", "regr"),
  supported.features = c("numerics", "factors"),
  fun = function(task, nselect, method = "oob.accuracy", ...) {
    assertChoice(method, choices = c("oob.accuracy", "node.impurity"))
    type = if (method == "oob.accuracy") 1L else 2L
    # no need to set importance = TRUE for node impurity (type = 2)
    rf = randomForest::randomForest(getTaskFormula(task), data = getTaskData(task),
      keep.forest = FALSE, importance = (type != 2L))
    im = randomForest::importance(rf, type = type, ...)
    setNames(im, rownames(im))
  }
)

#' The absolute Pearson correlation between each feature and the target is used as an indicator of feature importance.
#' Missing values are not taken into consideration in a pairwise fashion (see \dQuote{pairwise.complete.obs} in [cor]).
#'
#' @rdname makeFilter
#' @name makeFilter
makeFilter(
  name = "linear.correlation",
  desc = "Pearson correlation between feature and target",
  pkg  = character(0L),
  supported.tasks = "regr",
  supported.features = "numerics",
  fun = function(task, nselect, ...) {
    data = getTaskData(task, target.extra = TRUE)
    abs(cor(as.matrix(data$data), data$target, use = "pairwise.complete.obs", method = "pearson")[, 1L])
  }
)

#' The absolute Pearson correlation between each feature and the target is used as an indicator of feature importance.
#' Missing values are not taken into consideration in a pairwise fashion (see \dQuote{pairwise.complete.obs} in [cor]).
#'
#' @rdname makeFilter
#' @name makeFilter
makeFilter(
  name = "rank.correlation",
  desc = "Spearman's correlation between feature and target",
  pkg  = character(0L),
  supported.tasks = "regr",
  supported.features = "numerics",
  fun = function(task, nselect, ...) {
    data = getTaskData(task, target.extra = TRUE)
    abs(cor(as.matrix(data$data), data$target, use = "pairwise.complete.obs", method = "spearman")[, 1L])
  }
)

#' Filter \dQuote{information.gain} uses the entropy-based information gain
#' between each feature and target individually as an importance measure.
#'
#' @rdname makeFilter
#' @name makeFilter
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

#' Filter \dQuote{gain.ratio} uses the entropy-based information gain ratio
#' between each feature and target individually as an importance measure.
#'
#' @rdname makeFilter
#' @name makeFilter
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

#' Filter \dQuote{symmetrical.uncertainty} uses the entropy-based symmetrical uncertainty
#' between each feature and target individually as an importance measure.
#'
#' @rdname makeFilter
#' @name makeFilter
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

#' The chi-square test is a statistical test of independence to determine whether
#' two variables are independent. Filter \dQuote{chi.squared} applies this
#' test in the following way. For each feature the chi-square test statistic is
#' computed checking if there is a dependency between the feature and the target
#' variable. Low values of the test statistic indicate a poor relationship. High
#' values, i.e., high dependency identifies a feature as more important.
#'
#' @rdname makeFilter
#' @name makeFilter
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

#' Filter \dQuote{relief} is based on the feature selection algorithm \dQuote{ReliefF}
#' by Kononenko et al., which is a generalization of the orignal \dQuote{Relief}
#' algorithm originally proposed by Kira and Rendell. Feature weights are initialized
#' with zeros. Then for each instance `sample.size` instances are sampled,
#' `neighbours.count` nearest-hit and nearest-miss neighbours are computed
#' and the weight vector for each feature is updated based on these values.
#'
#' @references
#' Kira, Kenji and Rendell, Larry (1992). The Feature Selection Problem: Traditional
#' Methods and a New Algorithm. AAAI-92 Proceedings.
#'
#' Kononenko, Igor et al. Overcoming the myopia of inductive learning algorithms
#' with RELIEFF (1997), Applied Intelligence, 7(1), p39-55.
#'
#' @rdname makeFilter
#' @name makeFilter
makeFilter(
  name = "relief",
  desc = "RELIEF algorithm",
  pkg  = "FSelector",
  supported.tasks = c("classif", "regr"),
  supported.features = c("numerics", "factors"),
  fun = function(task, nselect, ...) {
    y = FSelector::relief(getTaskFormula(task), data = getTaskData(task), ...)
    setNames(y[["attr_importance"]], getTaskFeatureNames(task))
  }
)

#' Filter \dQuote{oneR} makes use of a simple \dQuote{One-Rule} (OneR) learner to
#' determine feature importance. For this purpose the OneR learner generates one
#' simple association rule for each feature in the data individually and computes
#' the total error. The lower the error value the more important the correspoding
#' feature.
#'
#' @rdname makeFilter
#' @name makeFilter
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

#' The \dQuote{univariate.model.score} feature filter resamples an \pkg{mlr}
#' learner specified via `perf.learner` for each feature individually
#' with randomForest from package \pkg{rpart} being the default learner.
#' Further parameter are the resamling strategey `perf.resampling` and
#' the performance measure `perf.measure`.
#'
#' @rdname makeFilter
#' @name makeFilter
univariate = makeFilter(
  name = "univariate.model.score",
  desc = "Resamples an mlr learner for each input feature individually. The resampling performance is used as filter score, with rpart as default learner.",
  pkg  = character(0L),
  supported.tasks = c("classif", "regr", "surv"),
  supported.features = c("numerics", "factors", "ordered"),
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
.FilterRegister[["univariate"]]$desc = "Resamples an mlr learner for each input feature individually. The resampling performance is used as filter score, with rpart as default learner. (DEPRECATED)"
.FilterRegister[["univariate"]]$fun = function(...) {
  .Deprecated(old = "Filter 'univariate'", new = "Filter 'univariate.model.score'")
  .FilterRegister[["univariate.model.score"]]$fun(...)
}

#' Filter \dQuote{anova.test} is based on the Analysis of Variance (ANOVA) between
#' feature and class. The value of the F-statistic is used as a measure of feature
#' importance.
#'
#' @rdname makeFilter
#' @name makeFilter
makeFilter(
  name = "anova.test",
  desc = "ANOVA Test for binary and multiclass classification tasks",
  pkg = character(0L),
  supported.tasks = "classif",
  supported.features = "numerics",
  fun = function(task, nselect, ...) {
    data = getTaskData(task)
    vnapply(getTaskFeatureNames(task), function(feat.name) {
      f = as.formula(stri_paste(feat.name, "~", getTaskTargetNames(task)))
      aov.t = aov(f, data = data)
      summary(aov.t)[[1L]][1L, "F value"]
    })
  }
)

#' Filter \dQuote{kruskal.test} applies a Kruskal-Wallis rank sum test of the
#' null hypothesis that the location parameters of the distribution of a feature
#' are the same in each class and considers the test statistic as an variable
#' importance measure: if the location parameters do not differ in at least one
#' case, i.e., the null hypothesis cannot be rejected, there is little evidence
#' that the corresponding feature is suitable for classification.
#'
#' @rdname makeFilter
#' @name makeFilter
makeFilter(
  name = "kruskal.test",
  desc = "Kruskal Test for binary and multiclass classification tasks",
  pkg = character(0L),
  supported.tasks = "classif",
  supported.features = c("numerics", "factors"),
  fun = function(task, nselect, ...) {
    data = getTaskData(task)
    sapply(getTaskFeatureNames(task), function(feat.name) {
      f = as.formula(stri_paste(feat.name, "~", getTaskTargetNames(task)))
      t = kruskal.test(f, data = data)
      unname(t$statistic)
    })
  }
)

#' Simple filter based on the variance of the features indepentent of each other.
#' Features with higher variance are considered more important than features with
#' low importance.
#'
#' @rdname makeFilter
#' @name makeFilter
makeFilter(
  name = "variance",
  desc = "A simple variance filter",
  pkg = character(0L),
  supported.tasks = c("classif", "regr", "surv"),
  supported.features = "numerics",
  fun = function(task, nselect, na.rm = TRUE, ...) {
    data = getTaskData(task)
    sapply(getTaskFeatureNames(task), function(feat.name) {
      var(data[[feat.name]], na.rm = na.rm)
    })
  }
)

#' Filter \dQuote{permutation.importance} computes a loss function between predictions made by a
#' learner before and after a feature is permuted. Special arguments to the filter function are
#' `imp.learner`, a ([Learner] or `character(1)]) which specifies the learner
#' to use when computing the permutation importance, `contrast`, a `function` which takes two
#' numeric vectors and returns one (default is the difference), `aggregation`, a `function` which
#' takes a `numeric` and returns a `numeric(1)` (default is the mean), `nmc`,
#' an `integer(1)`, and `replace`, a `logical(1)` which determines whether the feature being
#' permuted is sampled with or without replacement.
#'
#' @rdname makeFilter
#' @name makeFilter
makeFilter(
  name = "permutation.importance",
  desc = "Aggregated difference between feature permuted and unpermuted predictions",
  pkg = character(0L),
  supported.tasks = c("classif", "regr", "surv"),
  supported.features = c("numerics", "factors", "ordered"),
  fun = function(task, imp.learner, measure, contrast = function(x, y) x - y,
                 aggregation = mean, nmc = 50L, replace = FALSE, nselect) {
    imp = generateFeatureImportanceData(task, "permutation.importance",
      imp.learner, interaction = FALSE, measure = measure,
      contrast = contrast, aggregation = aggregation,
      nmc = nmc, replace = replace, local = FALSE)
    imp = as.numeric(imp$res)
    names(imp) = getTaskFeatureNames(task)
    return(imp)
  }
)

#' Filter \dQuote{auc} determines for each feature, how well the target
#' variable can be predicted only based on this feature. More precisely, the
#' prediction rule is: class 1 if the feature exceeds a threshold and class 0
#' otherwise. The performance of this classification rule is measured by the
#' AUC and the resulting filter score is |0.5 - AUC|.
#'
#' @rdname makeFilter
#' @name makeFilter
makeFilter(
  name = "auc",
  desc = "AUC filter for binary classification tasks",
  pkg  = character(0L),
  supported.tasks = "classif",
  supported.features = "numerics",
  fun = function(task, nselect, ...) {
    data = getTaskData(task, target.extra = TRUE)
    score = vnapply(data$data, function(x, y) {
      measureAUC(x, y, task$task.desc$negative, task$task.desc$positive)
    }, y = data$target)
    abs(0.5 - score)
  }
)


#' Filter \dQuote{ranger.permutation} trains a ranger learner with
#' \dQuote{importance = "permutation"} and assesses the variable
#' importance for each feature.
#'
#' @rdname makeFilter
#' @name makeFilter
makeFilter(
  name = "ranger.permutation",
  desc = "Variable importance based on ranger permutation importance",
  pkg  = "ranger",
  supported.tasks = c("classif", "regr", "surv"),
  supported.features = c("numerics", "factors", "ordered"),
  fun = function(task, nselect, ...) {
    lrn.type = paste0(getTaskType(task), ".ranger")
    lrn = makeLearner(lrn.type, importance = "permutation", ...)
    mod = train(lrn, task)
    ranger::importance(mod$learner.model)
  }
)


#' Filter \dQuote{ranger.impurity} trains a ranger learner with
#' \dQuote{importance = "impurity"} and assesses the variable
#' importance for each feature.
#'
#' @rdname makeFilter
#' @name makeFilter
makeFilter(
  name = "ranger.impurity",
  desc = "Variable importance based on ranger impurity importance",
  pkg  = "ranger",
  supported.tasks = c("classif", "regr"),
  supported.features = c("numerics", "factors", "ordered"),
  fun = function(task, nselect, ...) {
    lrn.type = paste0(getTaskType(task), ".ranger")
    lrn = makeLearner(lrn.type, importance = "impurity", ...)
    mod = train(lrn, task)
    ranger::importance(mod$learner.model)
  }
)


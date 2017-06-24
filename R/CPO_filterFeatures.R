#' @include Filter.R

#' @title Filter features by thresholding filter values.
#'
#' @description
#' First, calls \code{\link{generateFilterValuesData}}.
#' Features are then selected via \code{select} and \code{val}.
#'
#' @template arg_task
#' @param method [\code{character(1)}]\cr
#'   See \code{\link{listFilterMethods}}.
#'   Default is \dQuote{randomForestSRC.rfsrc}.
#' @param fval [\code{\link{FilterValues}}]\cr
#'   Result of \code{\link{generateFilterValuesData}}.
#'   If you pass this, the filter values in the object are used for feature filtering.
#'   \code{method} and \code{...} are ignored then.
#'   Default is \code{NULL} and not used.
#' @param perc [\code{numeric(1)}]\cr
#'   If set, select \code{perc}*100 top scoring features.
#'   Mutually exclusive with arguments \code{abs} and \code{threshold}.
#' @param abs [\code{numeric(1)}]\cr
#'   If set, select \code{abs} top scoring features.
#'   Mutually exclusive with arguments \code{perc} and \code{threshold}.
#' @param threshold [\code{numeric(1)}]\cr
#'   If set, select features whose score exceeds \code{threshold}.
#'   Mutually exclusive with arguments \code{perc} and \code{abs}.
#' @param filter.args [\code{list}]\cr
#'   Passed down to selected filter method. Default is \code{list()}.
#' @template ret_task
#' @export
#' @family filter
#' @family CPO
cpoFilterFeatures = makeCPO("filterFeatures", #nolint
  .par.set = c(
      makeParamSet(makeDiscreteLearnerParam("method", values = ls(.FilterRegister), default = "randomForestSRC.rfsrc"),
        makeUntypedLearnerParam("fval", default = NULL)),
      paramSetSugar(
          perc = NULL: numeric[0, 1] [[special.vals = list(NULL)]],
          abs = NULL: integer[0, ] [[special.vals = list(NULL)]],
          threshold = NULL: numeric[, ] [[special.vals = list(NULL)]]),
      makeParamSet(makeUntypedLearnerParam("filter.args", default = list()))),
  .datasplit = "task",
  cpo.trafo = function(data, target, method, fval, perc, abs, threshold, filter.args) {
    assertList(filter.args)
    fullargs = c(list(task = data, method = method, fval = fval, perc = perc, abs = abs, threshold = threshold), filter.args)
    assertSubset(unique(names(fullargs)[duplicated(names(fullargs))]), "")
    data = do.call(filterFeatures, fullargs)
    control = getTaskFeatureNames(data)
    data
  }, cpo.retrafo = function(data, control, ...) {
    data[control]
  })
registerCPO(cpoFilterFeatures, "featurefilter", "general", "Filter features using a provided method.")


declareFilterCPO = function(method, ..., .par.set = NULL) {
  if (is.null(.par.set)) {
    .par.set = paramSetSugar(..., .pss.env = parent.frame())
  }
  .par.set = c(paramSetSugar(
      perc = NULL: numeric[0, 1] [[special.vals = list(NULL)]],
      abs = NULL: integer[0, ] [[special.vals = list(NULL)]],
      threshold = NULL: numeric[, ] [[special.vals = list(NULL)]]),
    .par.set)

  methodobj = get(method, envir = .FilterRegister)

  makeCPO(method, .par.set = .par.set, .datasplit = "task", .properties.target = c(methodobj$supported.tasks, cpo.targetproperties),
    cpo.trafo = function(data, target, perc, abs, threshold, ...) {
      filter.args = list(...)
      td = getTaskData(data, target.extra = TRUE)$data
      tt = vcapply(td, function(x) class(x)[1])
      tt = c(numeric = "numerics", factor = "factors", ordered = "ordered")[tt]
      stask = subsetTask(data, features = tt %in% methodobj$supported.features)
      ftask = do.call(filterFeatures, c(list(task = stask, method = method, fval = NULL, perc = perc, abs = abs, threshold = threshold), filter.args))
      control = setdiff(getTaskFeatureNames(data), setdiff(getTaskFeatureNames(stask), getTaskFeatureNames(ftask)))
      subsetTask(data, features = control)
    }, cpo.retrafo = function(data, control, ...) {
      data[control]
    })
}

#' Minimum redundancy, maximum relevance filter \dQuote{mrmr} computes the
#' mutual information between the target and each individual feature minus the
#' average mutual information of previously selected features and this feature
#' using the \pkg{mRMRe} package.
#' @export
cpoFilterMrmr = declareFilterCPO("mrmr") # nolint  #  missing parameters
registerCPO(cpoFilterMrmr, "featurefilter", "specialised", "Filter features using 'minimum redundancy, maximum relevance'.")

#' Filter \dQuote{carscore} determines the \dQuote{Correlation-Adjusted (marginal) coRelation
#' scores} (short CAR scores). The CAR scores for a set of features are defined as the
#' correlations between the target and the decorrelated features.
#' @export
cpoFilterCarscore = declareFilterCPO("carscore", diagonal = FALSE: logical)  # nolint # missing parameter 'lambda'
registerCPO(cpoFilterCarscore, "featurefilter", "specialised", "Filter features using correlation-adjusted marginal correlation.")

#' Filter \dQuote{randomForestSRC.rfsrc} computes the importance of random forests
#' fitted in package \pkg{randomForestSRC}. The concrete method is selected via
#' the \code{method} parameter. Possible values are \code{permute} (default), \code{random},
#' \code{anti}, \code{permute.ensemble}, \code{random.ensemble}, \code{anti.ensemble}.
#' See the VIMP section in the docs for \code{\link[randomForestSRC]{rfsrc}} for
#' details.
#' @export
cpoFilterRfSRCImportance = declareFilterCPO("randomForestSRC.rfsrc") #,  # nolint
#  method = "permute": discrete[permute, random, anti, permute.ensemble, random.ensemble, anti.ensemble])  # missing parameters
registerCPO(cpoFilterRfSRCImportance, "featurefilter", "specialised", "Filter features using randomForestSRC.rfsrc.")

#' Filter \dQuote{randomForestSRC.var.select} uses the minimal depth variable
#' selection proposed by Ishwaran et al. (2010) (\code{method = "md"}) or a
#' variable hunting approach (\code{method = "vh"} or \code{method = "vh.vimp"}).
#' The minimal depth measure is the default.
#' @export
cpoFilterRfSRCMinDepth = declareFilterCPO("randomForestSRC.var.select")  # missing parameter: , method = "md": discrete[md, vh, vh.vimp])  # nolint  # missing parameters
registerCPO(cpoFilterRfSRCMinDepth, "featurefilter", "specialised", "Filter features using randomForestSRC minimal depth.")

#' Permutation importance of random forests fitted in package \pkg{party}.
#' The implementation follows the principle of mean decrese in accuracy used
#' by the \pkg{randomForest} package (see description of \dQuote{randomForest.importance})
#' filter.
#' @export
cpoFilterRfCImportance = declareFilterCPO("cforest.importance", mtry = 5: integer[1, ])  # nolint  # missing parameters
registerCPO(cpoFilterRfCImportance, "featurefilter", "specialised", "Filter features using party::cforest variable importance.")

#' Filter \dQuote{randomForest.importance} makes use of the \code{\link[randomForest]{importance}}
#' from package \pkg{randomForest}. The importance measure to use is selected via
#' the \code{method} parameter:
#' \describe{
#'   \item{oob.accuracy}{Permutation of Out of Bag (OOB) data.}
#'   \item{node.impurity}{Total decrease in node impurity.}
#' }
#' @export
cpoFilterRfImportance = declareFilterCPO("randomForest.importance")  #, method = "oob.accuracy": discrete[oob.accuracy, node.impurity])  # nolint  # missing parameters
registerCPO(cpoFilterRfImportance, "featurefilter", "specialised", "Filter features using randomForest variable importance.")

#' The Pearson correlation between each feature and the target is used as an indicator
#' of feature importance. Rows with NA values are not taken into consideration.
#' @export
cpoFilterLinearCorrelation = declareFilterCPO("linear.correlation")  # nolint
registerCPO(cpoFilterLinearCorrelation, "featurefilter", "specialised", "Filter features using Pearson correlation.")

#' The Spearman correlation between each feature and the target is used as an indicator
#' of feature importance. Rows with NA values are not taken into consideration.
#' @export
cpoFilterRankCorrelation = declareFilterCPO("rank.correlation")  # nolint
registerCPO(cpoFilterRankCorrelation, "featurefilter", "specialised", "Filter features using Spearman correlation.")

#' Filter \dQuote{information.gain} uses the entropy-based information gain
#' between each feature and target individually as an importance measure.
#' @export
cpoFilterInformationGain = declareFilterCPO("information.gain")  # nolint
registerCPO(cpoFilterInformationGain, "featurefilter", "specialised", "Filter features using entropy-based information gain.")

#' Filter \dQuote{gain.ratio} uses the entropy-based information gain ratio
#' between each feature and target individually as an importance measure.
#' @export
cpoFilterGainRatio = declareFilterCPO("gain.ratio")  # nolint
registerCPO(cpoFilterGainRatio, "featurefilter", "specialised", "Filter features using entropy-based information gain ratio")

#' Filter \dQuote{symmetrical.uncertainty} uses the entropy-based symmetrical uncertainty
#' between each feature and target individually as an importance measure.
#' @export
cpoFilterSymmetricalUncertainty = declareFilterCPO("symmetrical.uncertainty")  # nolint
registerCPO(cpoFilterSymmetricalUncertainty, "featurefilter", "specialised", "Filter features using entropy-based symmetrical uncertainty")

#' The chi-square test is a statistical test of independence to determine whether
#' two variables are independent. Filter \dQuote{chi.squared} applies this
#' test in the following way. For each feature the chi-square test statistic is
#' computed checking if there is a dependency between the feature and the target
#' variable. Low values of the test statistic indicate a poor relationship. High
#' values, i.e., high dependency identifies a feature as more important.
#' @export
cpoFilterChiSquared = declareFilterCPO("chi.squared")  # nolint
registerCPO(cpoFilterChiSquared, "featurefilter", "specialised", "Filter features using chi-squared test.")

#' Filter \dQuote{relief} is based on the feature selection algorithm \dQuote{ReliefF}
#' by Kononenko et al., which is a generalization of the orignal \dQuote{Relief}
#' algorithm originally proposed by Kira and Rendell. Feature weights are initialized
#' with zeros. Then for each instance \code{sample.size} instances are sampled,
#' \code{neighbours.count} nearest-hit and nearest-miss neighbours are computed
#' and the weight vector for each feature is updated based on these values.
#'
#' @references
#' Kira, Kenji and Rendell, Larry (1992). The Feature Selection Problem: Traditional
#' Methods and a New Algorithm. AAAI-92 Proceedings.
#'
#' Kononenko, Igor et al. Overcoming the myopia of inductive learning algorithms
#' with RELIEFF (1997), Applied Intelligence, 7(1), p39-55.
#' @export
cpoFilterRelief = declareFilterCPO("relief")  # nolint # missing parameters
registerCPO(cpoFilterRelief, "featurefilter", "specialised", "Filter features using the ReliefF algorithm.")

#' Filter \dQuote{oneR} makes use of a simple \dQuote{One-Rule} (OneR) learner to
#' determine feature importance. For this purpose the OneR learner generates one
#' simple association rule for each feature in the data individually and computes
#' the total error. The lower the error value the more important the correspoding
#' feature.
#' @export
cpoFilterOneR = declareFilterCPO("oneR")  # nolint
registerCPO(cpoFilterOneR, "featurefilter", "specialised", "Filter features using the OneR learner.")

#' The \dQuote{univariate.model.score} feature filter resamples an \pkg{mlr}
#' learner specified via \code{perf.learner} for each feature individually
#' with randomForest from package \pkg{rpart} being the default learner.
#' Further parameter are the resamling strategey \code{perf.resampling} and
#' the performance measure \code{perf.measure}.
#' @export
cpoFilterUnivariate = declareFilterCPO("univariate.model.score",  # nolint
  .par.set = makeParamSet(
      makeUntypedLearnerParam("perf.learner", NULL),
      makeUntypedLearnerParam("perf.measure", NULL),
      makeUntypedLearnerParam("perf.resampling", NULL)))
registerCPO(cpoFilterUnivariate, "featurefilter", "specialised", "Filter features using the predictiveness using a given learner.")

#' Filter \dQuote{anova.test} is based on the Analysis of Variance (ANOVA) between
#' feature and class. The value of the F-statistic is used as a measure of feature
#' importance.
#' @export
cpoFilterAnova = declareFilterCPO("anova.test")  # nolint
registerCPO(cpoFilterAnova, "featurefilter", "specialised", "Filter features using analysis of variance.")

#' Filter \dQuote{kruskal.test} applies a Kruskal-Wallis rank sum test of the
#' null hypothesis that the location parameters of the distribution of a feature
#' are the same in each class and considers the test statistic as an variable
#' importance measure: if the location parameters do not differ in at least one
#' case, i.e., the null hypothesis cannot be rejected, there is little evidence
#' that the corresponding feature is suitable for classification.
#' @export
cpoFilterKruskal = declareFilterCPO("kruskal.test")  # nolint
registerCPO(cpoFilterKruskal, "featurefilter", "specialised", "Filter features using the Kruskal-Wallis rank sum test.")

#' Simple filter based on the variance of the features indepentent of each other.
#' Features with higher variance are considered more important than features with
#' low importance.
#' @export
cpoFilterVariance = declareFilterCPO("variance")  # nolint
registerCPO(cpoFilterVariance, "featurefilter", "specialised", "Filter features using feature variance.")

#' Filter \dQuote{permutation.importance} computes a loss function between predictions made by a
#' learner before and after a feature is permuted. Special arguments to the filter function are
#' \code{imp.learner}, a [\code{\link{Learner}} or \code{character(1)}] which specifies the learner
#' to use when computing the permutation importance, \code{contrast}, a \code{function} which takes two
#' numeric vectors and returns one (default is the difference), \code{aggregation}, a \code{function} which
#' takes a \code{numeric} and returns a \code{numeric(1)} (default is the mean), \code{nmc},
#' an \code{integer(1)}, and \code{replace}, a \code{logical(1)} which determines whether the feature being
#' permuted is sampled with or without replacement.
#' @export
cpoFilterPermutationImportance = declareFilterCPO("permutation.importance",  # nolint
  .par.set = makeParamSet(
      makeUntypedLearnerParam("imp.learner"),
      makeUntypedLearnerParam("measure", default = NULL),
      makeUntypedLearnerParam("contrast", default = function(x, y) x - y),
      makeFunctionLearnerParam("aggregation", default = mean),
      makeIntegerLearnerParam("nmc", lower = 0, default = 50),
      makeLogicalLearnerParam("replace", default = FALSE)))
registerCPO(cpoFilterPermutationImportance, "featurefilter", "specialised", "Filter features using predictiveness loss upon permutation of a variable.")










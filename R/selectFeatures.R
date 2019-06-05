#' @title Feature selection by wrapper approach.
#'
#' @description
#' Optimizes the features for a classification or regression problem by choosing a variable selection wrapper approach.
#' Allows for different optimization methods, such as forward search or a genetic algorithm.
#' You can select such an algorithm (and its settings)
#' by passing a corresponding control object. For a complete list of implemented algorithms look at the
#' subclasses of ([FeatSelControl]).
#'
#' All algorithms operate on a 0-1-bit encoding of candidate solutions. Per default a single bit corresponds
#' to a single feature, but you are able to change this by using the arguments `bit.names`
#' and `bits.to.features`. Thus allowing you to switch on whole groups of features with a single bit.
#'
#' @template arg_learner
#' @template arg_task
#' @param resampling ([ResampleInstance] | [ResampleDesc])\cr
#'   Resampling strategy for feature selection. If you pass a description,
#'   it is instantiated once at the beginning by default, so all points are evaluated on the same training/test sets.
#'   If you want to change that behaviour, look at [FeatSelControl].
#' @template arg_measures_opt
#' @param bit.names [character]\cr
#'   Names of bits encoding the solutions. Also defines the total number of bits in the encoding.
#'   Per default these are the feature names of the task.
#'   Has to be used together with `bits.to.features`.
#' @param bits.to.features [function(x, task)]\cr
#'   Function which transforms an integer-0-1 vector into a character vector of selected features.
#'   Per default a value of 1 in the ith bit selects the ith feature to be in the candidate solution.
#'   The vector `x` will correspond to the `bit.names` and has to be of the same length.
#' @param control [see [FeatSelControl])
#'   Control object for search method.
#'   Also selects the optimization algorithm for feature selection.
#' @template arg_showinfo
#' @return ([FeatSelResult]).
#' @family featsel
#' @noMd
#' @export
#' @examples
#' \donttest{
#' rdesc = makeResampleDesc("Holdout")
#' ctrl = makeFeatSelControlSequential(method = "sfs", maxit = NA)
#' res = selectFeatures("classif.rpart", iris.task, rdesc, control = ctrl)
#' analyzeFeatSelResult(res)
#' }
selectFeatures = function(learner, task, resampling, measures,
  bit.names, bits.to.features, control, show.info = getMlrOption("show.info")) {

  learner = checkLearner(learner)
  assertClass(task, classes = "SupervisedTask")
  if (!inherits(resampling, "ResampleDesc") && !inherits(resampling, "ResampleInstance")) {
    stop("Argument resampling must be of class ResampleDesc or ResampleInstance!")
  }
  if (inherits(resampling, "ResampleDesc") && control$same.resampling.instance) {
    resampling = makeResampleInstance(resampling, task = task)
  }
  measures = checkMeasures(measures, learner)
  if (missing(bit.names)) {
    bit.names = getTaskFeatureNames(task)
  } else {
    assertCharacter(bit.names, any.missing = FALSE)
    if (missing(bits.to.features)) {
      stop("If you set bit.names you also have to set bits.to.features.")
    }
  }
  if (missing(bits.to.features)) {
    bits.to.features2 = function(x, task) binaryToFeatures(x, getTaskFeatureNames(task))
  } else {
    assertFunction(bits.to.features, args = c("x", "task"))
    # wrap the function to prevent wrong user input and give meaningful errors
    bits.to.features2 = function(x, task) {
      force(bits.to.features)
      res = bits.to.features(x, task)
      if (!testCharacter(res)) {
        stopf("bits.to.features did not return a valid character but an object of type %s.", class(res))
      }
      if (!testSubset(res, getTaskFeatureNames(task))) {
        wrong.names = setdiff(res, getTaskFeatureNames(task))
        stopf("bits.to.features returned feature names (%s) that are not in the task.", collapse(wrong.names))
      }
      return(res)
    }
  }
  assertClass(control, classes = "FeatSelControl")
  assertFlag(show.info)

  par.set = lapply(bit.names, function(bn) makeIntegerParam(bn))
  par.set = do.call(makeParamSet, par.set)
  # checkVarselParset(learner, par.set, bit.names, control)
  need.extra = control$tune.threshold || getMlrOption("on.error.dump")
  opt.path = makeOptPathDFFromMeasures(par.set, measures, include.extra = need.extra)
  control = setDefaultImputeVal(control, measures)

  cl = as.character(class(control))[1]
  if (show.info) {
    messagef("[FeatSel] Started selecting features for learner '%s'", learner$id)
    messagef("With control class: %s", cl)
    messagef("Imputation value: %g", control$impute.val)
  }
  sel.func = switch(cl,
    FeatSelControlRandom = selectFeaturesRandom,
    FeatSelControlExhaustive = selectFeaturesExhaustive,
    FeatSelControlSequential = selectFeaturesSequential,
    FeatSelControlGA = selectFeaturesGA
  )

  or = sel.func(learner, task, resampling, measures, bit.names,
    bits.to.features2, control, opt.path, show.info)
  if (show.info) {
    messagef("[FeatSel] Result: %s (%i bits)",
      clipString(collapse(or$x.bit.names), 30L), length(or$x.bit.names), perfsToString(or$y))
  }
  return(or)
}

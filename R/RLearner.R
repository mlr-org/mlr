#' @title Internal construction / wrapping of learner object.
#'
#' @description
#' Wraps an already implemented learning method from R to make it accessible to mlr.
#' Call this method in your constructor. You have to pass an id (name), the required
#' package(s), a description object for all changeable parameters (you do not have to do this for the
#' learner to work, but it is strongly recommended), and use property tags to define
#' features of the learner.
#'
#' For a general overview on how to integrate a learning algorithm into mlr's system, please read the
#' section in the online tutorial:
#' <https://mlr.mlr-org.com/articles/tutorial/create_learner.html>
#'
#' To see all possible properties of a learner, go to: [LearnerProperties].
#'
#' @template arg_lrncl
#' @param package ([character])\cr
#'   Package(s) to load for the implementation of the learner.
#' @param properties ([character])\cr
#'   Set of learner properties. See above.
#'   Default is `character(0)`.
#' @param class.weights.param (`character(1)`)\cr
#'   Name of the parameter, which can be used for providing class weights.
#' @param par.set ([ParamHelpers::ParamSet])\cr
#'   Parameter set of (hyper)parameters and their constraints.
#'   Dependent parameters with a `requires` field must use `quote` and not
#'   `expression` to define it.
#' @param par.vals ([list])\cr
#'   Always set hyperparameters to these values when the object is constructed.
#'   Useful when default values are missing in the underlying function.
#'   The values can later be overwritten when the user sets hyperparameters.
#'   Default is empty list.
#' @param name (`character(1)`)\cr
#'   Meaningful name for learner.
#'   Default is `id`.
#' @param short.name (`character(1)`)\cr
#'   Short name for learner.
#'   Should only be a few characters so it can be used in plots and tables.
#'   Default is `id`.
#' @param note (`character(1)`)\cr
#'   Additional notes regarding the learner and its integration in mlr.
#'   Default is \dQuote{}.
#' @param callees ([character])\cr
#'   Character vector naming all functions of the learner's package being called which
#'   have a relevant R help page.
#'   Default is `character(0)`.
#' @return ([RLearner]). The specific subclass is one of [RLearnerClassif],
#'   [RLearnerCluster], [RLearnerMultilabel],
#'   [RLearnerRegr], [RLearnerSurv].
#' @name RLearner
#' @rdname RLearner
#' @aliases RLearnerClassif RLearnerCluster RLearnerMultilabel RLearnerRegr RLearnerSurv
NULL

#' @export
#' @rdname RLearner
makeRLearner = function() {
  UseMethod("makeRLearner")
}

makeRLearnerInternal = function(id, type, package, par.set, par.vals, properties,
  name = id, short.name = id, note = "", callees) {

  # must do that before accessing par.set
  # one case where lazy eval is actually helpful...
  assertCharacter(package, any.missing = FALSE)
  requirePackages(package, why = stri_paste("learner", id, sep = " "), default.method = "load")

  assertString(id)
  assertChoice(type, choices = c("classif", "regr", "multilabel", "surv", "cluster", "costsens"))
  assertSubset(properties, listLearnerProperties(type))
  assertClass(par.set, classes = "ParamSet")
  checkListElementClass(par.set$pars, "LearnerParam")
  assertList(par.vals)
  if (!isProperlyNamed(par.vals)) {
    stop("Argument par.vals must be a properly named list!")
  }
  assertString(name)
  assertString(short.name)
  assertString(note)
  assertCharacter(callees, any.missing = FALSE)
  learner = makeLearnerBaseConstructor("RLearner",
    id = id,
    type = type,
    package = package,
    properties = unique(properties),
    par.set = par.set,
    par.vals = par.vals,
    predict.type = "response"
  )
  learner$name = name
  learner$short.name = short.name
  learner$note = note
  learner$callees = callees
  learner$help.list = makeParamHelpList(callees, package, par.set)
  return(learner)

}

#' @export
#' @rdname RLearner
makeRLearnerClassif = function(cl, package, par.set, par.vals = list(), properties = character(0L),
  name = cl, short.name = cl, note = "", class.weights.param = NULL, callees = character(0L)) {
  lrn = addClasses(
    makeRLearnerInternal(cl, "classif", package, par.set, par.vals, properties, name, short.name, note, callees),
    c(cl, "RLearnerClassif")
  )

  # include the class.weights.param
  if ("class.weights" %in% getLearnerProperties(lrn)) {
    assertString(class.weights.param)
    if (!is.null(par.set$pars[[class.weights.param]])) {
      lrn$class.weights.param = class.weights.param
    } else {
      stopf("'%s' needs to be defined in the parameter set as well.", class.weights.param)
    }
  }
  return(lrn)
}

#' @export
#' @rdname RLearner
makeRLearnerMultilabel = function(cl, package, par.set, par.vals = list(), properties = character(0L), name = cl, short.name = cl, note = "", callees = character(0L)) {
  addClasses(
    makeRLearnerInternal(cl, "multilabel", package, par.set, par.vals, properties, name, short.name, note, callees),
    c(cl, "RLearnerMultilabel")
  )
}

#' @export
#' @rdname RLearner
makeRLearnerRegr = function(cl, package, par.set, par.vals = list(), properties = character(0L), name = cl, short.name = cl, note = "", callees = character(0L)) {
  addClasses(
    makeRLearnerInternal(cl, "regr", package, par.set, par.vals, properties, name, short.name, note, callees),
    c(cl, "RLearnerRegr")
  )
}

#' @export
#' @rdname RLearner
makeRLearnerSurv = function(cl, package, par.set, par.vals = list(), properties = character(0L), name = cl, short.name = cl, note = "", callees = character(0L)) {
  addClasses(
    makeRLearnerInternal(cl, "surv", package, par.set, par.vals, properties, name, short.name, note, callees),
    c(cl, "RLearnerSurv")
  )
}

#' @export
#' @rdname RLearner
makeRLearnerCluster = function(cl, package, par.set, par.vals = list(), properties = character(0L), name = cl, short.name = cl, note = "", callees = character(0L)) {
  addClasses(
    makeRLearnerInternal(cl, "cluster", package, par.set, par.vals, properties, name, short.name, note, callees),
    c(cl, "RLearnerCluster")
  )
}

#' @export
#' @rdname RLearner
makeRLearnerCostSens = function(cl, package, par.set, par.vals = list(), properties = character(0L),
  name = cl, short.name = cl, note = "", callees = character(0L)) {
  lrn = addClasses(
    makeRLearnerInternal(cl, "costsens", package, par.set, par.vals, properties, name, short.name, note, callees),
    c(cl, "RLearnerCostSens")
  )

  return(lrn)
}

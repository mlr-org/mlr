#' @include options.R
##################################
### Creator                    ###
##################################

#' @title Create a custom CPO constructor
#'
#' @description
#' Create a function-based CPO constructor. This has the advantage of being possibly more
#' elegant and less boilerplate-heavy than object-based CPOs, but it uses more memory
#' and is a bit harder to debug. For object-based CPOs, see \code{\link{makeCPOFunctional}}.
#'
#' @param .cpo.name [\code{character(1)}]\cr
#'   The name of the resulting CPO constructor / CPO. This is used for identification in output.
#' @param ...
#'   Parameters of the CPO, in the format of \code{\link{paramSetSugar}}.
#' @param .par.set [\code{ParamSet}]\cr
#'   Optional parameter set. If this is not \code{NULL}, the \dQuote{...} parameters are ignored.
#' @param .par.vals [\code{list}]\cr
#'   Named list of default parameter values for the CPO. These are used additionally to the
#'   parameter default values in \dQuote{...} and \code{.par.set}. It is preferred to use
#'   these default values, and not \code{.par.vals}.
#' @param .datasplit [\code{character(1)}]\cr
#'   Indicate what format the data should be as seen by \dQuote{cpo.trafo}. Possibilities are:
#'   \itemize{
#'     \item target the \dQuote{data} variable contains the data in a data.frame without
#'       the target column(s), the \dQuote{target} variable contains the target column(s) in
#'       a data.frame.
#'     \item most the \dQuote{data} is a list containing three data.frames: \dQuote{numeric}
#'       the numeric columns, \dQuote{factor} the factorial columns (ordered and unordered),
#'       \dQuote{other} the columns that are neither numeric nor factors. The \dQuote{target}
#'       variable contains the target column(s) in a data.frame.
#'     \item all similarly to \dQuote{most}, but factors are additionally split up into \dQuote{factor}
#'       (unordered factors) and \dQuote{ordered}.
#'     \item no the \dQuote{data} variable contains a data.frame with all data, the \dQuote{target}
#'       variable is a \code{character} indicating the names of the target columns.
#'     \item task the \dQuote{data} variable contains the data as a \dQuote{\link{Task}}.
#'   }
#'   Note that the returned data must always be in the same format as the one requested.
#'   Currently it is an error to change the target column(s) in the \dQuote{no} and \dQuote{task}
#'   cases. Default is \dQuote{target}.
#' @param .properties [\code{character}]\cr
#'   The kind if data that the CPO will be able to handle. This can be one or many of: \dQuote{numerics},
#'   \dQuote{factors}, \dQuote{ordered}, \dQuote{missings}.
#'   There should be a bias towards including properties. If a property is absent, the preproc
#'   operator will reject the data. If an operation e.g. only works on numeric columns that have no
#'   missings (like PCA), it is recommended to give all properties, ignore the columns that
#'   are not numeric (using \dQuote{.datasplit} = \dQuote{most}), and giving an error when
#'   there are missings in the numeric columns (since missings in factorial features are not a problem).
#'   Defaults to the maximal set.
#' @param .properties.adding [\code{character}]\cr
#'   Can be one or many of the same values as \dQuote{.properties}. These properties get added to
#'   a Learner (or CPO) coming after / behind this CPO. When a CPO imputes missing values, for example,
#'   this should be \dQuote{missings}. This must be a subset of \dQuote{.properties}. Default is
#'   \code{character(0)}.
#' @param .properties.needed [\code{character}]\cr
#'   Can be one or many of the same values as \dQuote{.properties}. These properties are required
#'   from a Learner (or CPO) coming after / behind this CPO. E.g., when a CPO converts factors to
#'   numerics, this should be \dQuote{numerics} (and \dQuote{.properties.adding} should be \dQuote{factors}).
#'   Default is \code{character(0)}.
#' @param cpo.trafo [\code{language} | \code{function}]\cr
#'   This can either be a function, just the expressions to perform wrapped in curly braces.
#'   If this is a function, it must have the parameters \dQuote{data} and \dQuote{target},
#'   as well as the parameters specified in \dQuote{...} or \dQuote{.par.set}. (Alternatively,
#'   the function may have a dotdotdot argument). It must return a \dQuote{data.frame} object
#'   with an added \dQuote{\link{retrafo}}. This must be a function with the argument
#'   \dQuote{data} and return another \dQuote{data.frame}.\cr
#'   If \dQuote{cpo.trafo} is a list of expressions (preferred), it is turned into a function
#'   by mlr, with the above mentioned criteria.
#' @family CPO
#' @examples
#' noop = makeCPOFunctional("noop", dummy: logical, cpo.trafo = {
#'   retrafo(data) = function(data) data
#'   data
#' })
#'
#' @export
makeCPOFunctional = function(.cpo.name, ..., .par.set = NULL, .par.vals = list(),
                             .datasplit = c("no", "target", "most", "all", "no", "task"),  # TODO: put "no" after "all"
                             .properties = c("numerics", "factors", "ordered", "missings"),
                             .properties.adding = character(0), .properties.needed = character(0),
                             cpo.trafo) {
  # dotted parameter names are necessary to avoid problems with partial argument matching.
  cpo.name = .cpo.name
  par.set = .par.set
  par.vals = .par.vals
  assertList(par.vals, names = "unique")
  assertString(cpo.name)
  if (is.null(par.set)) {
    par.set = paramSetSugar(..., .pss.env = parent.frame())
  }

  .datasplit = match.arg(.datasplit)
  .properties = match.arg(.properties, several.ok = TRUE)
  assertSubset(.properties.adding, .properties)
  assertSubset(.properties.needed, c("numerics", "factors", "ordered", "missings"))
  badprops = intersect(.properties.adding, .properties.needed)
  if (length(badprops)) {
    stopf(".properties.adding and .properties.needed must not contain the same properties, but both contained %s.",
      collapse(badprops, sep = ", "))
  }

  # these parameters are either special parameters given to the constructor function (id),
  # special parameters given to the cpo.trafo function (data, target)

  reserved.params = c("data", "target", "id")
  if (any(names(par.set$pars) %in% reserved.params)) {
    stopf("Parameters %s are reserved", collapse(reserved.params, ", "))
  }

  par.vals = insert(getParamSetDefaults(par.set), par.vals)

  assert(length(setdiff(names(par.vals), names(par.set$pars))) == 0)

  checkParamsFeasible(par.set, par.vals)

  funargs = lapply(par.set$pars, function(dummy) substitute())
  funargs = insert(funargs, par.vals)

  required.arglist.trafo = funargs
  required.arglist.trafo$data = substitute()
  required.arglist.trafo$target = substitute()
  cpo.trafo = makeFunction(substitute(cpo.trafo), required.arglist.trafo, env = parent.frame())
  funargs = insert(funargs, list(id = NULL))
  cpo.trafo = captureEnvWrapper(cpo.trafo)

  funbody = quote({
    args = base::match.call()
    base::rm(list = base::setdiff(base::ls(), "args"))  # delete all arguments to avoid name clashes
    args[[1]] = quote(list)
    args = eval(args, envir = parent.frame())
    args = insert(funargs, args)
    if (!is.null(args$id)) {
      assertString(args$id, .var.name = "id")
    }

    present.pars = Filter(function(x) !identical(x, substitute()), args[names(par.set$pars)])
    checkParamsFeasible(par.set, present.pars)
    par.set = par.set  # get par.set into current env, same with properties.*
    properties = .properties
    properties.adding = .properties.adding
    properties.needed = .properties.needed

    outerTrafo = function(data, .par.vals) {

      upper.retrafo = retrafo(data)
      is.prim = is.null(upper.retrafo)
      args = subsetParams(.par.vals, par.set, cpo.name)

      # prepare input
      tin = prepareTrafoInput(data, datasplit, properties, cpo.name)

      result = do.call(cpo.trafo, insert(args, tin$indata))

      trafoenv = environment(cpo.trafo)$.ENV
      assign(".ENV", NULL, envir = environment(cpo.trafo))

      trafoenv$data = NULL

      retrafo.fn = trafoenv$cpo.retrafo
      if (is.null(retrafo.fn) || !isTRUE(checkFunction(retrafo.fn, nargs = 1))) {
        stopf("CPO %s cpo.trafo did not set a variable 'cpo.retrafo' to a function with one argument.", cpo.name)
      }
      if (!"data" %in% names(formals(retrafo.fn)) && referencesNonfunctionNames(body(retrafo.fn), "data")) {
        warning(paste("The function given as cpo.retrafo references a 'data' variable.",
          "Beware that the 'data' variable as given as an argument to the surrounding function",
          "will not be accessible when cpo.retrafo is called.",
          "If you still need to access this data, copy it to a variable with a different name.",
          "If this warning is a false positive and you assign the 'data' variable properly, you can avoid",
          "this warning by giving it a name different from 'data'.", sep = "\n"))
      }

      # check result & retrafo
      allowed.properties = union(tin$properties, properties.needed)
      tout = handleTrafoOutput(result, data, datasplit, allowed.properties, properties.adding, cpo.name)

      returning = tout$outdata

      retrafo.info = list(
          args = args,
          par.set = par.set,
          cpo.name = cpo.name,
          properties = .properties,
          properties.adding = .properties.adding,
          properties.needed = .properties.needed,
          datasplit = .datasplit,
          shapeinfo.input = tin$shapeinfo,
          shapeinfo.output = tin$shapeinfo)

      retrafo(returning) = addClasses(cpoFunctionalRetrafo(retrafo.fn, upper.retrafo, retrafo.info),
        c(if (is.prim) "CPOFunctionalRetrafoPrimitive", "CPOFunctionalRetrafo", "CPORetrafo"))

      returning
    }
    # can't do the following in function head, since par.vals must be eval'd
    formals(outerTrafo) = as.pairlist(list(task = substitute(), .par.vals = present.pars))
    attr(outerTrafo, "name") = cpo.name
    attr(outerTrafo, "barename") = cpo.name
    attr(outerTrafo, "id") = NULL
    outerTrafo = addClasses(outerTrafo, c("CPOPrimitive", "CPOFunctional", "CPO"))  # nolint
    setCPOId(outerTrafo, args$id)
  })
  addClasses(eval(call("function", as.pairlist(funargs), funbody)), c("CPOFunctionalConstructor", "CPOConstructor"))
}


##################################
### Primary Trafo Operations   ###
##################################

# CPO %>>% CPO

#' @export
composeCPO.CPOFunctional = function(cpo1, cpo2) {
  # in theory we could just do function composition, but then we
  # would lose the ability to setHyperPars().
  assertClass(cpo2, "CPOFunctional")
  parameterClashAssert(cpo1, cpo2, attr(cpo1, "name"), attr(cpo2, "name"))
  par.set = c(getParamSet(cpo1), getParamSet(cpo2))
  outerTrafo = function(task, .par.vals) {
    pv1names = names(getParamSet(cpo1)$pars)
    pv2names = names(getParamSet(cpo2)$pars)
    assert(length(intersect(pv1names, pv2names)) == 0)
    assert(length(setdiff(names(.par.vals), c(pv1names, pv2names))) == 0)
    setHyperPars(cpo2, par.vals = .par.vals[intersect(names(.par.vals), pv2names)])(
      setHyperPars(cpo1, par.vals = .par.vals[intersect(names(.par.vals), pv1names)])(task))
  }
  formals(outerTrafo) = as.pairlist(list(task = substitute(), .par.vals = c(getHyperPars(cpo1), getHyperPars(cpo2))))
  attr(outerTrafo, "name") = paste(attr(cpo1, "name"), attr(cpo2, "name"), sep = " >> ")
  attr(outerTrafo, "barename") = paste(attr(cpo2, "barename"), attr(cpo1, "barename"), sep = ".")
  addClasses(outerTrafo, c("CPOFunctional", "CPO"))
}

# CPO %>>% LEARNER

#' @export
attachCPO.CPOFunctional = function(cpo, learner) {
  learner = checkLearner(learner)
  id = paste(learner$id, attr(cpo, "barename"), sep = ".")
  # makeBaseWrapper checks for parameter name clash, but gives
  # less informative error message
  parameterClashAssert(cpo, learner, attr(cpo, "name"), getLearnerName(learner))

  oldprops = getLearnerProperties(learner)
  oldprops.relevant = intersect(oldprops, c("numerics", "factors", "ordered", "missings"))
  oldprops.relevant = compositeProperties(cpo$properties, cpo$properties.adding, cpo$properties.needed,
    oldprops.relevant, character(0), character(0), cpo$name, getLearnerName(learner))$properties  # checks for property problems automatically

  wlearner = makeBaseWrapper(id, learner$type, learner, learner$package,
    getParamSet(cpo), getHyperPars(cpo), c("CPOFunctionalLearner", "CPOLearner"), c("CPOFunctionalModel", "CPOModel"))
  wlearner$cpo = cpo
  wlearner$properties = c(oldprops.relevant, setdiff(oldprops, c("numerics", "factors", "ordered", "missings")))
  wlearner
}

#' @export
trainLearner.CPOFunctionalLearner = function(.learner, .task, .subset = NULL, ...) {
  cpo = setHyperPars(.learner$cpo, par.vals = .learner$par.vals)

  .task = cpo(subsetTask(.task, .subset))
  retrafo = retrafo(.task)
  retrafo(.task) = NULL

  model = makeChainModel(train(.learner$next.learner, .task), "CPOFunctionalWrappedModel")
  model$retrafo = retrafo
  model
}

#' @export
predictLearner.CPOFunctionalLearner = function(.learner, .model, .newdata, ...) {
  .newdata = .model$learner.model$retrafo(.newdata)
  NextMethod(.newdata = .newdata)
}

# DATA %>>% CPO

#' @export
applyCPO.CPOFunctional = function(cpo, task) {
  cpo(task)
}

# get CPO from learner

singleLearnerCPO.CPOObjectLearner = function(learner) {
  cpo = learner$cpo
  setHyperPars(cpo, subsetParams(learner$par.vals, cpo$par.set, cpo$name))
}

# CPO splitting

#' @export
as.list.CPOFunctional = function(x, ...) {
  assert(length(list(...)) == 0)
  cpo1 = substitute()  # pacify static checker
  cpo2 = substitute()  # pacify static checker
  catabolize = function(task, .par.vals) {
    pv1names = names(getParamSet(cpo1)$pars)
    pv2names = names(getParamSet(cpo2)$pars)
    assert(length(intersect(pv1names, pv2names)) == 0)
    assert(length(setdiff(names(.par.vals), c(pv1names, pv2names))) == 0)
    c(as.list(setHyperPars(cpo1, par.vals = .par.vals[intersect(names(.par.vals), pv1names)])),
      as.list(setHyperPars(cpo2, par.vals = .par.vals[intersect(names(.par.vals), pv2names)])))
  }
  formals(catabolize) = formals(x)
  environment(catabolize) = environment(x)
  catabolize(NULL)
}

# Param Sets

#' @export
getParamSet.CPOFunctional = function(x) {
  ps = environment(x)$par.set
  id = attr(x, "id")
  if (!is.null(id) && length(ps$pars)) {
    nametranslation = setNames(paste(id, names(ps$pars), sep = "."), names(ps$pars))
    names(ps$pars) = nametranslation

    ps$pars = lapply(ps$pars, function(x) {
      x$id = paste(id, x$id, sep = ".")
      if (!is.null(x$requires)) {
        x$requires = renameNonfunctionNames(x$requires, nametranslation)
      }
      x
    })
  }
  ps
}

#' @export
getHyperPars.CPOFunctional = function(learner, for.fun = c("train", "predict", "both")) {
  id = attr(learner, "id")
  pv = formals(learner)$.par.vals
  if (!is.null(id) && length(pv) > 0) {
    names(pv) = paste(id, names(pv), sep = ".")
  }
  pv
}

#' @export
setHyperPars2.CPOFunctional = function(learner, par.vals = list()) {
  ps = getParamSet(learner)
  id = attr(learner, "id")
  badpars = setdiff(names(par.vals), names(ps$pars))
  if (length(badpars)) {
    stopf("CPO %s does not have parameter%s %s", getCPOName(learner),
          ifelse(length(badpars) > 1, "s", ""), coalesce(badpars, ", "))
  }
  checkParamsFeasible(ps, par.vals)
  pv = getHyperPars(learner)
  pv = insert(pv, par.vals)
  if (!is.null(id)) {
    names(pv) = stri_sub(names(pv), nchar(id) + 2)
  }
  at = attributes(learner)
  formals(learner) = as.pairlist(list(task = substitute(), .par.vals = pv))
  attributes(learner) = at
  learner
}

# Properties

#' @export
getCPOProperties.CPOFunctional = function(cpo) {
  list(properties = environment(cpo)$properties,
    properties.adding = environment(cpo)$properties.adding,
    properties.needed = environment(cpo)$properties.needed)
}

# CPO ID, NAME

#' @export
getCPOName.CPOFunctional = function(cpo) {
  attr(cpo, "name")
}

setCPOId.CPOFunctional = function(cpo, id) {
  if (!is.null(id)) {
    assertString(id)
  }
  if (!"CPOPrimitive" %in% class(cpo)) {
    stop("Cannot set ID of compound CPO.")
  }
  attr(cpo, "id") = id
  attr(cpo, "name") = collapse(c(attr(cpo, "barename"), id), sep = ".")
  cpo
}

##################################
### Primary Retrafo Operations ###
##################################

# RETRAFO main function
cpoFunctionalRetrafo = function(retrafo.fn, upper.retrafo, retrafo.info) {

  is.prim = is.null(upper.retrafo)
  # chain retrafo
  lower.retrafo = retrafo.fn  # lower.retrafo must be present in any case, for later chaining of retrafos
  if (!is.prim) {
    assertFunction(upper.retrafo, nargs = 1, .var.name = "retrafo(task)")
    retrafo.fn = function(data) {
      lower.retrafo(upper.retrafo(data))
    }
  }
  function(data) {
    tin = prepareRetrafoInput(data, retrafo.info$datasplit,
      retrafo.info$properties, retrafo.info$shapeinfo.input, retrafo.info$cpo.name)

    result = retrafo.fn(tin$indata)

    allowed.properties = union(tin$properties, retrafo.info$properties.needed)
    handleRetrafoOutput(result, data, retrafo.info$datasplit,
      allowed.properties, retrafo.info$properties.adding, retrafo.info$shapeinfo.output, retrafo.info$cpo.name)
  }
}

# get RETRAFO from learner

singleModelRetrafo.CPOFunctionalModel = function(model, prevfun) {
  if (!is.null(prevfun)) {
    prevfun %>>% model$learner.model$retrafo
  } else {
    model$learner.model$retrafo
  }
}

# RETRAFO %>>% RETRAFO


#' @export
composeCPO.CPOFunctionalRetrafo = function(cpo1, cpo2) {
  assertClass(cpo2, "CPOFunctionalRetrafo")
  oldenv = environment(cpo2)
  assert(is.null(oldenv$upper.retrafo) == ("CPOFunctionalRetrafoPrimitive" %in% class(cpo2)))

  cpo2 = copyFunctionalRetrafo(cpo2)

  if (!is.null(oldenv$upper.retrafo)) {
    cpo1 = cpo1 %>>% oldenv$upper.retrafo
  }
  class(cpo2) = setdiff(class(cpo2), "CPOFunctionalRetrafoPrimitive")

  environment(cpo2)$upper.retrafo = cpo1

  lower.retrafo = substitute()  # pacify static checker
  upper.retrafo = substitute()  # pacify static checker

  retrafo.fn = function(data) {
    lower.retrafo(upper.retrafo(data))
  }
  environment(retrafo.fn) = environment(cpo2)
  environment(cpo2)$retrafo.fn = retrafo.fn
  cpo2
}

# DATA %>>% RETRAFO

#' @export
predict.CPOFunctionalRetrafo = function(object, data, ...) {
  assert(length(list(...)) == 0)
  object(data)
}

# RETRAFO splitting

#' @export
as.list.CPOFunctionalRetrafo = function(x, ...) {
  assert(length(list(...)) == 0)
  oldenv = environment(x)
  x = copyFunctionalRetrafo(x)
  if (!is.null(oldenv$upper.retrafo)) {
    assertClass(oldenv$upper.retrafo, "CPORetrafo")
  }
  c(as.list(oldenv$upper.retrafo), list(x))
}

copyFunctionalRetrafo = function(rtf) {
  # used vars:
  #  retrafo.fn: the retrafo being used.
  #  lower.retrafo: original retrafo
  #  cpo.name, args, par.set
  copyvars = c("lower.retrafo", "retrafo.info")
  oldenv = environment(rtf)
  newenv = new.env(parent = parent.env(oldenv))
  for (cp in copyvars) {
    newenv[[cp]] = oldenv[[cp]]
  }
  newenv$retrafo.fn = newenv$lower.retrafo  # cut out next retrafo
  environment(rtf) = newenv
  class(rtf) = union("CPOFunctionalRetrafoPrimitive", class(rtf))
  rtf
}

# RETRAFO State

#' @export
getRetrafoState.CPOFunctionalRetrafoPrimitive = function(retrafo.object) {
  res = as.list(environment(environment(retrafo.object)$retrafo.fn))
  # the 'cpo.retrafo' contains the retrafo.fn, which is 'primitive',
  # so no need to copy it.
  retrafo.info = environment(retrafo.object)$retrafo.info
  res$data = list(shapeinfo.input = retrafo.info$shapeinfo.input,
    shapeinfo.output = retrafo.info$shapeinfo.output)
  res
}

#' @export
makeRetrafoFromState.CPOFunctionalConstructor = function(constructor, state) {
  assertList(state, names = "unique")
  assertSubset("cpo.retrafo", names(state))

  bare = constructor()

  data = state$data
  state$data = NULL

  retrafo.info = insert(list(shapeinfo.input = data$shapeinfo.input,
    shapeinfo.output = data$shapeinfo.output,
    args = list(),
    par.set = getParamSet(bare),
    cpo.name = getCPOName(bare),
    datasplit = environment(bare)$.datasplit), getCPOProperties(bare))



  retrafo.fn = state$cpo.retrafo

  env = new.env(parent = parent.env(environment(retrafo.fn)))

  list2env(state, envir = env)

  environment(retrafo.fn) = env

  env$cpo.retrafo = retrafo.fn  # in case of recursion

  # par.set set to empty because the parameters are not part of the state.
  retr = cpoFunctionalRetrafo(retrafo.fn, NULL, retrafo.info)
  addClasses(retr, c("CPOFunctionalRetrafoPrimitive", "CPOFunctionalRetrafo", "CPORetrafo"))
}



# Param Sets

#' @export
getParamSet.CPOFunctionalRetrafoPrimitive = function(x) {
  environment(x)$retrafo.info$par.set
}

#' @export
getHyperPars.CPOFunctionalRetrafoPrimitive = function(learner, for.fun = c("train", "predict", "both")) {
  pv = environment(learner)$retrafo.info$args
  pv$data = NULL
  pv$target = NULL
  pv
}

#' @export
getCPOProperties.CPOFunctionalRetrafo = function(cpo) {
  list(properties = environment(cpo)$retrafo.info$properties,
    properties.adding = environment(cpo)$retrafo.info$properties.adding,
    properties.needed = environment(cpo)$retrafo.info$properties.needed)
}

#' @export
getCPOName.CPOFunctionalRetrafoPrimitive = function(cpo) {
  environment(cpo)$retrafo.info$cpo.name
}

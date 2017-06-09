##################################
### Creator                    ###
##################################

#' @title Create a custom CPO constructor
#'
#' @description
#' Create an object-based CPO constructor. This has the advantage of being more memory efficient
#' and being easier to debug, but possibly less elegant than function-based CPOs (as created
#' by \code{\link{makeCPOFunctional}}).
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
#'   the function may have a dotdotdot argument). It must return a \dQuote{data.frame} object.
#'   Furthermore, it must create a \dQuote{control} variable in its namespace, which will
#'   be passed on to \dQuote{cpo.retrafo}.\cr
#'   If \dQuote{cpo.trafo} is a list of expressions (preferred), it is turned into a function
#'   by mlr, with the above mentioned criteria.
#' @param cpo.retrafo [\code{language} | \code{function}]\cr
#'   Similarly to \dQuote{cpo.trafo}, this is either a function or a sequence of expressions
#'   in curly braces (preferred). This function must have the same arguments, except that
#'   instead of a \dQuote{target} argument, it has a \dQuote{control} argument, which will be
#'   the value created in the \dQuote{cpo.trafo} run. This function must similarly return a
#'   \dQuote{data.frame} object.
#' @family CPO
#' @examples
#' noop = makeCPOObject("noop", dummy: logical, cpo.trafo = {
#'   control = 0
#'   data
#' }, cpo.retrafo = { data })
#'
#' @export
makeCPOObject = function(.cpo.name, ..., .par.set = NULL, .par.vals = list(),
                         .datasplit = c("no", "target", "most", "all", "task"),  # TODO: put "no" after "all"
                         .properties = c("numerics", "factors", "ordered", "missings"),
                         .properties.adding = character(0), .properties.needed = character(0),
                         cpo.trafo, cpo.retrafo) {
  # dotted parameter names are necessary to avoid problems with partial argument matching.
  cpo.name = .cpo.name
  par.set = .par.set
  par.vals = .par.vals
  assertString(cpo.name)
  assertList(par.vals, names = "unique")
  if (is.null(par.set)) {
    par.set = paramSetSugar(..., .pss.env = parent.frame())
  }

  .datasplit = match.arg(.datasplit)
  .properties = match.arg(.properties, several.ok = TRUE)
  assertSubset(.properties.adding, .properties)
  assertSubset(.properties.needed, c("numerics", "factors", "ordered", "missings"))
  badprops = intersect(.properties.adding, .properties.needed)
  if (length(badprops)) {
    stop(".properties.adding and .properties.needed must not contain the same properties, but both contained %s.",
      collapse(badprops, sep = ", "))
  }

  # these parameters are either special parameters given to the constructor function (id),
  # special parameters given to the cpo.trafo function (data, target), special parameters given to the
  # cpo.retrafo function (control),

  reserved.params = c("data", "target", "control", "id")
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

  cpo.trafo = captureEnvWrapper(cpo.trafo)

  required.arglist.retrafo = funargs
  required.arglist.retrafo$data = substitute()
  required.arglist.retrafo$control = substitute()
  cpo.retrafo = makeFunction(substitute(cpo.retrafo), required.arglist.retrafo, env = parent.frame())

  funargs = insert(funargs, list(id = NULL))

  funbody = quote({
    args = base::match.call()
    base::rm(list = base::setdiff(base::ls(), "args"))  # delete all arguments to avoid name clashes
    args[[1]] = quote(list)
    args = eval(args, envir = parent.frame())
    args = insert(funargs, args)
    if (!is.null(args$id)) {
      assertString(args$id)
    }
    present.pars = Filter(function(x) !identical(x, substitute()), args[names(par.set$pars)])
    checkParamsFeasible(par.set, present.pars)
    cpo = makeS3Obj(base::c("CPOPrimitive", "CPOObject", "CPO"),
      barename = cpo.name,
      name = cpo.name,
      id = NULL,
      bare.par.names = names(par.set$pars),
      par.set = par.set,
      par.vals = present.pars,
      properties = .properties,
      properties.adding = .properties.adding,
      properties.needed = .properties.needed,
      datasplit = .datasplit,
      trafo = cpo.trafo,
      retrafo = cpo.retrafo)
    setCPOId(cpo, args$id)  # this also adjusts par.set and par.vals
  })
  addClasses(eval(call("function", as.pairlist(funargs), funbody)), c("CPOObjectConstructor", "CPOConstructor"))
}

##################################
### Primary Trafo Operations   ###
##################################

# TRAFO main function
# - checks the inbound and outbound data is in the right format
# - data will be turned into the shape requested by the cpo
# - properties check (inbound, and outbound)
# - automatically subsets 'args' to the relevant ones for cpo
# - collects control from called function
# - returns list(data, info = list(control, shapeinfo.input, shapeinfo.output)

# receiver.properties are the properties of the next layer
callCPOObjectTrafo = function(cpo, data) {
  tin = prepareTrafoInput(data, cpo$datasplit, cpo$properties, cpo$name)

  result = do.call(cpo$trafo, insert(getBareHyperPars(cpo), tin$indata))

  trafoenv = environment(cpo$trafo)$.ENV
  assign(".ENV", NULL, envir = environment(cpo$trafo))
  if (!"control" %in% ls(trafoenv)) {
    stopf("CPO %s cpo.trafo did not create a 'control' object.", cpo$name)
  }

  # the properties of the output should only be the input properties + the ones we're adding
  allowed.properties = setdiff(tin$properties, cpo$properties.adding)
  tout = handleTrafoOutput(result, data, cpo$datasplit, allowed.properties, cpo$name)

  list(data = tout$outdata, info = list(control = trafoenv$control,
    shapeinfo.input = tin$shapeinfo, shapeinfo.output = tout$shapeinfo))
}

# CPO %>>% CPO

#' @export
composeCPO.CPOObject = function(cpo1, cpo2) {
  assertClass(cpo2, "CPOObject")
  parameterClashAssert(cpo1, cpo2, cpo1$name, cpo2$name)
  newprops = compositeProperties(
      cpo1$properties, cpo1$properties.adding, cpo1$properties.needed,
      cpo2$properties, cpo2$properties.adding, cpo2$properties.needed,
      cpo1$name, cpo2$name)

  makeS3Obj(c("CPOObject", "CPO"),
    barename = paste(cpo2$barename, cpo1$barename, sep = "."),
    name = paste(cpo1$name, cpo2$name, sep = " >> "),
    bare.par.names = c(names(cpo1$par.set$pars), names(cpo2$par.set$pars)),
    par.set = c(cpo1$par.set, cpo2$par.set),
    par.vals = c(cpo1$par.vals, cpo2$par.vals),
    properties = newprops$properties,
    properties.adding = newprops$properties.adding,
    properties.needed = newprops$properties.needed,
    datasplit = "task",
    trafo = captureEnvWrapper(function(data, ...) {
      args = list(...)
      cpo1$par.vals = subsetParams(args, cpo1$par.set, cpo1$name)
      cpo2$par.vals = subsetParams(args, cpo2$par.set, cpo2$name)
      result = callCPOObjectTrafo(cpo1, data)
      result2 = callCPOObjectTrafo(cpo2, result$data)
      control = list(fun1 = result$info, fun2 = result2$info)
      result2$data
    }),
    retrafo = function(data, control, ...) {
      args = list(...)
      cpo1$par.vals = subsetParams(args, cpo1$par.set, cpo1$name)
      cpo2$par.vals = subsetParams(args, cpo2$par.set, cpo2$name)
      step1 = getCPOObjectRetrafoFn(cpo1, control$fun1, NULL)
      step2 = getCPOObjectRetrafoFn(cpo2, control$fun2, step1)
      finalres = step2(data)
      finalres
    })
}

# CPO %>>% LEARNER

#' @export
attachCPO.CPOObject = function(cpo, learner) {
  learner = checkLearner(learner)
  id = paste(learner$id, cpo$barename, sep = ".")
  # makeBaseWrapper checks for parameter name clash, but gives
  # less informative error message
  parameterClashAssert(cpo, learner, cpo$name, getLearnerName(learner))

  oldprops = getLearnerProperties(learner)
  oldprops.relevant = intersect(oldprops, c("numerics", "factors", "ordered", "missings"))
  oldprops.relevant = compositeProperties(cpo$properties, cpo$cpoproperties.adding, cpo$properties.needed,
    oldprops.relevant, character(0), character(0))$properties  # checks for property problems automatically


  wlearner = makeBaseWrapper(id, learner$type, learner, learner$package,
    cpo$par.set, cpo$par.vals, c("CPOObjectLearner", "CPOLearner"), c("CPOObjectModel", "CPOModel"))
  wlearner$cpo = cpo

  wlearner$properties = c(oldprops.relevant, setdiff(oldprops, c("numerics", "factors", "ordered", "missings")))

  wlearner
}

#' @export
trainLearner.CPOObjectLearner = function(.learner, .task, .subset = NULL, ...) {
  if (!is.null(.subset)) {
    .task = subsetTask(.task, .subset)
  }
  cpo = .learner$cpo
  cpo$par.vals = subsetParams(.learner$par.vals, cpo$par.set, cpo$name)

  transformed = callCPOObjectTrafo(cpo, .task)

  model = makeChainModel(train(.learner$next.learner, transformed$data), "CPOObjectWrappedModel")
  model$info = transformed$info
  model$cpo = cpo
  model
}

#' @export
predictLearner.CPOObjectLearner = function(.learner, .model, .newdata, ...) {
  retrafo.fn = getCPOObjectRetrafoFn(.model$learner.model$cpo, .model$learner.model$info, NULL)
  NextMethod(.newdata = retrafo.fn(.newdata))
}

# DATA %>>% CPO

#' @export
applyCPO.CPOObject = function(cpo, task) {
  prevfun = retrafo(task)

  transformed = callCPOObjectTrafo(cpo, task)
  task = transformed$data

  retrafo.fn = getCPOObjectRetrafoFn(cpo, transformed$info, prevfun)
  is.prim = (is.null(prevfun) && "CPOPrimitive" %in% class(cpo))
  retrafo(task) = addClasses(retrafo.fn, c(if (is.prim) "CPOObjectRetrafoPrimitive", "CPOObjectRetrafo", "CPORetrafo"))
  task
}

# CPO splitting

#' @export
as.list.CPOObject = function(x, ...) {
  applyParams = function(cpo, par.vals) {
    assertClass(cpo, "CPOObject")
    relevant.names = intersect(names(cpo$par.set$pars), names(par.vals))
    # apply the parameter set to the cpo
    if (length(relevant.names)) {
      setHyperPars(cpo, par.vals = par.vals[relevant.names])
    } else {
      cpo
    }
  }
  cpo1 = parent.env(environment(x$trafo))$cpo1
  cpo2 = parent.env(environment(x$trafo))$cpo2
  par.vals = getHyperPars(x)
  c(as.list(applyParams(cpo1, par.vals)),
    as.list(applyParams(cpo2, par.vals)))
}

# Param Sets

#' @export
getParamSet.CPOObject = function(x) {
  x$par.set
}

#' @export
getHyperPars.CPOObject = function(learner, for.fun = c("train", "predict", "both")) {
  learner$par.vals
}

#' @export
setHyperPars2.CPOObject = function(learner, par.vals = list()) {
  badpars = setdiff(names(par.vals), names(learner$par.set$pars))
  if (length(badpars)) {
    stopf("CPO %s does not have parameter%s %s", getLearnerName(learner),
          ifelse(length(badpars) > 1, "s", ""), collapse(badpars, ", "))
  }
  checkParamsFeasible(learner$par.set, par.vals)
  learner$par.vals = insert(learner$par.vals, par.vals)
  learner
}

# get par.vals with bare par.set names
getBareHyperPars = function(cpo) {
  args = cpo$par.vals
  if (length(args)) {
    namestranslation = cpo$bare.par.names
    names(namestranslation) = names(cpo$par.set$pars)
    names(args) = namestranslation[names(args)]
  }
  args
}

# Properties

getCPOProperties.CPOObject = function(cpo, which = NULL) {
  list(properties = cpo$properties,
    properties.adding = cpo$properties.adding,
    properties.needed = cpo$properties.needed)
}

# CPO ID, NAME

#' @export
getCPOName.CPOObject = function(cpo) {
  cpo$name
}

setCPOId.CPOObject = function(cpo, id) {
  if (!is.null(id)) {
    assertString(id)
  }
  if (!"CPOPrimitive" %in% class(cpo)) {
    stop("Cannot set ID of compound CPO.")
  }
  pasteIdIfNN = function(names) {
    if (is.null(id)) {
      names
    } else {
      paste(id, names, sep = ".")
    }
  }
  par.names = names(cpo$par.set$pars)
  bare.par.vals.names = cpo$bare.par.names[match(names(cpo$par.vals), par.names)]
  if (length(cpo$par.vals)) {
    names(cpo$par.vals) = pasteIdIfNN(bare.par.vals.names)
  }

  if (length(cpo$bare.par.names)) {
    newparnames = pasteIdIfNN(cpo$bare.par.names)
    names(cpo$par.set$pars) = newparnames

    names(newparnames) = par.names  # translation table: old names -> new names
    for (n in newparnames) {
      cpo$par.set$pars[[n]]$id = n
      if (!is.null(cpo$par.set$pars[[n]]$requires)) {
        cpo$par.set$pars[[n]]$requires = renameNonfunctionNames(
            cpo$par.set$pars[[n]]$requires, newparnames)
      }
    }
  }

  cpo$id = id
  cpo$name = collapse(c(cpo$barename, id), sep = ".")
  cpo
}


##################################
### Primary Retrafo Operations ###
##################################

# RETRAFO main function

# - checks the inbound and outbound data is in the right format
# - checks the shape of input and output is as was before
# - data will be turned into the shape requested by the cpo
# - properties check (inbound, and outbound)
# - automatically subsets 'args' to the relevant ones for cpo
# - returns the resulting data

# info is list(control, shapeinfo.input, shapeinfo.output)
# receiver.properties are the properties of the next layer

getCPOObjectRetrafoFn = function(cpo, info, prevfun) {
  function(data) {
    if (!is.null(prevfun)) {
      data = prevfun(data)
    }
    tin = prepareRetrafoInput(data, cpo$datasplit, cpo$properties, info$shapeinfo.input, cpo$barename)

    result = do.call(cpo$retrafo, insert(getBareHyperPars(cpo), list(data = tin$indata, control = info$control)))

    # the properties of the output should only be the input properties + the ones we're adding
    allowed.properties = union(tin$properties, cpo$properties.adding)

    handleRetrafoOutput(result, data, cpo$datasplit, allowed.properties, info$shapeinfo.output, cpo$barename)
  }
}

# get RETRAFO from learner

singleModelRetrafo.CPOObjectModel = function(model, prevfun) {
  res = getCPOObjectRetrafoFn(model$learner.model$cpo, model$learner.model$info, prevfun)

  is.prim = (is.null(prevfun) && "CPOPrimitive" %in% class(model$learner$cpo))
  addClasses(res, c(if (is.prim) "CPOObjectRetrafoPrimitive", "CPOObjectRetrafo", "CPORetrafo"))
}

# RETRAFO %>>% RETRAFO

#' @export
`%>>%.CPOObjectRetrafo` = function(cpo1, cpo2) {
  assertClass(cpo2, "CPOObjectRetrafo")
  oldenv = environment(cpo2)
  assert((is.null(oldenv$prevfun) && "CPOPrimitive" %in% class(oldenv$cpo)) ==
         ("CPOObjectRetrafoPrimitive" %in% class(cpo2)))

  cpo2 = copyCpoObjectRetrafo(cpo2)
  if (!is.null(oldenv$prevfun)) {
    cpo1 = cpo1 %>>% oldenv$prevfun
  }
  class(cpo2) = setdiff(class(cpo2), "CPOObjectRetrafoPrimitive")

  environment(cpo2)$prevfun = cpo1

  cpo2
}

# DATA %>>% RETRAFO

#' @export
predict.CPOObjectRetrafo = function(object, data, ...) {
  assert(length(list(...)) == 0)
  object(data)
}

# RETRAFO splitting

#' @export
as.list.CPOObjectRetrafoPrimitive = function(x, ...) {
  assert(length(list(...)) == 0)
  list(x)
}

#' @export
as.list.CPOObjectRetrafo = function(x, ...) {
  assert(length(list(...)) == 0)
  if (!is.null(environment(x)$prevfun)) {
    # chained via prevfun
    # call as.list on both fragments again, since they might still be chained
    # with at least one other method
    c(as.list(environment(x)$prevfun), as.list(copyCpoObjectRetrafo(x)))
  } else {
    # chained via composition
    cpoToRetrafo = function(cpo, pv, info) {
      cpo$par.vals = subsetParams(pv, cpo$par.set, cpo$name)

      res = getCPOObjectRetrafoFn(cpo, info, NULL)
      is.prim = ("CPOPrimitive" %in% class(cpo))
      addClasses(res, c(if (is.prim) "CPOObjectRetrafoPrimitive", "CPOObjectRetrafo", "CPORetrafo"))
    }
    ctl = environment(x)$info$control
    prs = getHyperPars(environment(x)$cpo)
    retenv = environment(environment(x)$cpo$retrafo)
    c(as.list(cpoToRetrafo(retenv$cpo1, prs, ctl$fun1)),
      as.list(cpoToRetrafo(retenv$cpo2, prs, ctl$fun2)))
  }
}

copyCpoObjectRetrafo = function(rtf) {
  # make a 'primitive' copy with prevfun set to NULL
  copyvars = c("cpo", "info")
  oldenv = environment(rtf)

  assert((is.null(oldenv$prevfun) && "CPOPrimitive" %in% class(oldenv$cpo)) ==
         ("CPOObjectRetrafoPrimitive" %in% class(rtf)))

  newenv = new.env(parent = parent.env(oldenv))
  for (cp in copyvars) {
    newenv[[cp]] = oldenv[[cp]]
  }
  assign("prevfun", NULL, envir = newenv)
  environment(rtf) = newenv
  if ("CPOPrimitive" %in% class(newenv$cpo)) {
    class(rtf) = union("CPOObjectRetrafoPrimitive", class(rtf))
  }
  rtf
}

# RETRAFO State

#' @export
getRetrafoState.CPOObjectRetrafoPrimitive = function(retrafo.object) {
  info = environment(retrafo.object)$info
  control = info$control
  info$control = NULL

  insert(getHyperPars(retrafo.object), list(control = control, data = info))
}

#' @export
makeRetrafoFromState.CPOObjectConstructor = function(constructor, state) {
  assertList(state, names = "unique")
  assertSubset("control", names(state))
  info = insert(state$data, list(control = state$control))
  assertSetEqual(names(info), c("control", "shapeinfo.input", "shapeinfo.output"))
  state$data = NULL
  state$control = NULL
  state["id"] = list(NULL)
  bare = do.call(constructor, state)

  retr = getCPOObjectRetrafoFn(bare, info, NULL)
  addClasses(retr, c("CPOObjectRetrafoPrimitive", "CPOObjectRetrafo", "CPORetrafo"))
}

# Param Sets

#' @export
getParamSet.CPOObjectRetrafoPrimitive = function(x) {
  ps = getParamSet(environment(x)$cpo)
  parnames = environment(x)$cpo$bare.par.names
  names(ps$pars) = parnames
  for (n in parnames) {
    ps$pars[[n]]$id = n
  }
  ps
}

#' @export
getHyperPars.CPOObjectRetrafoPrimitive = function(learner, for.fun = c("train", "predict", "both")) {
  getBareHyperPars(environment(learner)$cpo)
}

getCPOProperties.CPOObjectRetrafo = function(cpo, which = NULL) {
  getCPOProperties(environment(cpo)$cpo)
}

#' @export
getCPOName.CPOObjectRetrafoPrimitive = function(cpo) {
  environment(cpo)$cpo$barename
}

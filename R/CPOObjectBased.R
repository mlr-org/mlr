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
makeCPOObject = function(.cpo.name, ..., .par.set = NULL, .par.vals = list(), cpo.trafo, cpo.retrafo) {
  # dotted parameter names are necessary to avoid problems with partial argument matching.
  cpo.name = .cpo.name
  par.set = .par.set
  par.vals = .par.vals
  assertString(cpo.name)
  assertList(par.vals, names = "unique")
  if (is.null(par.set)) {
    par.set = paramSetSugar(..., .pss.env = parent.frame())
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
# - accepts only Tasks
# - automatically subsets 'args' to the relevant ones for cpo
# - collects control from called function
# - checks the result is a df, does not check for columns

callCPOTrafo = function(cpo, data, target) {
  result = do.call(cpo$trafo, insert(getBareHyperPars(cpo), list(data = data, target = target)))
  assertTrafoResult(result, cpo$name)
  trafoenv = environment(cpo$trafo)$.ENV
  assign(".ENV", NULL, envir = environment(cpo$trafo))
  if (!"control" %in% ls(trafoenv)) {
    stopf("CPO %s cpo.trafo did not create a 'control' object.", cpo$name)
  }
  result = list(data = result, control = trafoenv$control)
  result
}

# CPO %>>% CPO

#' @export
composeCPO.CPOObject = function(cpo1, cpo2) {
  assertClass(cpo2, "CPOObject")
  parameterClashAssert(cpo1, cpo2, cpo1$name, cpo2$name)

  makeS3Obj(c("CPOObject", "CPO"),
    barename = paste(cpo2$barename, cpo1$barename, sep = "."),
    name = paste(cpo1$name, cpo2$name, sep = " >> "),
    bare.par.names = c(names(cpo1$par.set$pars), names(cpo2$par.set$pars)),
    par.set = c(cpo1$par.set, cpo2$par.set),
    par.vals = c(cpo1$par.vals, cpo2$par.vals),
    trafo = captureEnvWrapper(function(data, target, ...) {
      args = list(...)
      cpo1$par.vals = subsetParams(args, cpo1$par.set, cpo1$name)
      cpo2$par.vals = subsetParams(args, cpo2$par.set, cpo2$name)
      result = callCPOTrafo(cpo1, data, target)
      result2 = callCPOTrafo(cpo2, result$data, target)
      control = list(fun1 = result$control, fun2 = result2$control)
      result2$data
    }),
    retrafo = function(data, control, ...) {
      args = list(...)
      cpo1$par.vals = subsetParams(args, cpo1$par.set, cpo1$name)
      cpo2$par.vals = subsetParams(args, cpo2$par.set, cpo2$name)
      result = callCPORetrafo(cpo1, data, control$fun1)
      finalres = callCPORetrafo(cpo2, result, control$fun2)
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
  wlearner = makeBaseWrapper(id, learner$type, learner, learner$package,
    cpo$par.set, cpo$par.vals, c("CPOObjectLearner", "CPOLearner"), c("CPOObjectModel", "CPOModel"))
  wlearner$cpo = cpo
  wlearner
}

#' @export
trainLearner.CPOObjectLearner = function(.learner, .task, .subset = NULL, ...) {
  cpo = .learner$cpo
  cpo$par.vals = subsetParams(.learner$par.vals, cpo$par.set, cpo$name)

  transformed = callCPOTrafo(cpo, getTaskData(.task, .subset), getTaskTargetNames(.task))
  .task = changeData(.task, transformed$data)

  model = makeChainModel(train(.learner$next.learner, .task), "CPOObjectWrappedModel")
  model$control = transformed$control
  model$cpo = cpo
  model
}

#' @export
predictLearner.CPOObjectLearner = function(.learner, .model, .newdata, ...) {
  .newdata = callCPORetrafo(.model$learner.model$cpo, .newdata, .model$learner.model$control)
  NextMethod(.newdata = .newdata)
}

# DATA %>>% CPO

#' @export
applyCPO.CPOObject = function(cpo, task) {
  prevfun = retrafo(task)

  transformed = callCPOTrafo(cpo, getTaskData(task), getTaskTargetNames(task))
  task = changeData(task, transformed$data)

  retr = cpoObjectRetrafo(cpo, transformed$control, prevfun)
  is.prim = (is.null(prevfun) && "CPOPrimitive" %in% class(cpo))
  retrafo(task) = addClasses(retr, c(if (is.prim) "CPOObjectRetrafoPrimitive", "CPOObjectRetrafo", "CPORetrafo"))
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

cpoObjectRetrafo = function(cpo, control, prevfun) {
  function(data) {
    assert(checkClass(data, "data.frame"), checkClass(data, "Task"))
    if (is.data.frame(data)) {
      if (!is.null(prevfun)) {
        data = prevfun(data)
      }
      callCPORetrafo(cpo, data, control)
    } else {
      taskdata = getTaskData(data, target.extra = TRUE)$data
      if (!is.null(prevfun)) {
        taskdata = prevfun(taskdata)
      }
      newdata = getTaskData(data)
      newdata[getTaskFeatureNames(data)] = callCPORetrafo(cpo, taskdata, control)
      changeData(data, newdata)
    }
  }
}

# RETRAFO bare function
# does no conversion of data.frames, no checking
# of column equality etc.

callCPORetrafo = function(cpo, data, control) {
  result = do.call(cpo$retrafo, insert(getBareHyperPars(cpo), list(data = data, control = control)))
  assertRetrafoResult(result, cpo$name)
  result
}

# get RETRAFO from learner

singleModelRetrafo.CPOObjectModel = function(model, prevfun) {
  cpo = model$learner$cpo
  args = model$learner$par.vals
  cpo$par.vals = subsetParams(args, cpo$par.set, cpo$name)
  control = model$learner.model$control

  res = cpoObjectRetrafo(cpo, control, prevfun)

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
as.list.CPOObjectRetrafoPrimitive = function(rtf) {
  list(rtf)
}

#' @export
as.list.CPOObjectRetrafo = function(rtf) {
  if (!is.null(environment(rtf)$prevfun)) {
    # chained via prevfun
    # call as.list on both fragments again, since they might still be chained
    # with at least one other method
    c(as.list(environment(rtf)$prevfun), as.list(copyCpoObjectRetrafo(rtf)))
  } else {
    # chained via composition
    cpoToRetrafo = function(cpo, pv, control) {
      cpo$par.vals = subsetParams(pv, cpo$par.set, cpo$name)
      res = cpoObjectRetrafo(cpo, control, NULL)
      is.prim = ("CPOPrimitive" %in% class(cpo))
      addClasses(res, c(if (is.prim) "CPOObjectRetrafoPrimitive", "CPOObjectRetrafo", "CPORetrafo"))
    }
    ctl = environment(rtf)$control
    prs = getHyperPars(environment(rtf)$cpo)
    retenv = environment(environment(rtf)$cpo$retrafo)
    c(as.list(cpoToRetrafo(retenv$cpo1, prs, ctl$fun1)),
      as.list(cpoToRetrafo(retenv$cpo2, prs, ctl$fun2)))
  }
}

copyCpoObjectRetrafo = function(rtf) {
  # make a 'primitive' copy with prevfun set to NULL
  copyvars = c("cpo", "control")
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
  control = environment(retrafo.object)$control

  insert(getHyperPars(retrafo.object), list(control = control))
}

#' @export
makeRetrafoFromState.CPOObjectConstructor = function(constructor, state) {
  assertList(state, names = "unique")
  assertSubset("control", names(state))
  control = state$control
  state$control = NULL
  state["id"] = list(NULL)
  assertSetEqual(names(state), names(formals(constructor)))
  bare = do.call(constructor, state)
  retr = cpoObjectRetrafo(bare, control, NULL)
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

#' @export
getCPOName.CPOObjectRetrafoPrimitive = function(cpo) {
  environment(cpo)$cpo$barename
}

##################################
### Auxiliary Functions        ###
##################################

# check the trafo result what it is supposed to be
assertTrafoResult = function(result, name) {
  if (!is.data.frame(result)) {
    stopf("CPO %s cpo.trafo gave bad result\ncpo.trafo must return a data.frame.", name)
  }
}

# check the retrafo result is what it is supposed to be
assertRetrafoResult = function(result, name) {
  if (!is.data.frame(result)) {
    stopf("CPO %s cpo.retrafo gave bad result\ncpo.retrafo must return a data.frame.", name)
  }
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



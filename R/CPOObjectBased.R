
### Creation

#' @export
makeCPOObject = function(.cpo.name, ..., .par.set = NULL, .par.vals = list(), cpo.trafo, cpo.retrafo) {
  # dotted parameter names are necessary to avoid problems with partial argument matching.
  cpo.name = .cpo.name
  par.set = .par.set
  par.vals = .par.vals
  assertString(cpo.name)
  assertList(par.vals, names = "unique")
  if (is.null(par.set)) {
    par.set = paramSetSugar(..., pss.env = parent.frame())
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
    cpo = makeS3Obj(base::c("CPOObject", "CPOPrimitive", "CPO"),
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

### Compose, Attach

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
      result = callCPOTrafo(cpo1, args, data, target)
      result2 = callCPOTrafo(cpo2, args, result$data, target)
      control = list(fun1 = result$control, fun2 = result2$control)
      result2$data
    }),
    retrafo = function(data, control, ...) {
      args = list(...)
      result = callCPORetrafo(cpo1, args, data, control$fun1)
      finalres = callCPORetrafo(cpo2, args, result, control$fun2)
      finalres
    })
}

#' @export
attachCPO.CPOObject = function(cpo, learner) {
  learner = checkLearner(learner)
  id = paste(learner$id, cpo$barename, sep = ".")
  # makeBaseWrapper checks for parameter name clash, but gives
  # less informative error message
  parameterClashAssert(cpo, learner, cpo$name, learner$name)
  wlearner = makeBaseWrapper(id, learner$type, learner, learner$package,
    cpo$par.set, cpo$par.vals, "CPOObjectLearner", "CPOObjectModel")
  wlearner$cpo = cpo
  wlearner
}

#' @export
trainLearner.CPOObjectLearner = function(.learner, .task, .subset = NULL, ...) {
  cpo = .learner$cpo
  args = .learner$par.vals
  transformed = callCPOTrafo(cpo, args, getTaskData(.task, .subset), getTaskTargetNames(.task))
  .task = changeData(.task, transformed$data)
  model = makeChainModel(train(.learner$next.learner, .task), "CPOObjectModel")
  model$control = transformed$control
  model
}

#' @export
predictLearner.CPOObjectLearner = function(.learner, .model, .newdata, ...) {
  cpo = .learner$cpo
  args = .learner$par.vals
  .newdata = callCPORetrafo(cpo, args, .newdata, .model$learner.model$control)
  NextMethod(.newdata = .newdata)
}

### IDs, ParamSets

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
    stopf("CPO %s does not have parameter%s %s", learner$name,
          ifelse(length(badpars) > 1, "s", ""), collapse(badpars, ", "))
  }
  checkParamsFeasible(learner$par.set, par.vals)
  learner$par.vals = insert(learner$par.vals, par.vals)
  learner
}

#' @export
removeHyperPars.CPOObjectLearner = function(learner, ids) {
  i = intersect(names(learner$par.vals), ids)
  if (length(i) > 0) {
    stopf("CPO Parameters (%s) can not be removed", collapse(i, sep = ", "))
  }
  learner$next.learner = removeHyperPars(learner$next.learner, ids)
  learner
}

#' @export
getCPOName.CPOObject = function(cpo) {
  cpo$name
}

### Auxiliaries

captureEnvWrapper = function(fun) {
  envcapture = quote({ assign(".ENV", tail(sys.frames(), 1)[[1]], envir = environment(sys.function())) ; 0 })
  envcapture[[3]] = body(fun)
  body(fun) = envcapture
  environment(fun) = new.env(parent = environment(fun))
  fun
}

assertTrafoResult = function(result, name) {
  if (!is.data.frame(result)) {
    stopf("CPO %s cpo.trafo gave bad result\ncpo.trafo must return a data.frame.", name)
  }
}

assertRetrafoResult = function(result, name) {
  if (!is.data.frame(result)) {
    stopf("CPO %s cpo.retrafo gave bad result\ncpo.retrafo must return a data.frame.", name)
  }
}

# filter args for the arguments relevant for cpo
# then add whatever is in '...'.
subsetCPOArgs = function(cpo, args, ...) {
  args = subsetParams(args, cpo$par.set, cpo$name)
  namestranslation = cpo$bare.par.names
  names(namestranslation) = names(cpo$par.set$pars)
  names(args) = namestranslation[names(args)]
  insert(args, list(...))
}

callCPOTrafo = function(cpo, args, data, target) {
  result = do.call(cpo$trafo, subsetCPOArgs(cpo, args, data = data, target = target))
  assertTrafoResult(result, cpo$name)
  trafoenv = environment(cpo$trafo)$.ENV
  assign(".ENV", NULL, envir = environment(cpo$trafo))
  if (!"control" %in% ls(trafoenv)) {
    stopf("CPO %s did not create a 'control' object.", cpo$name)
  }
  result = list(data = result, control = trafoenv$control)
  result
}

callCPORetrafo = function(cpo, args, data, control) {
  result = do.call(cpo$retrafo, subsetCPOArgs(cpo, args, data = data, control = control))
  assertRetrafoResult(result, cpo$name)
  result
}

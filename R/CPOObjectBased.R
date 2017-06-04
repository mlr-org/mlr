
#' @export
makeCPOObject = function(name, ..., par.set = NULL, par.vals = NULL, cpo.trafo, cpo.retrafo) {
  assertString(name)
  if (is.null(par.set)) {
    par.set = paramSetSugar(..., pss.env = parent.frame())
  }
  reservedParams = c("data", "target", "control", "id")
  if (any(names(par.set$pars) %in% reservedParams)) {
    stopf("Parameters %s are reserved", collapse(reservedParams, ", "))
  }
  if (is.null(par.vals)) {
    par.vals = getParamSetDefaults(par.set)
  }
  assertSubset(names(par.vals), names(par.set$pars))

  funargs = lapply(par.set$pars, function(dummy) substitute())
  funargs = insert(funargs, par.vals)

  required.arglist.trafo = funargs
  required.arglist.trafo$data = substitute()
  required.arglist.trafo$target = substitute()
  cpo.trafo = makeFunction(substitute(cpo.trafo), required.arglist.trafo, env = parent.frame())

  required.arglist.retrafo = funargs
  required.arglist.retrafo$data = substitute()
  required.arglist.retrafo$control = substitute()
  cpo.retrafo = makeFunction(substitute(cpo.retrafo), required.arglist.retrafo, env = parent.frame())

  funargs = insert(funargs, list(id = NULL))

  funbody = quote({
    args = match.call()
    args[[1]] = quote(list)
    args = eval(args, envir = parent.frame())
    args = insert(funargs, args)
    if (!is.null(args$id)) {
      assertString(args$id)
    }
    present.pars = Filter(function(x) !identical(x, substitute()), args[names(par.set$pars)])
    cpo = makeS3Obj(c("CPOObject", "CPOPrimitive", "CPO"),
      barename = name,
      name = name,
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

assertTrafoResult = function(result, name) {
  if (!is.list(result) || length(result) != 2 || length(intersect(names(result), c("data", "control"))) != 2 ||
      !is.data.frame(result$data)) {
    stopf("CPO %s cpo.trafo gave bad result\ncpo.trafo must return list(data=[data.frame], control= ).", name)
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
  args = args[names(cpo$par.set$pars)]
  names(args) = cpo$bare.par.names
  insert(args, list(...))
}

callCPOTrafo = function(cpo, args, data, target) {
  result = do.call(cpo$trafo, subsetCPOArgs(cpo, args, data = data, target = target))
  assertTrafoResult(result, cpo$name)
  result
}

callCPORetrafo = function(cpo, args, data, control) {
  result = do.call(cpo$retrafo, subsetCPOArgs(cpo, args, data = data, control = control))
  assertRetrafoResult(result, cpo$name)
  result
}

#' @export
composeCPO.CPOObject = function(cpo1, cpo2) {
  assertClass(cpo2, "CPOObject")
  parameterClashAssert(cpo1$par.set, cpo2$par.set, cpo1$name, cpo2$name)

  makeS3Obj(c("CPOObject", "CPO"),
    barename = paste(cpo2$barename, cpo1$barename, sep="."),
    name = paste(cpo1$name, cpo2$name, sep=" >> "),
    bare.par.names = c(names(cpo1$par.set$pars), names(cpo2$par.set$pars)),
    par.set = c(cpo1$par.set, cpo2$par.set),
    par.vals = c(cpo1$par.vals, cpo2$par.vals),
    trafo = function(data, target, ...) {
      args = list(...)
      result = callCPOTrafo(cpo1, args, data, target)
      result2 = callCPOTrafo(cpo2, args, result$data, target)
      list(data = result2$data, control = list(fun1 = result$control, fun2 = result2$control))
    },
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
  id = paste(learner$id, cpo$barename, sep=".")
  # makeBaseWrapper checks for parameter name clash, but gives
  # less informative error message
  parameterClashAssert(cpo$par.set, getParamSet(learner), cpo$name, learner$name)
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

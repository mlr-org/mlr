

#' @export
makeCPOFunctional = function(name, ..., par.set = NULL, par.vals = NULL, cpo.trafo) {
  assertString(name)
  if (is.null(par.set)) {
    par.set = paramSetSugar(..., pss.env = parent.frame())
  }
  reservedParams = c("data", "target", "id")
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

    outerTrafo = function(task, .par.vals) {

      assertClass(task, "Task")
      args = .par.vals[names(par.set$pars)]
      args$data = getTaskData(.task, .subset)
      args$target = getTaskTargetNames(.task)
      result = do.call(cpo.trafo, args)
      if (!is.data.frame(result) || is.null(attr(result, "retrafo"))) {
        stopf("CPO %s cpo.trafo gave bad result\ncpo.trafo must return a data.frame with attribute 'retrafo' (a function).", name)
      }
      retrafo = attr(result, "retrafo")
      attr(result, "retrafo") = NULL
      assertFunction(retrafo, "data", nargs = 1)

      upper.retrafo = attr(task, "retrafo")
      if (!is.null(upper.retrafo)) {
        assertFunction(upper.retrafo, "data", nargs = 1)
        lower.retrafo = retrafo
        retrafo = function(data) {
          lower.retrafo(upper.retrafo(data))
        }
      }
      task = changeData(task, result)
      attr(task, "retrafo") = function(data) {
        result = retrafo(data)
        if (!is.data.frame(result)) {
          stopf("CPO %s retrafo gave bad result\nretrafo must return a data.frame.", name)
        }
      }
    }
    attr(outerTrafo, "name") = name
    attr(outerTrafo, "barename") = name
    attr(outerTrafo, "id") = NULL
    formals(outerTrafo) = as.pairlist(task = substitute(), .par.vals = par.vals)
    outerTrafo = addClasses(outerTrafo, c("CPOFunctional", "CPOFunctionalPrimitive", "CPO"))
    setCPOId(outerTrafo, args$id)
  })
  addClasses(eval(call("function", as.pairlist(funargs), funbody)), c("CPOFunctionalConstructor", "CPOConstructor"))
}

#' @export
getParamSet.CPOFunctional = function(x) {
  ps = environment(x)$par.set
  id = attr(x, "id")
  if (!is.null(id)) {
    names(ps$pars) = paste(id, names(ps$pars), sep = ".")
    ps$pars = lapply(ps$pars, function(x) {
      x$id = paste(id, names(ps$pars), sep = ".")
      x
    })
  }
  ps
}

#' @export
getHyperPars.CPOFunctional = function(learner, for.fun = c("train", "predict", "both")) {
  id = attr(learner, "id")
  pv = formals(learner)$.par.vals
  if (!is.null(id)) {
    names(pv) = paste(id, names(pv), sep = ".")
  }
  pv
}

#' @export
setHyperPars2.CPOFunctional = function(learner, par.vals = list()) {
  ps = getParamSet(learner)
  badpars = setdiff(names(par.vals), names(ps))
  if (length(badpars)) {
    stopf("CPO %s does not have parameter%s %s", learner$name,
          ifelse(length(badpars) > 1, "s", ""), coalesce(badpars, ", "))
  }
  pv = getHyperPars(learner)
  pv = insert(pv, par.vals)
  if (!is.null(id)) {
    names(pv) = stri_sub(names(pv), nchar(id) + 2)
  }
  formals(learner) = as.pairlist(task = substitute(), .par.vals = pv)
  learner
}

#' @export
compose.CPOFunctional = function(cpo1, cpo2) {
  # in theory we could just do function composition, but then we
  # would lose the ability to setHyperPars().
  assertClass(cpo2, "CPOFunctional")
  parameterClashAssert(cpo1, cpo2, attr(cpo1, "name"), attr(cpo2, "name"))
  outerTrafo = function(task, .par.vals) {
    pv1names = names(getParamSet(cpo1)$pars)
    pv2names = names(getParamSet(cpo2)$pars)
    assert(length(intersect(pv1names, pv2names)) == 0)
    assert(length(setdiff(names(.par.vals), c(pv1names, pv2names))) == 0)
    setHyperPars(cpo2, .par.vals[pv2names])(setHyperPars(cpo1, .par.vals[pv1names])(task))
  }
  attr(outerTrafo, "name") = paste(attr(cpo1, "name"), attr(cpo2, "name"), sep=" >> ")
  attr(outerTrafo, "barename") = paste(attr(cpo2, "barename"), attr(cpo1, "barename"), sep=".")
  addClasses(outerTrafo, c("CPOFunctional", "CPO"))
}

#' @export
setCPOId.CPOFunctional = function(cpo, id) {
  if (!is.null(id)) {
    assertString(id)
  }
  if (!"CPOFunctionalPrimitive" %in% class(cpo)) {
    stop("Cannot set ID of compound CPO.")
  }
  attr(cpo, "id") = id
  attr(cpo, "name") = collapse(c(attr(cpo, "barename"), id), sep=".")
  cpo
}


#' @export
attachCPO.CPOFunctional = function(cpo, learner) {
  learner = checkLearner(learner)
  id = paste(learner$id, cpo$barename, sep=".")
  # makeBaseWrapper checks for parameter name clash, but gives
  # less informative error message
  parameterClashAssert(cpo, learner, attr(cpo, "name"), learner$name)
  wlearner = makeBaseWrapper(id, learner$type, learner, learner$package,
    cpo$par.set, cpo$par.vals, "CPOFunctionalLearner", "CPOFunctionalModel")
  wlearner$cpo = cpo
  wlearner
}

#' @export
trainLearner.CPOFunctionalLearner = function(.learner, .task, .subset = NULL, ...) {
  args = .learner$par.vals
  cpo = setHyperPars(.learner$cpo, par.vals = args)
  .task = cpo(.task)
  retrafo = attr(cpo, "retrafo")
  attr(cpo, "retrafo") = NULL
  model = makeChainModel(train(.learner$next.learner, .task), "CPOObjectModel")
  model$retrafo = retrafo
  model
}

#' @export
predictLearner.CPOFunctionalLearner = function(.learner, .model, .newdata, ...) {
  .newdata = .model$learner.model$retrafo(.newdata)
  NextMethod(.newdata = .newdata)
}

#' @export
removeHyperPars.CPOFunctionalLearner = function(learner, ids) {
  i = intersect(names(learner$par.vals), ids)
  if (length(i) > 0) {
    stopf("CPO Parameters (%s) can not be removed", collapse(i, sep=", "))
  }
  learner$next.learner = removeHyperPars(learner$next.learner, ids)
  learner
}

#' @export
getCPOName.CPOFunctional = function(cpo) {
  attr(cpo, "name")
}

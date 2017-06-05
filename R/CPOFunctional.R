
### Creation

#' @export
makeCPOFunctional = function(.cpo.name, ..., .par.set = NULL, .par.vals = list(), cpo.trafo) {
  # dotted parameter names are necessary to avoid problems with partial argument matching.
  cpo.name = .cpo.name
  par.set = .par.set
  par.vals = .par.vals
  assertList(par.vals, names = "unique")
  assertString(cpo.name)
  if (is.null(par.set)) {
    par.set = paramSetSugar(..., pss.env = parent.frame())
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
    par.set = par.set  # get par.set into current env
    outerTrafo = function(task, .par.vals) {
      assertClass(task, "Task")

      args = subsetParams(.par.vals, par.set, cpo.name)

      args$data = getTaskData(task)
      args$target = getTaskTargetNames(task)

      result = do.call(cpo.trafo, args)

      if (!is.data.frame(result) || is.null(attr(result, "retrafo"))) {
        stopf("CPO %s cpo.trafo gave bad result\ncpo.trafo must return a data.frame with attribute 'retrafo' (a function).", cpo.name)
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
          stopf("CPO %s retrafo gave bad result\nretrafo must return a data.frame.", cpo.name)
        }
        result
      }

      task
    }
    # can't do the following in function head, since par.vals must be eval'd
    formals(outerTrafo) = as.pairlist(list(task = substitute(), .par.vals = present.pars))
    attr(outerTrafo, "name") = cpo.name
    attr(outerTrafo, "barename") = cpo.name
    attr(outerTrafo, "id") = NULL
    outerTrafo = addClasses(outerTrafo, c("CPOFunctional", "CPOFunctionalPrimitive", "CPO"))  # nolint
    setCPOId(outerTrafo, args$id)
  })
  addClasses(eval(call("function", as.pairlist(funargs), funbody)), c("CPOFunctionalConstructor", "CPOConstructor"))
}

### Compose, Attach

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

#' @export
attachCPO.CPOFunctional = function(cpo, learner) {
  learner = checkLearner(learner)
  id = paste(learner$id, attr(cpo, "barename"), sep = ".")
  # makeBaseWrapper checks for parameter name clash, but gives
  # less informative error message
  parameterClashAssert(cpo, learner, attr(cpo, "name"), learner$name)
  wlearner = makeBaseWrapper(id, learner$type, learner, learner$package,
    getParamSet(cpo), getHyperPars(cpo), "CPOFunctionalLearner", "CPOFunctionalModel")
  wlearner$cpo = cpo
  wlearner
}

#' @export
trainLearner.CPOFunctionalLearner = function(.learner, .task, .subset = NULL, ...) {
  args = .learner$par.vals
  cpo = setHyperPars(.learner$cpo, par.vals = args)
  .task = cpo(subsetTask(.task, .subset))
  retrafo = attr(.task, "retrafo")
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

### IDs, ParamSets

setCPOId.CPOFunctional = function(cpo, id) {
  if (!is.null(id)) {
    assertString(id)
  }
  if (!"CPOFunctionalPrimitive" %in% class(cpo)) {
    stop("Cannot set ID of compound CPO.")
  }
  attr(cpo, "id") = id
  attr(cpo, "name") = collapse(c(attr(cpo, "barename"), id), sep = ".")
  cpo
}

#' @export
getParamSet.CPOFunctional = function(x) {
  ps = environment(x)$par.set
  id = attr(x, "id")
  if (!is.null(id)) {
    if (length(ps$pars)) {
      names(ps$pars) = paste(id, names(ps$pars), sep = ".")
    }
    ps$pars = lapply(ps$pars, function(x) {
      x$id = paste(id, x$id, sep = ".")
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

#' @export
removeHyperPars.CPOFunctionalLearner = function(learner, ids) {
  i = intersect(names(learner$par.vals), ids)
  if (length(i) > 0) {
    stopf("CPO Parameters (%s) can not be removed", collapse(i, sep = ", "))
  }
  learner$next.learner = removeHyperPars(learner$next.learner, ids)
  learner
}

#' @export
getCPOName.CPOFunctional = function(cpo) {
  attr(cpo, "name")
}


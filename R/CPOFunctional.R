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
makeCPOFunctional = function(.cpo.name, ..., .par.set = NULL, .par.vals = list(), cpo.trafo) {
  # dotted parameter names are necessary to avoid problems with partial argument matching.
  cpo.name = .cpo.name
  par.set = .par.set
  par.vals = .par.vals
  assertList(par.vals, names = "unique")
  assertString(cpo.name)
  if (is.null(par.set)) {
    par.set = paramSetSugar(..., .pss.env = parent.frame())
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
    par.set = par.set  # get par.set into current env
    outerTrafo = function(task, .par.vals) {
      assertClass(task, "Task")
      upper.retrafo = retrafo(task)
      is.prim = is.null(upper.retrafo)
      args = subsetParams(.par.vals, par.set, cpo.name)

      # prepare task
      args$data = getTaskData(task)
      args$target = getTaskTargetNames(task)

      result = do.call(cpo.trafo, args)

      trafoenv = environment(cpo.trafo)$.ENV
      assign(".ENV", NULL, envir = environment(cpo.trafo))

      # check result & retrafo
      if (!is.data.frame(result)) {
        stopf("CPO %s cpo.trafo gave bad result\ncpo.trafo must return a data.frame.", cpo.name)
      }
      retrafo.fn = trafoenv$cpo.retrafo

      task = changeData(task, result)

      if (is.null(retrafo.fn) || !isTRUE(checkFunction(retrafo.fn, nargs = 1))) {
        stopf("CPO %s cpo.trafo did not set a variable 'cpo.retrafo' to a function with one argument.", cpo.name)
      }

      # chain retrafo
      lower.retrafo = retrafo.fn  # lower.retrafo must be present in any case, for later chaining of retrafos
      if (!is.prim) {
        assertFunction(upper.retrafo, nargs = 1, .var.name = "retrafo(task)")
        retrafo.fn = function(data) {
          lower.retrafo(upper.retrafo(data))
        }
      }


      cpo.name = cpo.name  # get cpo.name into the 'retrafo' env for easier manipulation later
      par.set = par.set  # same

      retrafo(task) = function(data) {
        was.task = "Task" %in% class(data)
        task = data
        if (was.task) {
          data = getTaskData(data, target.extra = TRUE)$data
        }
        result = retrafo.fn(data)
        if (!is.data.frame(result)) {
          stopf("CPO %s retrafo gave bad result\nretrafo must return a data.frame.", cpo.name)
        }
        if (was.task) {
          newdata = getTaskData(task)
          newdata[getTaskFeatureNames(task)] = result
          result = changeData(task, newdata)
        }
        result
      }
      retrafo(task) = addClasses(retrafo(task), c(if (is.prim) "CPOFunctionalRetrafoPrimitive", "CPOFunctionalRetrafo", "CPORetrafo"))

      task
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
  wlearner = makeBaseWrapper(id, learner$type, learner, learner$package,
    getParamSet(cpo), getHyperPars(cpo), c("CPOFunctionalLearner", "CPOLearner"), c("CPOFunctionalModel", "CPOModel"))
  wlearner$cpo = cpo
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

# CPO splitting

#' @export
as.list.CPOFunctional = function(x, ...) {
  assert(length(list(...)) == 0)
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
`%>>%.CPOFunctionalRetrafo` = function(cpo1, cpo2) {
  assertClass(cpo2, "CPOFunctionalRetrafo")
  oldenv = environment(cpo2)
  assert(is.null(oldenv$upper.retrafo) == ("CPOFunctionalRetrafoPrimitive" %in% class(cpo2)))

  cpo2 = copyFunctionalRetrafo(cpo2)

  if (!is.null(oldenv$upper.retrafo)) {
    cpo1 = cpo1 %>>% oldenv$upper.retrafo
  }
  class(cpo2) = setdiff(class(cpo2), "CPOFunctionalRetrafoPrimitive")

  environment(cpo2)$upper.retrafo = cpo1
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
as.list.CPOFunctionalRetrafo = function(rtf) {
  oldenv = environment(rtf)
  rtf = copyFunctionalRetrafo(rtf)
  if (!is.null(oldenv$upper.retrafo)) {
    assertClass(oldenv$upper.retrafo, "CPORetrafo")
  }
  c(as.list(oldenv$upper.retrafo), list(rtf))
}

copyFunctionalRetrafo = function(rtf) {
  # used vars:
  #  retrafo.fn: the retrafo being used.
  #  lower.retrafo: original retrafo
  #  cpo.name, args, par.set
  copyvars = c("lower.retrafo", "args", "cpo.name", "par.set")
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

# TODO



### Param Sets

#' @export
getParamSet.CPOFunctionalRetrafo = function(x) {
  stop("Cannot get param set of compound retrafo. Use as.list to get individual elements")
}


#' @export
getHyperPars.CPOFunctionalRetrafo = function(learner, for.fun = c("train", "predict", "both")) {
  stop("Cannot get parameters of compound retrafo. Use as.list to get individual elements")
}

#' @export
getParamSet.CPOFunctionalRetrafoPrimitive = function(x) {
  environment(x)$par.set
}

#' @export
getHyperPars.CPOFunctionalRetrafoPrimitive = function(learner, for.fun = c("train", "predict", "both")) {
  pv = environment(learner)$args
  pv$data = NULL
  pv$target = NULL
  pv
}

#' @export
getCPOName.CPOFunctionalRetrafoPrimitive = function(cpo) {
  environment(cpo)$cpo.name
}

#' @export
getCPOName.CPOFunctionalRetrafo = function(cpo) {
  paste(sapply(as.list(cpo), getCPOName), collapse = " => ")
}


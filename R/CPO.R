#' @include CPOFormatCheck.R
##################################
### Creator                    ###
##################################

#' @title Create a custom CPO constructor
#'
#' @description
#' Create a CPO constructor.
#'
#' @param .cpo.name [\code{character(1)}]\cr
#'   The name of the resulting CPO constructor / CPO. This is used for identification in output.
#' @param ...
#'   Parameters of the CPO, in the format of \code{\link{paramSetSugar}}.
#' @param .par.set [\code{ParamSet}]\cr
#'   Optional parameter set. If this is not \code{NULL}, the \dQuote{...} parameters are ignored.
#'   Default is \code{NULL}.
#' @param .par.vals [\code{list}]\cr
#'   Named list of default parameter values for the CPO. These are used additionally to the
#'   parameter default values in \dQuote{...} and \code{.par.set}. It is preferred to use
#'   these default values, and not \code{.par.vals}. Default is \code{list()}.
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
#'     \item factor similar to \dQuote{target}, but \dQuote{data} will only contain the features
#'       whtat are either of type \dQuote{factor} or \dQuote{ordered}.
#'     \item onlyfactor similar to \dQuote{target} but \dQuote{data} will only contain the features
#'       whtat are of type \dQuote{factor}.
#'     \item ordered similar to \dQuote{target} but \dQuote{data} will only contain the features
#'       whtat are of type \dQuote{ordered}.
#'     \item numeric similar to \dQuote{target} but \dQuote{data} will only contain the features
#'       whtat are of type \dQuote{numeric}.
#'   }
#'   The returned data must always be in the same format as the one requested and it is an error to
#'   change the target column(s) in the \dQuote{no} and \dQuote{task}. If \dQuote{.datasplit} is
#'   \dQuote{most} or \dQuote{all}, the \dQuote{$numeric} slot of the returned object may also be a
#'   \code{matrix}. If \dQuote{.datasplit} is \dQuote{numeric}, the returned object may also be a
#'   matrix.
#'   Default is \dQuote{target}.
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
#'   the function may have a dotdotdot argument). It must return a \dQuote{data.frame}, a \dQuote{task},
#'   a dQuote{matrix}, or a \dQuote{list} of \dQuote{data.frame} and \dQuote{matrix} objects, depending
#'   on the parameter \dQuote{.datasplit}. If \dQuote{cpo.retrafo} is given, it must create a \dQuote{control}
#'   variable in its namespace, which will be passed on to \dQuote{cpo.retrafo}. If \dQuote{cpo.retrafo} is
#'   not given, it must create a \dQuote{cpo.retrafo} function within its namespace, which will be called
#'   for re-transformation. This function must have a \dQuote{data} and \dQuote{target} argument.\cr
#'   If \dQuote{cpo.trafo} is a list of expressions (preferred), it is turned into a function
#'   by mlr, with the above mentioned criteria.
#' @param cpo.retrafo [\code{language} | \code{function}]\cr
#'   Similarly to \dQuote{cpo.trafo}, this is either a function or a sequence of expressions
#'   in curly braces (preferred), or \code{NULL}. This function must have the same arguments, except that
#'   instead of a \dQuote{target} argument, it has a \dQuote{control} argument, which will be
#'   the value created in the \dQuote{cpo.trafo} run. It gets its input data in the same format as
#'   \dQuote{cpo.trafo}, with the exception that if \dQuote{.datasplit} is \dQuote{task}, it gets a
#'   \dQuote{data.frame} as if \dQuote{.datasplit} were \dQuote{no}. This function must similarly return an
#'   object in the same format as it received as input.
#' @family CPO
#'
#' @export
makeCPO = function(.cpo.name, ..., .par.set = NULL, .par.vals = list(),
                   .datasplit = c("target", "most", "all", "no", "task", "factor", "onlyfactor", "ordered", "numeric"),
                   .fix.factors = FALSE,
                   .stateless = FALSE,
                   .properties = c("numerics", "factors", "ordered", "missings"),
                   .properties.adding = character(0), .properties.needed = character(0),
                   .properties.target = c("cluster", "classif", "multilabel", "regr", "surv",
                     "oneclass", "twoclass", "multiclass", "lcens", "rcens", "icens"),
                   .packages = character(0),
                    cpo.trafo, cpo.retrafo) {
  # dotted parameter names are necessary to avoid problems with partial argument matching.
  # The reason cpo.trafo and cpo.retrafo are not dotted is that they always need to be given.
  # If that changes, they would also neede to be dotted.

  .datasplit = match.arg(.datasplit)

  assertSubset(.properties, cpo.dataproperties)
  assertSubset(.properties.target, c(cpo.tasktypes, cpo.targetproperties))
  assertSubset(.properties.needed, cpo.dataproperties)

  eval.parent(substitute(makeCPOGeneral(.cpotype = "databound",
    .cpo.name = .cpo.name, .par.set = .par.set, .par.vals = .par.vals,
    .datasplit = .datasplit, .fix.factors = .fix.factors, .data.dependent = TRUE, .stateless = .stateless, .properties = .properties,
    .properties.adding = .properties.adding, .properties.needed = .properties.needed,
    .properties.target = .properties.target, .type.from = NULL, .type.to = NULL,
    .predict.type = NULL, cpo.trafo = cpo.trafo, cpo.retrafo = cpo.retrafo, ...)))
}

#' @export
makeCPOGeneral = function(.cpotype = c("databound", "targetbound"), .cpo.name, .par.set, .par.vals,
                          .datasplit, .fix.factors, .data.dependent, .stateless, .properties, .properties.adding, .properties.needed,
                          .properties.target, .type.from, .type.to, .predict.type, .packages, cpo.trafo, cpo.retrafo, ...) {
  .cpotype = match.arg(.cpotype)
  assertFlag(.data.dependent)
  assertString(.cpo.name)
  assertList(.par.vals, names = "unique")
  assertFlag(.stateless)

  if (is.null(.par.set)) {
    .par.set = paramSetSugar(..., .pss.env = parent.frame())
  }

  assertCharacter(.properties, unique = TRUE)
  assertCharacter(.properties.needed, unique = TRUE)
  assertCharacter(.properties.adding, unique = TRUE)
  assertCharacter(.properties.target, unique = TRUE)
  if (is.null(.predict.type)) {
    # for databound CPOs, this is the identity.
    .predict.type = c(response = "response", prob = "prob", se = "se")
    .properties = c(.properties, "prob", "se")
  }
  assertCharacter(.predict.type, any.missing = FALSE, names = "unique")

  if (.cpotype == "targetbound") {
    assertChoice(.type.from, cpo.tasktypes)
    assertChoice(.type.to, cpo.tasktypes)
    if (.type.from != .type.to) {
      .properties.adding = union(.properties.adding, .type.from)
      .properties.needed = union(.properties.needed, .type.to)
    }
    .properties = union(.properties, .type.from)
  }

  if (length(.properties)) {
    assertSubset(.properties.adding, .properties)
  } else {
    assert(length(.properties.adding) == 0)
  }
  .properties = union(.properties, .properties.target)

  badprops = intersect(.properties.adding, .properties.needed)
  if (length(badprops)) {
    stopf(".properties.adding and .properties.needed must not contain the same properties, but both contained %s.",
      collapse(badprops, sep = ", "))
  }

  # these parameters are either special parameters given to the constructor function (id, affect.*),
  # special parameters given to the cpo.trafo function (data, target), special parameters given to the
  # cpo.retrafo function (predict.type, control),

  affect.params = c("affect.type", "affect.index", "affect.names", "affect.pattern", "affect.invert", "affect.pattern.ignore.case",
    "affect.pattern.perl", "affect.pattern.fixed")
  reserved.params = c("data", "target", "predict.type", "control", "id", affect.params)
  if (any(names(.par.set$pars) %in% reserved.params)) {
    stopf("Parameters %s are reserved", collapse(reserved.params, ", "))
  }

  .par.vals = insert(getParamSetDefaults(.par.set), .par.vals)

  assert(length(setdiff(names(.par.vals), names(.par.set$pars))) == 0)

  .par.vals = convertItemsToNamesDVP(.par.vals, .par.set)

  checkParamsFeasible(.par.set, .par.vals)

  funargs = lapply(.par.set$pars, function(dummy) substitute())
  funargs = insert(funargs, .par.vals)

  required.arglist.trafo = funargs
  if (.data.dependent) {
    required.arglist.trafo$data = substitute()
  }
  required.arglist.trafo$target = substitute()
  trafo.expr = substitute(cpo.trafo)
  if (!.stateless || (is.recursive(trafo.expr) && identical(trafo.expr[[1]], quote(`{`))) || !is.null(cpo.trafo)) {
    cpo.trafo = makeFunction(trafo.expr, required.arglist.trafo, env = parent.frame())
  } else if (.cpotype == "targetbound") {
    stop("A target-bound CPO must have a cpo.trafo function, even if stateless.")
  } else if (.datasplit == "task") {
    stop("A stateless CPO without cpo.trafo cannot have .datasplit 'task'.")
  } else {
    if (.datasplit == "no") {
      .datasplit = "target"
    }
    cpo.trafo = NULL
  }
  cpo.trafo = captureEnvWrapper(cpo.trafo)

  retrafo.expr = substitute(cpo.retrafo)
  if ((is.recursive(retrafo.expr) && identical(retrafo.expr[[1]], quote(`{`))) || !is.null(cpo.retrafo)) {
    required.arglist.retrafo = funargs
    if (.cpotype == "targetbound") {
      required.arglist.retrafo$target = substitute()
      required.arglist.retrafo$predict.type = substitute()
    } else if (.data.dependent) {
      required.arglist.retrafo$data = substitute()
    }
    if (!.stateless) {
      required.arglist.retrafo$control = substitute()
    }
    cpo.retrafo = makeFunction(retrafo.expr, required.arglist.retrafo, env = parent.frame())
  } else if (.stateless) {
    stop("Cannot have a functional stateless CPO.")
  }

  funargs = insert(funargs, list(id = NULL, affect.type = NULL, affect.index = integer(0),
    affect.names = character(0), affect.pattern = NULL, affect.invert = FALSE,
    affect.pattern.ignore.case = FALSE, affect.pattern.perl = FALSE, affect.pattern.fixed = FALSE))

  funbody = quote({
    # in the first two code lines, there are still arguments around that we don't know the names of.
    # therefore, we need to catch them into the 'args' list and delete them, always referencing the
    # functions we mean by '::'.
    args = base::match.call()
    base::rm(list = base::setdiff(base::ls(), "args"))  # delete all arguments to avoid name clashes
    args[[1]] = quote(list)
    args = eval(args, envir = parent.frame())
    args = insert(funargs, args)
    id = args$id
    args$id = NULL
    if (!is.null(id)) {
      assertString(id)
    }
    affect.args = args[affect.params]
    names(affect.args) = substring(names(affect.args), 8)
    if (!is.null(affect.args$type)) {
      assertSubset(affect.args$type, c("numeric", "factor", "ordered", "other"))
    }
    assertIntegerish(affect.args$index, any.missing = FALSE, unique = TRUE)
    assertCharacter(affect.args$names, any.missing = FALSE, unique = TRUE)
    if (!is.null(affect.args$pattern)) {
      assertString(affect.args$pattern)
    }
    assertFlag(affect.args$invert)
    assertFlag(affect.args$pattern.ignore.case)
    assertFlag(affect.args$pattern.perl)
    assertFlag(affect.args$pattern.fixed)
    args = dropNamed(args, affect.params)

    present.pars = Filter(function(x) !identical(x, substitute()), args[names(.par.set$pars)])
    checkParamsFeasible(.par.set, present.pars)
    cpo = makeS3Obj(c("CPOS3Primitive", "CPOPrimitive", "CPOS3", "CPO"),
      # --- CPOS3 part
      bare.name = .cpo.name,
      name = .cpo.name,
      par.set = .par.set,
      par.vals = present.pars,
      properties = list(properties = .properties,
        properties.data = .properties,
        properties.adding = .properties.adding,
        properties.needed = .properties.needed),
      bound = .cpotype,
      predict.type = .predict.type,
      # --- CPOS3Primitive part
      id = NULL,
      packages = .packages,
      affect.args = affect.args,
      bare.par.set = .par.set,
      datasplit = .datasplit,
      stateless = .stateless,
      fix.factors = .fix.factors,
      type = ifelse(is.null(cpo.retrafo), "functional", "object"),
      trafo = cpo.trafo,
      retrafo = cpo.retrafo,
      convertfrom = .type.from,
      convertto = .type.to,
      data.dependent = .data.dependent,
      hybrid.inverter = .cpotype == "targetbound" && .stateless)
    if (length(getCPOAffect(cpo))) {
      # data is subset, so the overall 'properties' is the maximal set
      cpo$properties$properties = union(cpo$properties$properties,  c("numerics", "factors", "ordered", "missings"))
    }
    setCPOId(cpo, id)  # this also adjusts par.set and par.vals
  })
  addClasses(eval(call("function", as.pairlist(funargs), funbody)), c("CPOS3Constructor", "CPOConstructor"))
}

makeCPOS3Inverter = function(cpo, state, prev.inverter, data, shapeinfo) {
  if (!"Task" %in% class(data)) {
    data = makeClusterTask("CPO Generated", data, check.data = FALSE)
  }

  inverter = makeCPOS3RetrafoBasic(cpo, state, prev.inverter, "inverter")
  # --- only in pure "inverter" kind
  inverter$indatatd = getTaskDesc(data)
  inverter$truth = prepareRetrafoData(data, cpo$datasplit, cpo$properties$properties, shapeinfo, cpo$bare.name)$target
  inverter
}

makeCPOS3Retrafo = function(cpo, state, prev.retrafo, shapeinfo.input, shapeinfo.output) {
  retrafo = makeCPOS3RetrafoBasic(cpo, state, prev.retrafo, c("retrafo", if (cpo$hybrid.inverter) "inverter"))
  # --- only in "retrafo" kind
  retrafo$shapeinfo.input = shapeinfo.input
  retrafo$shapeinfo.output = shapeinfo.output
  retrafo$properties.needed = cpo$properties$properties.needed
  retrafo
}


makeCPOS3RetrafoBasic = function(cpo, state, prev.retrafo, kind) {
  retrafo = makeS3Obj(c("CPOS3RetrafoPrimitive", "CPOS3Retrafo", "CPORetrafo"),
    cpo = setCPOId(cpo, NULL),
    state = state,
    prev.retrafo = NULL,
    # --- Target Bound things
    bound = cpo$bound,
    predict.type = cpo$predict.type,  # named list type to predict --> needed type
    kind = kind)
  if (!is.null(prev.retrafo)) {
    retrafo = composeCPO(prev.retrafo, retrafo)
  }
  retrafo
}


##################################
### Primary Operations         ###
##################################

# CPOS3 is a tree datastructure. CPOS3Primitive are
# the leaves, CPOS3Tree the nodes.
# CPOS3Retrafo is a linked list, which gets automatically
# constructed in 'callCPO'.
callCPO = function(cpo, data, build.retrafo, prev.retrafo, build.inverter, prev.inverter) {
  UseMethod("callCPO")
}

# TRAFO main function
# - checks the inbound and outbound data is in the right format
# - data will be turned into the shape requested by the cpo
# - properties check (inbound, and outbound)
# - automatically subsets 'args' to the relevant ones for cpo
# - collects control / cpo.retrafo from called function
# - returns list(data, retrafo = [CPORetrafo object])

# attaches prev.retrafo to the returned retrafo object, if present.
callCPO.CPOS3Primitive = function(cpo, data, build.retrafo, prev.retrafo, build.inverter, prev.inverter) {

  requireCPOPackages(cpo)

  checkAllParams(cpo$par.vals, cpo$par.set, cpo$name)
  if (is.nullcpo(prev.retrafo)) {
    prev.retrafo = NULL
  }
  if (is.nullcpo(prev.inverter)) {
    prev.inverter = NULL
  }
  if (!build.inverter) {
    assertNull(prev.inverter)
    inverter = NULL
  }
  if (is.null(prev.retrafo)) {
    prevneeded = character(0)
  } else {
    prevneeded = prev.retrafo$properties.needed
    assertCharacter(prevneeded, unique = TRUE)
    assertSubset(prevneeded, cpo$properties$properties)  # this should never happen, since we test this during CPO composition
  }



  tin = prepareTrafoInput(data, cpo$datasplit, cpo$properties$properties.data, getCPOAffect(cpo, FALSE), cpo$fix.factors, cpo$name)
  if (!cpo$data.dependent) {
    assert(cpo$bound == "targetbound")
    tin$indata$data = NULL
  }
  if (is.null(cpo$trafo)) {
    # stateless trafo-less CPO
    tin$indata$target = NULL
    result = do.call(cpo$trafo, insert(getBareHyperPars(cpo), tin$indata))
  } else {
    result = do.call(cpo$trafo, insert(getBareHyperPars(cpo), tin$indata))

    trafoenv = .ENV
  }
  assertChoice(cpo$type, c("functional", "object"))
  if (cpo$type == "functional") {
    state = trafoenv$cpo.retrafo
    if (cpo$bound == "targetbound") {
      requiredargs = c("target", "predict.type")
      if (is.null(state) || !isTRUE(checkFunction(state, args = requiredargs, nargs = 2))) {
        stopf('.data.dependent targetbound CPO %s cpo.trafo must set a variable "cpo.retrafo"\n%s"%s".',
          cpo$name, "to a function with two arguments ", collapse(requiredargs, sep = '", "'))
      }
    } else if (is.null(state) || !isTRUE(checkFunction(state, nargs = 1))) {
      stopf("CPO %s cpo.trafo did not set a variable 'cpo.retrafo' to a function with one argument.", cpo$name)
    }
    if (!"data" %in% names(formals(state)) && referencesNonfunctionNames(body(state), "data") && cpo$data.dependent) {
      warning(paste("The function given as cpo.retrafo references a 'data' variable.",
        "Beware that the 'data' variable as given as an argument to the surrounding function",
        "will not be accessible when cpo.retrafo is called.",
        "If you still need to access this data, copy it to a variable with a different name.",
        "If this warning is a false positive and you assign the 'data' variable properly, you can avoid",
        "this warning by giving it a name different from 'data'.", sep = "\n"))
    }
    trafoenv$data = NULL
  } else {  # cpo$type == "object"
    if (!cpo$stateless && !"control" %in% ls(trafoenv)) {
      stopf("CPO %s cpo.trafo did not create a 'control' object. Use the .stateless flag on creation if you don't need a control object.", cpo$name)
    }
    state = if (cpo$stateless) NULL else trafoenv$control
  }

  # the properties of the output should only be the input properties + the ones we're adding
  allowed.properties = union(tin$properties, cpo$properties$properties.needed)
  tout = handleTrafoOutput(result, data, tin$tempdata, cpo$datasplit, allowed.properties, cpo$properties$properties.adding,
    cpo$bound == "targetbound", cpo$convertto, tin$subset.index, cpo$name)



  retrafo = if (build.retrafo) makeCPOS3Retrafo(cpo, state, prev.retrafo, tin$shapeinfo, tout$shapeinfo) else prev.retrafo

  inverter = if (build.inverter && cpo$bound == "targetbound") makeCPOS3Inverter(cpo, state, prev.inverter, data, tin$shapeinfo) else prev.inverter

  list(data = tout$outdata, retrafo = retrafo, inverter = inverter)
}

# call cpo$first, then cpo$second, and chain the retrafos.
#
# A CPOS3 tree looks like this:
#
#                 CPOS3Tree
#               /[first]    \[second]
#      CPOS3Tree             CPOS3Tree
#     /[first]  \[second]   /[first]  \[second]
# CPOS3Prim1 CPOS3Prim2 CPOS3Prim3 CPOS3Prim4
#
# callCPO calls go as:
#
#    1 -------> 2 -------> 3 -------> 4
#
# Retrafos will be chained by 'prev.retrafo', as:
#
# retr.1 <-- retr.2 <-- retr.3 <-- retr.4
#
callCPO.CPOS3Tree = function(cpo, data, build.retrafo, prev.retrafo, build.inverter, prev.inverter) {
  checkAllParams(cpo$par.vals, cpo$par.set, cpo$name)
  first = cpo$first
  second = cpo$second
  first$par.vals = subsetParams(cpo$par.vals, first$par.set)
  second$par.vals = subsetParams(cpo$par.vals, second$par.set)
  intermediate = callCPO(first, data, build.retrafo, prev.retrafo, build.inverter, prev.inverter)
  callCPO(second, intermediate$data, build.retrafo, intermediate$retrafo, build.inverter, intermediate$inverter)
}

# RETRAFO main function
# - checks the inbound and outbound data is in the right format
# - checks the shape of input and output is as was before
# - data will be turned into the shape requested by the cpo
# - properties check (inbound, and outbound)
# - automatically subsets 'args' to the relevant ones for cpo
# - possibly calls next.retrafo
# - returns the resulting data

# receiver.properties are the properties of the next layer
applyCPORetrafoEx = function(retrafo, data, build.inverter, prev.inverter) {

  assertClass(retrafo, "CPOS3Retrafo")
  cpo = retrafo$cpo

  requireCPOPackages(cpo)
  if (!"retrafo" %in% retrafo$kind) {
    stop("Object %s is an inverter, not a retrafo.", cpo$bare.name)
  }

  if (!is.null(retrafo$prev.retrafo)) {
    assertSubset(retrafo$prev.retrafo$properties.needed, cpo$properties$properties)  # this is already tested during composition
    assertClass(retrafo$prev.retrafo, "CPOS3Retrafo")
    upper.result = applyCPORetrafoEx(retrafo$prev.retrafo, data, build.inverter, prev.inverter)
    data = upper.result$data
    prev.inverter = upper.result$inverter
  }

  if (cpo$bound == "targetbound") {
    return(callCPO(cpo, data, FALSE, NULL, build.inverter, prev.inverter))
  }
  assert(cpo$bound == "databound")

  tin = prepareRetrafoInput(data, cpo$datasplit, cpo$properties$properties.data, retrafo$shapeinfo.input, cpo$bare.name)

  assertChoice(cpo$type, c("functional", "object"))
  if (cpo$type == "functional") {
    result = retrafo$state(tin$indata)
  } else {  # cpo$type == "object"
    args = cpo$par.vals
    args$data = tin$indata
    if (!cpo$stateless) {
      args$control = retrafo$state
    }
    result = do.call(cpo$retrafo, args)
  }

  # the properties of the output should only be the input properties + the ones we're adding
  allowed.properties = union(tin$properties, cpo$properties$properties.needed)

  list(data = handleRetrafoOutput(result, data, tin$tempdata, cpo$datasplit, allowed.properties,
    cpo$properties$properties.adding, retrafo$shapeinfo.output, tin$subset.index, cpo$bare.name),
    inverter = prev.inverter)
}

#' @export
applyCPO.CPOS3Retrafo = function(cpo, data) {
  retrafo = cpo
  build.inverter = hasTagInvert(data)
  prev.inverter = inverter(data)
  if (is.nullcpo(prev.inverter)) {
    prev.inverter = NULL
  }
  inverter(data) = NULL
  data = tagInvert(data, FALSE)
  if (!build.inverter && !is.null(prev.inverter)) {
    stop("Data had 'inverter' attribute set, but not the 'keep.inverter' tag.")
  }
  if (!is.null(prev.inverter)) {
    assertClass(prev.inverter, "CPOS3Retrafo")
  }

  prev.retrafo = retrafo(data)
  if (is.nullcpo(prev.retrafo)) {
    prev.retrafo = NULL
  }
  retrafo(data) = NULL

  result = applyCPORetrafoEx(retrafo, data, build.inverter, prev.inverter)
  data = result$data
  retrafo(data) = prev.retrafo
  inverter(data) = result$inverter
  tagInvert(data, build.inverter)
}


##################################
### Trafo Operations           ###
##################################

# CPO %>>% CPO

#' @export
composeCPO.CPOS3 = function(cpo1, cpo2) {
  assertClass(cpo2, "CPOS3")
  parameterClashAssert(cpo1, cpo2, cpo1$name, cpo2$name)
  newprops = compositeProperties(cpo1$properties, cpo2$properties, cpo1$name, cpo2$name)
  newpt = chainPredictType(cpo1$predict.type, cpo2$predict.type, cpo1$name, cpo2$name)

  makeS3Obj(c("CPOS3Tree", "CPOS3", "CPO"),
    # --- CPOS3 Part
    bare.name = paste(cpo2$bare.name, cpo1$bare.name, sep = "."),
    name = paste(cpo1$name, cpo2$name, sep = " >> "),
    par.set = c(cpo1$par.set, cpo2$par.set),
    par.vals = c(cpo1$par.vals, cpo2$par.vals),
    properties = newprops,
    bound = unique(cpo1$bound, cpo2$bound),
    predict.type = newpt,
    # --- CPOS3Tree part
    first = cpo1,
    second = cpo2)
}

# CPO splitting
#' @export
as.list.CPOS3Tree = function(x, ...) {
  first = x$first
  second = x$second
  first$par.vals = subsetParams(x$par.vals, first$par.set)
  second$par.vals = subsetParams(x$par.vals, second$par.set)
  c(as.list(first), as.list(second))
}

# CPO %>>% Learner

#' @export
attachCPO.CPOS3 = function(cpo, learner) {
  learner = checkLearner(learner)
  if (!learner$type %in% union(cpo$properties$properties.needed, setdiff(cpo$properties$properties, cpo$properties$properties.adding))) {
    stopf("Cannot combine CPO that outputs type %s with learner of type %s.",
      cpo$convertto, learner$type)
  }

  parameterClashAssert(cpo, learner, cpo$name, getLearnerName(learner))
  if (!"CPOS3Learner" %in% class(learner)) {
    learner = makeBaseWrapper(learner$id, learner$type, learner,
      learner.subclass = c("CPOS3Learner", "CPOLearner"), model.subclass = c("CPOS3Model", "CPOModel"))
    learner$predict.type = learner$next.learner$predict.type
  } else {
    cpo = composeCPO(cpo, learner$cpo)
  }
  learner$cpo = cpo
  learner$properties = compositeCPOLearnerProps(cpo, learner$next.learner)
  learner$type = intersect(learner$properties, cpo.tasktypes)
  assertString(learner$type)
  learner$properties = setdiff(learner$properties, cpo.tasktypes)
  learner$par.vals = cpo$par.vals
  learner$par.set = cpo$par.set
  learner$id = paste(learner$id, cpo$bare.name, sep = ".")

  # possibly need to reset 'predict.type', or just change it to something else.
  prev.predict.type = learner$next.learner$predict.type
  next.predict.type = "response"
  if (prev.predict.type %in% c("response", getLearnerProperties(learner))) {  # if the previous predict.type is still supported
    next.predict.type = prev.predict.type
  }
  setPredictType(learner, next.predict.type)
}

compositeCPOLearnerProps = function(cpo, learner) {
  props = setdiff(getLearnerProperties(learner), "weights")
  props = union(props, getLearnerType(learner))
  # relevant: we only have an influence on these properties.
  relevant = c(cpo.dataproperties, cpo.targetproperties, cpo.tasktypes, "prob", "se")
  props.relevant = intersect(props, relevant)
  props.relevant = compositeProperties(cpo$properties,
    list(properties = props.relevant, properties.adding = character(0), properties.needed = character(0)),
    cpo$name, getLearnerName(learner))$properties  # checks for property problems automatically
  c(props.relevant, setdiff(props, relevant))
}

#' @export
trainLearner.CPOS3Learner = function(.learner, .task, .subset = NULL, ...) {
  if (!is.null(.subset)) {
    .task = subsetTask(.task, .subset)
  }

  cpo = .learner$cpo
  cpo$par.vals = subsetParams(.learner$par.vals, cpo$par.set)

  # note that an inverter for a model makes no sense, since the inverter is crucially bound to
  # the data that is supposed to be *predicted*.
  retrafo(.task) = NULL
  inverter(.task) = NULL
  .task = tagInvert(.task, FALSE)
  transformed = callCPO(cpo, .task, TRUE, NULL, FALSE, NULL)

  model = makeChainModel(train(.learner$next.learner, transformed$data), "CPOS3WrappedModel")
  model$retrafo = transformed$retrafo
  model
}

#' @export
predictLearner.CPOS3Learner = function(.learner, .model, .newdata, ...) {
  retrafod = applyCPORetrafoEx(.model$learner.model$retrafo, .newdata, TRUE, NULL)
  prediction = NextMethod(.newdata = retrafod$data)
  if (!is.null(retrafod$inverter)) {
    invertCPO(retrafod$inverter, prediction, .learner$predict.type)$new.prediction
  } else {
    prediction
  }
}

# get CPO from learner
singleLearnerCPO.CPOS3Learner = function(learner) {
  cpo = learner$cpo
  cpo$par.vals = subsetParams(learner$par.vals, cpo$par.set)
  cpo
}

#' @export
setPredictType.CPOS3Learner = function(learner, predict.type) {
  assertChoice(predict.type, c("response", "prob", "se"))
  ptconvert = learner$cpo$predict.type
  supported.below = c(intersect(getLearnerProperties(learner$next.learner), c("prob", "se")), "response")
  supported.here = names(ptconvert)[ptconvert %in% supported.below]
  assertSetEqual(supported.here, c(intersect(getLearnerProperties(learner), c("prob", "se")), "response"))
  if (!predict.type %in% supported.here) {
    stopf("Trying to predict %s, but %s does not support that.", predict.type, learner$id)
  }
  learner$predict.type = predict.type
  learner$next.learner = setPredictType(learner$next.learner, ptconvert[predict.type])
  learner
}

# DATA %>>% CPO
#' @export
applyCPO.CPOS3 = function(cpo, task) {
  if ("Task" %in% class(task) && !is.null(getTaskWeights(task))) {
    stop("CPO can not handle tasks with weights!")
  }
  build.inverter = hasTagInvert(task)
  prev.inverter = inverter(task)
  if (is.nullcpo(prev.inverter)) {
    prev.inverter = NULL
  }
  if (!build.inverter && !is.null(prev.inverter)) {
    stop("Data had 'inverter' attribute set, but not the 'keep.inverter' tag.")
  }
  if (!is.null(prev.inverter)) {
    assertClass(prev.inverter, "CPOS3Retrafo")
  }
  prev.retrafo = retrafo(task)

  retrafo(task) = NULL
  inverter(task) = NULL
  task = tagInvert(task, FALSE)

  result = callCPO(cpo, task, TRUE, prev.retrafo, build.inverter, prev.inverter)
  task = result$data
  retrafo(task) = result$retrafo
  inverter(task) = result$inverter
  tagInvert(task, build.inverter)
}

# Param Sets
#' @export
getParamSet.CPOS3 = function(x) {
  x$par.set
}

#' @export
getHyperPars.CPOS3 = function(learner, for.fun = c("train", "predict", "both")) {
  learner$par.vals
}

#' @export
setHyperPars2.CPOS3 = function(learner, par.vals = list()) {
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
  assertClass(cpo, "CPOS3Primitive")
  args = cpo$par.vals
  namestranslation = setNames(names(cpo$bare.par.set$pars),
    names(cpo$par.set$pars))
  setNames(args, namestranslation[names(args)])
}

# Properties

#' @export
getCPOProperties.CPOS3 = function(cpo, only.data = FALSE) {
  if (only.data) {
    lapply(cpo$properties, intersect, y = cpo.dataproperties)
  } else {
    cpo$properties
  }
}


# CPO ID, NAME

#' @export
getCPOName.CPOS3 = function(cpo) {
  cpo$name
}

#' @export
setCPOId.CPOS3Primitive = function(cpo, id) {
  cpo$id = id
  cpo$name = collapse(c(cpo$bare.name, id), sep = ".")
  cpo$par.vals = getBareHyperPars(cpo)
  cpo$par.set = cpo$bare.par.set
  pars = cpo$par.set$pars
  if (!is.null(id) && length(pars)) {
    trans = setNames(paste(id, names(pars), sep = "."), names(pars))
    names(pars) = trans
    pars = lapply(pars, function(x) {
      x$id = trans[x$id]
      if (!is.null(x$requires)) {
        x$requires = renameNonfunctionNames(x$requires, trans)
      }
      x
    })
    cpo$par.set$pars = pars
    names(cpo$par.vals) = trans[names(cpo$par.vals)]
  }
  cpo
}

#' @export
getCPOBound.CPOS3 = function(cpo) {
  cpo$bound
}

#' @export
getCPOKind.CPOS3 = function(cpo) {
  "trafo"
}

#' @export
getCPOAffect.CPOS3Primitive = function(cpo, drop.defaults = TRUE) {
  affect.args = cpo$affect.args
  if (!drop.defaults) {
    if (!length(getCPOAffect(cpo))) {
      affect.args$type = c("numeric", "factor", "ordered", "other")
    }
    return(affect.args)
  }
  if (setequal(affect.args$type, c("numeric", "factor", "ordered", "other"))) {
    affect.args$type = NULL
  }
  if (!length(affect.args$index)) {
    affect.args$index = NULL
  }
  if (!length(affect.args$names)) {
    affect.args$names = NULL
  }
  if (is.null(affect.args$pattern)) {
    affect.args$pattern.ignore.case = NULL
    affect.args$pattern.perl = NULL
    affect.args$pattern.fixed = NULL
  }
  Filter(function(x) !is.null(x) && !identical(x, FALSE), affect.args)
}

##################################
### Retrafo Operations         ###
##################################

# get RETRAFO from learner
# 'prevfun' is not a function for CPOS3!
singleModelRetrafo.CPOS3Model = function(model, prev) {
  retrafo = model$learner.model$retrafo
  if (!is.null(prev)) {
    retrafo = composeCPO(prev, retrafo)
  }
  retrafo
}

# RETRAFO %>>% RETRAFO

#' @export
composeCPO.CPOS3Retrafo = function(cpo1, cpo2) {
  assertClass(cpo2, "CPOS3Retrafo")
  is.prim = "CPOS3RetrafoPrimitive" %in% class(cpo2)
  assert(is.prim == is.null(cpo2$prev.retrafo))
  newkind = intersect(cpo1$kind, cpo2$kind)
  if (!length(newkind)) {
    stopf("Cannot compose retrafos of kind %s with retrafos of kind %s.",
      collapse(cpo1$kind), collapse(cpo2$kind))
  }
  if (!is.prim) {
    cpo1 = composeCPO(cpo1, cpo2$prev.retrafo)
  }
  class(cpo2) = setdiff(class(cpo2), "CPOS3RetrafoPrimitive")

  # check for properties match
  if ("retrafo" %in% newkind) {
    cpo2$properties.needed = compositeProperties(
        list(properties = character(0), properties.adding = character(0), properties.needed = cpo1$properties.needed),
        cpo2$cpo$properties, getCPOName(cpo1), cpo2$cpo$bare.name)$properties.needed
  }
  if ("inverter" %in% newkind) {
    if (length(newkind) == 1) {
      # pure inverter chaining: do myopic property checking
      assert(length(cpo1$kind) == 1)
      assertString(cpo1$cpo$convertto)
      assertString(cpo2$cpo$convertfrom)
      if (cpo1$convertto != cpo2$convertfrom) {
        stopf("Incompatible chaining of inverters: %s converts to %s, but %s needs %s.",
          cpo1$cpo$barelname, cpo1$cpo$convertto, cpo2$cpo$bare.name, cpo2$cpo$bare.name)
      }
      compositeProperties(cpo1$cpo$properties, cpo2$cpo$properties, cpo1$cpo$bare.name, cpo2$cpo$bare.name)  # just for checking
    }
    assert(length(cpo1$predict.type) <= 2)  # predict.type cannot be the identity
    assert(length(cpo2$predict.type) <= 2)  # the identity (and only the identity) has more than 2 elements.
  }

  cpo2$predict.type = chainPredictType(cpo1$predict.type, cpo2$cpo$predict.type, getCPOName(cpo1), cpo2$cpo$bare.name)
  cpo2$prev.retrafo = cpo1
  cpo2$bound = unique(cpo2$cpo$bound, cpo1$bound)
  cpo2$kind = newkind
  cpo2
}

# chain CPOs with predict.type pt1 %>>% pt2  # FIXME: sort this where it belongs
chainPredictType = function(pt1, pt2, name1, name2) {
  result = sapply(pt1, function(x) unname(pt2[x]))
  result = result[!is.na(result)]
  if (!length(result)) {
    # So this is a bit of a weird situation: The CPO chain would work for trafo AND retrafo, but not for predictions.
    stopf("Incompatible chaining of inverters: %s needs a predict.type being%s '%s', but %s can only deliver type%s '%s'.",
      name1, ifelse(length(pt1) == 1, " one of", ""), collapse(pt1, sep = "', '"),
      name2, ifelse(length(pt2) > 1, "s", ""), collapse(pt2, sep = "', '"))
  }
  result
}




# RETRAFO splitting

#' @export
as.list.CPOS3Retrafo = function(x, ...) {
  assert(length(list(...)) == 0)
  prev = if (!is.null(x$prev.retrafo)) as.list(x$prev.retrafo)
  x$prev.retrafo = NULL
  if ("retrafo" %in% x$kind) {
    x$properties.needed = x$cpo$properties$properties.needed
  }
  x$predict.type = x$cpo$predict.type
  x$bound = x$cpo$bound
  if (identical(x$kind, "retrafo")) {
    if (x$cpo$hybrid.inverter) {
      x$kind = c("retrafo", "inverter")
    }
  }
  class(x) = unique(c("CPOS3RetrafoPrimitive", class(x)))
  c(prev, list(x))
}

# RETRAFO State

#' @export
getRetrafoState.CPOS3RetrafoPrimitive = function(retrafo.object) {
  cpo = retrafo.object$cpo
  if (!"retrafo" %in% retrafo.object$kind) {
    stop("Cannot get state of inverter")
  }
  assertChoice(cpo$type, c("functional", "object"))
  if (cpo$type == "functional") {
    res = as.list(environment(retrafo.object$state))
    if (!"cpo.retrafo" %in% names(res)) {
      res$cpo.retrafo = retrafo.object$state
    } else if (!identical(res$cpo.retrafo, retrafo.object$state)) {
      stopf("Could not get coherent state of CPO Retrafo %s, since 'cpo.retrafo' in\n%s",
        cpo$name, "the environment of the retrafo function is not identical to the retrafo function.")
    }
  } else {  # cpo$type == "object
    res = cpo$par.vals
    res$control = retrafo.object$state
  }
  # c() to drop the retrafo.object's class
  res$data = c(retrafo.object[c("shapeinfo.input", "shapeinfo.output")])  # nolint
  res
}

#' @export
makeRetrafoFromState.CPOS3Constructor = function(constructor, state) {
  assertList(state, names = "unique")
  bare = constructor()

  data = state$data
  state$data = NULL
  assertSetEqual(names(data), c("shapeinfo.input", "shapeinfo.output"))

  assertChoice(bare$type, c("functional", "object"))
  if (bare$type == "functional") {
    assertSubset("cpo.retrafo", names(state))
    bare$par.vals = list()

    newstate = state$cpo.retrafo
    # update newstate's environment to actually contain the
    # values set in the 'state'
    env = new.env(parent = parent.env(environment(newstate)))
    list2env(state, envir = env)
    environment(newstate) = env
    # also set the 'cpo.retrafo' in the env to point to the current 'newstate' function.
    # if we did not do this, the 'cpo.retrafo' variable visible to newstate would
    # be the same function *but with a different environment* -- recursion would break
    # (this is because of 'environment(newstate) = env' above)
    env$cpo.retrafo = newstate
  } else {  # bare$type == "object
    assertSubset("control", names(state))
    newstate = state$control
    state$control = NULL
    bare$par.vals = state
    assertSubset(names(bare$par.vals), names(bare$bare.par.set$pars))
  }

  makeCPOS3Retrafo(bare, newstate, NULL, data$shapeinfo.input, data$shapeinfo.output)
}

# Param Sets

#' @export
getParamSet.CPOS3RetrafoPrimitive = function(x) {
  x$cpo$par.set
}

#' @export
getHyperPars.CPOS3RetrafoPrimitive = function(learner, for.fun = c("train", "predict", "both")) {
  learner$cpo$par.vals
}

#' @export
getCPOProperties.CPOS3Retrafo = function(cpo, only.data = FALSE) {
  if (!is.null(cpo$prev.retrafo)) {
    props = compositeProperties(getCPOProperties(cpo$prev.retrafo), cpo$cpo$properties, "[PREVIOUS RETRAFO CHAIN]", cpo$cpo$bare.name)
  } else {
    props = cpo$cpo$properties
  }
  if (only.data) {
    lapply(props, intersect, y = cpo.dataproperties)
  } else {
    props
  }
}

#' @export
getCPOName.CPOS3RetrafoPrimitive = function(cpo) {
  cpo$cpo$bare.name
}

#' @export
getCPOBound.CPOS3Retrafo = function(cpo) {
  cpo$bound
}

#' @export
getCPOKind.CPOS3Retrafo = function(cpo) {
  cpo$kind
}

#' @export
getCPOPredictType.CPOS3Retrafo = function(cpo) {
  names(cpo$predict.type)
}

#' @export
getCPOName.CPOS3Constructor = function(cpo) {
  environment(cpo)$.cpo.name
}

#' @export
getCPOId.CPOS3Primitive = function(cpo) {
  cpo$id
}

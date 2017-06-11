
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


#'   If \dQuote{type} is \dQuote{targetbound}, the returned data must be either the full \dQuote{data.frame}
#'   (if \dQuote{.datasplit} is \dQuote{no}), the full task (if \dQuote{.datasplit} is \dQuote{task}), or
#'   a \dQuote{data.frame} only consisting of the target features (if \dQuote{.datasplit} is any other value
#'   -- in this case, the returned value is \dQuote{target} as given to the function's \dQuote{target} argument).


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
                   .properties = c("numerics", "factors", "ordered", "missings"),
                   .properties.adding = character(0), .properties.needed = character(0),
                   cpo.trafo, cpo.retrafo) {
  # dotted parameter names are necessary to avoid problems with partial argument matching.
  # The reason cpo.trafo and cpo.retrafo are not dotted is that they always need to be given.
  # If that changes, they would also neede to be dotted.
  assertList(.par.vals, names = "unique")
  assertString(.cpo.name)
  if (is.null(.par.set)) {
    .par.set = paramSetSugar(..., .pss.env = parent.frame())
  }

  .datasplit = match.arg(.datasplit)
  .properties = match.arg(.properties, several.ok = TRUE)
  assertCharacter(.properties, unique = TRUE)
  assertCharacter(.properties.needed, unique = TRUE)
  assertCharacter(.properties.adding, unique = TRUE)
  assertSubset(.properties.adding, .properties)
  assertSubset(.properties.needed, c("numerics", "factors", "ordered", "missings"))
  badprops = intersect(.properties.adding, .properties.needed)
  if (length(badprops)) {
    stopf(".properties.adding and .properties.needed must not contain the same properties, but both contained %s.",
      collapse(badprops, sep = ", "))
  }

  # these parameters are either special parameters given to the constructor function (id),
  # special parameters given to the cpo.trafo function (data, target), special parameters given to the
  # cpo.retrafo function (control),

  reserved.params = c("data", "target", "control", "id")
  if (any(names(.par.set$pars) %in% reserved.params)) {
    stopf("Parameters %s are reserved", collapse(reserved.params, ", "))
  }

  .par.vals = insert(getParamSetDefaults(.par.set), .par.vals)

  assert(length(setdiff(names(.par.vals), names(.par.set$pars))) == 0)

  checkParamsFeasible(.par.set, .par.vals)

  funargs = lapply(.par.set$pars, function(dummy) substitute())
  funargs = insert(funargs, .par.vals)

  required.arglist.trafo = funargs
  required.arglist.trafo$data = substitute()
  required.arglist.trafo$target = substitute()
  cpo.trafo = makeFunction(substitute(cpo.trafo), required.arglist.trafo, env = parent.frame())
  cpo.trafo = captureEnvWrapper(cpo.trafo)

  retrafo.expr = substitute(cpo.retrafo)
  if ((is.recursive(retrafo.expr) && identical(retrafo.expr[[1]], quote(`{`))) || !is.null(cpo.retrafo)) {
    required.arglist.retrafo = funargs
    required.arglist.retrafo$data = substitute()
    required.arglist.retrafo$control = substitute()
    cpo.retrafo = makeFunction(retrafo.expr, required.arglist.retrafo, env = parent.frame())
  }

  funargs = insert(funargs, list(id = NULL))

  funbody = quote({
    # in the first two code lines, there are still arguments around that we don't know the names of.
    # therefore, we need to catch them into the 'args' list and delete them, always referencing the
    # functions we mean by '::'.
    args = base::match.call()
    base::rm(list = base::setdiff(base::ls(), "args"))  # delete all arguments to avoid name clashes
    args[[1]] = quote(list)
    args = eval(args, envir = parent.frame())
    args = insert(funargs, args)
    if (!is.null(args$id)) {
      assertString(args$id)
    }
    present.pars = Filter(function(x) !identical(x, substitute()), args[names(.par.set$pars)])
    checkParamsFeasible(.par.set, present.pars)
    cpo = makeS3Obj(c("CPOS3Primitive", "CPOPrimitive", "CPOS3", "CPO"),
      # --- CPOS3 part
      bare.name = .cpo.name,
      name = .cpo.name,
      par.set = .par.set,
      par.vals = present.pars,
      properties = list(properties = .properties,
        properties.adding = .properties.adding,
        properties.needed = .properties.needed),
      # --- CPOS3Primitive part
      id = NULL,
      bare.par.set = .par.set,
      datasplit = .datasplit,
      type = ifelse(is.null(cpo.retrafo), "functional", "object"),
      trafo = cpo.trafo,
      retrafo = cpo.retrafo)
    setCPOId(cpo, args$id)  # this also adjusts par.set and par.vals
  })
  addClasses(eval(call("function", as.pairlist(funargs), funbody)), c("CPOS3Constructor", "CPOConstructor"))
}

##################################
### Primary Operations         ###
##################################

# CPOS3 is a tree datastructure. CPOS3Primitive are
# the leaves, CPOS3Tree the nodes.
# CPOS3Retrafo is a linked list, which gets automatically
# constructed in 'callCPO'.
callCPO = function(cpo, data, prev.retrafo) {
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
callCPO.CPOS3Primitive = function(cpo, data, prev.retrafo) {
  if (is.null(prev.retrafo)) {
    prevneeded = character(0)
  } else {
    prevneeded = prev.retrafo$properties.needed
    assertCharacter(prevneeded, unique = TRUE)
    assertSubset(prevneeded, cpo$properties$properties)  # this should never happen, since we test this during CPO composition
  }

  tin = prepareTrafoInput(data, cpo$datasplit, cpo$properties$properties, cpo$name)
  result = do.call(cpo$trafo, insert(getBareHyperPars(cpo), tin$indata))

  trafoenv = environment(cpo$trafo)$.ENV
  assign(".ENV", NULL, envir = environment(cpo$trafo))
  assertChoice(cpo$type, c("functional", "object"))
  if (cpo$type == "functional") {
    state = trafoenv$cpo.retrafo
    if (is.null(state) || !isTRUE(checkFunction(state, nargs = 1))) {
      stopf("CPO %s cpo.trafo did not set a variable 'cpo.retrafo' to a function with one argument.", cpo$name)
    }
    if (!"data" %in% names(formals(state)) && referencesNonfunctionNames(body(state), "data")) {
      warning(paste("The function given as cpo.retrafo references a 'data' variable.",
        "Beware that the 'data' variable as given as an argument to the surrounding function",
        "will not be accessible when cpo.retrafo is called.",
        "If you still need to access this data, copy it to a variable with a different name.",
        "If this warning is a false positive and you assign the 'data' variable properly, you can avoid",
        "this warning by giving it a name different from 'data'.", sep = "\n"))
    }
    trafoenv$data = NULL
  } else {  # cpo$type == "object"
    if (!"control" %in% ls(trafoenv)) {
      stopf("CPO %s cpo.trafo did not create a 'control' object.", cpo$name)
    }
    state = trafoenv$control
  }

  # the properties of the output should only be the input properties + the ones we're adding
  allowed.properties = union(tin$properties, cpo$properties$properties.needed)
  tout = handleTrafoOutput(result, data, tin$tempdata, cpo$datasplit, allowed.properties, cpo$properties$properties.adding, cpo$name)

  retrafo = makeS3Obj(c("CPOS3RetrafoPrimitive", "CPOS3Retrafo", "CPORetrafo"),
    cpo = setCPOId(cpo, NULL),
    state = state,
    shapeinfo.input = tin$shapeinfo,
    shapeinfo.output = tout$shapeinfo,
    properties.needed = cpo$properties$properties.needed,
    prev.retrafo = NULL)
  if (!is.null(prev.retrafo)) {
    retrafo = composeCPO(prev.retrafo, retrafo)
  }

  list(data = tout$outdata, retrafo = retrafo)
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
callCPO.CPOS3Tree = function(cpo, data, prev.retrafo) {
  first = cpo$first
  second = cpo$second
  first$par.vals = subsetParams(cpo$par.vals, first$par.set, first$name)
  second$par.vals = subsetParams(cpo$par.vals, second$par.set, second$name)
  intermediate = callCPO(first, data, prev.retrafo)
  callCPO(second, intermediate$data, intermediate$retrafo)
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
applyCPO.CPOS3Retrafo = function(retrafo, data) {
  cpo = retrafo$cpo
  if (!is.null(retrafo$prev.retrafo)) {
    assertSubset(retrafo$prev.retrafo$properties.needed, cpo$properties$properties)  # this is already tested during composition
    assertClass(retrafo$prev.retrafo, "CPOS3Retrafo")
    data = applyCPO(retrafo$prev.retrafo, data)
  }

  tin = prepareRetrafoInput(data, cpo$datasplit, cpo$properties$properties, retrafo$shapeinfo.input, cpo$bare.name)

  assertChoice(cpo$type, c("functional", "object"))
  if (cpo$type == "functional") {
    result = retrafo$state(tin$indata)
  } else {  # cpo$type == "object"
    result = do.call(cpo$retrafo, insert(cpo$par.vals, list(data = tin$indata, control = retrafo$state)))
  }

  # the properties of the output should only be the input properties + the ones we're adding
  allowed.properties = union(tin$properties, cpo$properties$properties.needed)

  handleRetrafoOutput(result, data, tin$tempdata, cpo$datasplit, allowed.properties, cpo$properties$properties.adding, retrafo$shapeinfo.output, cpo$bare.name)
}

##################################
### Trafo Operations           ###
##################################

# CPO %>>% CPO

composeCPO.CPOS3 = function(cpo1, cpo2) {
  assertClass(cpo2, "CPOS3")
  parameterClashAssert(cpo1, cpo2, cpo1$name, cpo2$name)
  newprops = compositeProperties(cpo1$properties, cpo2$properties, cpo1$name, cpo2$name)

  makeS3Obj(c("CPOS3Tree", "CPOS3", "CPO"),
    # --- CPOS3 Part
    bare.name = paste(cpo2$bare.name, cpo1$bare.name, sep = "."),
    name = paste(cpo1$name, cpo2$name, sep = " >> "),
    par.set = c(cpo1$par.set, cpo2$par.set),
    par.vals = c(cpo1$par.vals, cpo2$par.vals),
    properties = newprops,
    # --- CPOS3Tree part
    first = cpo1,
    second = cpo2)
}

# CPO splitting
#' @export
as.list.CPOS3Tree = function(x, ...) {
  first = x$first
  second = x$second
  first$par.vals = subsetParams(x$par.vals, first$par.set, first$name)
  second$par.vals = subsetParams(x$par.vals, second$par.set, second$name)
  c(as.list(first), as.list(second))
}

# CPO %>>% Learner

attachCPO.CPOS3 = function(cpo, learner) {
  learner = checkLearner(learner)
  parameterClashAssert(cpo, learner, cpo$name, getLearnerName(learner))
  if (!"CPOS3Learner" %in% class(learner)) {
    learner = makeBaseWrapper(learner$id, learner$type, learner,
      learner.subclass = c("CPOS3Learner", "CPOLearner"), model.subclass = c("CPOS3Model", "CPOModel"))
  } else {
    cpo = composeCPO(cpo, learner$cpo)
  }
  learner$cpo = cpo
  learner$properties = compositeCPOLearnerProps(cpo, learner$next.learner)
  learner$par.vals = cpo$par.vals
  learner$par.set = cpo$par.set
  learner$id = paste(learner$id, cpo$bare.name, sep = ".")
  learner
}

compositeCPOLearnerProps = function(cpo, learner) {
  props = getLearnerProperties(learner)
  relevant = c("numerics", "factors", "ordered", "missings")
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
  cpo$par.vals = subsetParams(.learner$par.vals, cpo$par.set, cpo$name)

  transformed = callCPO(cpo, .task, NULL)

  model = makeChainModel(train(.learner$next.learner, transformed$data), "CPOS3WrappedModel")
  model$retrafo = transformed$retrafo
  model
}

#' @export
predictLearner.CPOS3Learner = function(.learner, .model, .newdata, ...) {
  NextMethod(.newdata = applyCPO(.model$learner.model$retrafo, .newdata))
}

# get CPO from learner
singleLearnerCPO.CPOObjectLearner = function(learner) {
  cpo = learner$cpo
  cpo$par.vals = subsetParams(learner$par.vals, cpo$par.set, cpo$name)
  cpo
}

# DATA %>>% CPO

applyCPO.CPOS3 = function(cpo, task) {
  result = callCPO(cpo, task, retrafo(task))
  task = result$data
  retrafo(task) = result$retrafo
  task
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
getCPOProperties.CPOS3 = function(cpo) {
  properties = cpo$properties
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

composeCPO.CPOS3Retrafo = function(cpo1, cpo2) {
  assertClass(cpo2, "CPOS3Retrafo")
  is.prim = "CPOS3RetrafoPrimitive" %in% class(cpo2)
  assert(is.prim == is.null(cpo2$prev.retrafo))
  if (!is.prim) {
    cpo1 = composeCPO(cpo1, cpo2$prev.retrafo)
  }
  class(cpo2) = setdiff(class(cpo2), "CPOS3RetrafoPrimitive")

  # check for properties match
  cpo2$properties.needed = compositeProperties(
      list(properties = character(0), properties.adding = character(0), properties.needed = cpo1$properties.needed),
      cpo2$cpo$properties, getCPOName(cpo1), cpo2$cpo$bare.name)$properties.needed

  cpo2$prev.retrafo = cpo1
  cpo2
}

# RETRAFO splitting

#' @export
as.list.CPOS3Retrafo = function(x, ...) {
  assert(length(list(...)) == 0)
  prev = if (!is.null(x$prev.retrafo)) as.list(x$prev.retrafo)
  x$prev.retrafo = NULL
  x$properties.needed = x$cpo$properties$properties.needed
  class(x) = unique(c("CPOS3RetrafoPrimitive", class(x)))
  c(prev, list(x))
}

# RETRAFO State

#' @export
getRetrafoState.CPOS3RetrafoPrimitive = function(retrafo.object) {
  cpo = retrafo.object$cpo
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

  retrafo = insert(makeS3Obj(c("CPOS3RetrafoPrimitive", "CPOS3Retrafo", "CPORetrafo"),
    cpo = bare,
    state = newstate,
    properties.needed = bare$properties$properties.needed,
    prev.retrafo = NULL), data)
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
getCPOProperties.CPOS3Retrafo = function(cpo) {
  if (!is.null(cpo$prev.retrafo)) {
    compositeProperties(getCPOProperties(cpo$prev.retrafo), cpo$cpo$properties, "[PREVIOUS RETRAFO CHAIN]", cpo$cpo$bare.name)
  } else {
    cpo$cpo$properties
  }
}

#' @export
getCPOName.CPOS3RetrafoPrimitive = function(cpo) {
  cpo$cpo$bare.name
}

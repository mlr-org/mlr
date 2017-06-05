
#' Turn the argument list into a \code{ParamSet} of \code{LearnerParam}s
#'
#' The arguments are of the form \code{name = default: type range ^ dimension [settings]}.
#'
#' \dQuote{name} is any valid R identifier name.
#'
#' \dQuote{= default} may be absent. If present, it determines the 'default' setting
#' in \dQuote{makeXXXLearnerParam}. Note that this is different from an R function parameter
#' default value, in that it serves only as information to the user and does not set the
#' parameter to this value if it is not given.
#'
#' \dQuote{type} is one of
#' \dQuote{integer}, \dQuote{numeric}, \dQuote{logical}, \dQuote{discrete}.
#'
#' \dQuote{range} is absent for type \dQuote{logical}. For \dQuote{discrete},
#' it is either \code{(valuelist)} with \dQuote{valuelist} evaluating to a list,
#' or of the form \code{(value1, value2, ...)}, creating a discrete parameter of character
#' or numeric values according to \dQuote{value1},
#' \dQuote{value2} etc. If \dQuote{type} is one of \dQuote{integer} or \dQuote{numeric},
#' \dQuote{range} is of the form \code{(lowBound, upBound)}, where \dQuote{lowBound}
#' and \dQuote{upBound} must either be numerical (or integer) values indicating the
#' lower and upper bound, or may be missing (indicating the absence of a bound). To indicate
#' an exclusive bound, prefix the values with a tilda (\dQuote{~}). For a "numeric" variable, to
#' indicate an unbounded value which may not be infinite, use ond dot (\dQuote{.}).
#' Note that \code{ParamHelpers} makes it impossible to specify values unbounded above and below
#' that accept only infinite values with a specified sign.
#'
#'
#' \dQuote{^ dimension} may be absent, resulting in a normal \dQuote{LearnerParam}, or present,
#' resulting in a \dQuote{VectorLearnerParam}. Note that a one-dimensional \dQuote{VectorLearnerParam}
#' is distinct from a normal \dQuote{LearnerParam}.
#'
#' \dQuote{[settings]} may be a collection of further settings to supply to \dQuote{makeXXXLearnerParam}
#' and is optional.
#'
#' This makes definition of \code{ParamSet}s shorter and more readable.
#'
#' Examples:
#'
#' \code{paramSetSugar(a: integer (~0, )^2 [requires = expression(b != 0)],
#'     b = -10: numeric (., 0), c: discrete (x, y, 1))}
#' is equivalent to
#' \code{
#'   makeParamSet(
#'     makeIntegerVectorLearnerParam('a', len = 2, lower = 1,  # note exclusive bound
#'          upper = Inf, allow.inf = TRUE, requires = expression(b != 0)),
#'     makeNumericLearnerParam('b', lower = -Inf, upper = 0,
#'          allow.inf = FALSE, default = -10),  # note infinite value is prohibited.
#'     makeDiscreteLearnerParam('c', values = list(x = "x", y = "y", `1` = 1))
#'   )
#' }
#'
#' @param pss.learner.params [\code{logical}]\cr
#'   Whether to create \dQuote{LearnerParam} instead of \dQuote{Param} objects.
#'   Default is \dQuote{TRUE}.
#' @param pss.env [\code{environment}]\cr
#'   Which environment to use when evaluating expressions. Defaults to the calling
#'   function's frame.
#' @export
paramSetSugar = function(..., pss.learner.params = TRUE, pss.env = parent.frame()) {
  promises = substitute(paramSetSugar(...))  # match.call doesn't work with indirect calls.
  promises[[1]] = NULL
  allparams = lapply(seq_along(promises), function(paridx) {
    thispar = promises[[paridx]]
    name = coalesce(names(promises)[paridx], "")
    parseSingleParameter(name, thispar, pss.learner.params, pss.env)
  })
  makeParamSet(params = allparams)
}

### Auxiliary functions

# formerr: Give informational error about malformed parameter
formerr = function(pstring, specific) {
    stopf("Parameter '%s' must be of the form\n%s\n%s", pstring,
          "NAME [= DEFAULT]: TYPE [RANGE] [^ DIMENSION] [SETTINGS]",
          specific)
}

# get the makeXXXParam function appropriate for the type and vector-ness
getConstructor = function(type, is.learner, is.vector) {
  normal.const = list(numeric = makeNumericParam,
                     integer = makeIntegerParam,
                     logical = makeLogicalParam,
                     discrete = makeDiscreteParam)
  vector.const = list(numeric = makeNumericVectorParam,
                     integer = makeIntegerVectorParam,
                     logical = makeLogicalVectorParam,
                     discrete = makeDiscreteVectorParam)
  normlrn.const = list(numeric = makeNumericLearnerParam,
                      integer = makeIntegerLearnerParam,
                      logical = makeLogicalLearnerParam,
                      discrete = makeDiscreteLearnerParam)
  vectlrn.const = list(numeric = makeNumericVectorLearnerParam,
                      integer = makeIntegerVectorLearnerParam,
                      logical = makeLogicalVectorLearnerParam,
                      discrete = makeDiscreteVectorLearnerParam)
    if (is.vector) {
    if (is.learner) {
      vectlrn.const[[type]]
    } else {
      vector.const[[type]]
    }
  } else {
    if (is.learner) {
      normlrn.const[[type]]
    } else {
      normal.const[[type]]
    }
  }
}

### parameter parsing sub-functions

# parseDimension: parse the '^n' part indicating a vector
parseDimension = function(pdeco, pstring, pss.env) {
  if (is.recursive(pdeco[[3]]) && identical(pdeco[[3]][[1]], quote(`[`))) { # settings
    exponent = pdeco[[3]][[2]]
    # remove '^..' part, but keep settings part
    new.pdeco = pdeco[[3]]
    new.pdeco[[2]] = pdeco[[2]]
    pdeco = new.pdeco
  } else {
    exponent = pdeco[[3]]
    pdeco = pdeco[[2]]  # remove '^...' part
  }
  len = eval(exponent, envir = pss.env)
  if (!is.numeric(len)) {
    formerr(pstring, sprintf("`^` found, but exponent %s did not eval to numeric.", deparseJoin(exponent)))
  }
  list(pdeco = pdeco, len = len)
}

# parseDiscrete: parse the range part of a discrete parameter
parseDiscrete = function(pdeco, pstring, pss.env) {
  if (length(pdeco) == 2) {
    # only one value given -> interpret it as expression that gives the values list
    return(eval(pdeco[[2]], envir = pss.env))
  } else if (all(names(pdeco) == "")) {
    # succession of names of the kind (a, b, c, 1, 2, 3) -> turn it into a list
    vallist = as.list(pdeco)
    vallist[[1]] = NULL
    vallist = lapply(vallist, function(item) {
      if (is.name(item)) {
        as.character(item)
      } else if (is.numeric(item)) {
        item
      } else {
        formerr(pstring, sprintf("value list %s invalid", deparseJoin(pdeco)))
      }
    })
    names(vallist) = sapply(vallist, as.character)
    return(vallist)
  } else {
    pdeco[[1]] = quote(list)
    return(eval(pdeco, envir = pss.env))
  }
}

# parseNumeric: parse the range part of a numeric / integer parameter
parseNumeric = function(pdeco, ptype, pstring, pss.env) {
  if (length(pdeco) != 3) {
    formerr(pstring, "invalid numeric / integer range")
  }
  quasi.inf = .Machine$double.xmax
  parse.bound = function(expr, lower) {
    if (is.recursive(expr) && identical(expr[[1]], quote(`~`))) {
      value = eval(expr[[2]], envir = pss.env)
      if (ptype == "integer") {
        value = value + ifelse(lower, 1, -1)
      } else {
        if (value == 0) {
          value = value + .Machine$double.xmin * ifelse(lower, 1, -1)
        } else {
          epsilon = ifelse(lower == (value > 0), .Machine$double.eps, -.Machine$double.neg.eps)
          value = value + epsilon * value
        }
      }
    } else if (identical(expr, quote(`.`)) ||
               (is.recursive(expr) && identical(expr[[1]], quote(`.`)))) {
      if (length(expr) > 1) {
        formerr(pstring, "invalid numeric / integer range")
      }
      if (ptype == "integer") {
        formerr(pstring, '"."-bounds (unbounded but excluding "Inf") are only allowed for "numeric" variables.')
      }
      value = ifelse(lower, -quasi.inf, quasi.inf)
    } else if (identical(expr, substitute())) {
      if (length(expr) > 1) {
        formerr(pstring, "invalid numeric / integer range")
      }
      value = ifelse(lower, -Inf, Inf)
    } else {
      value = eval(expr, envir = pss.env)
    }
  }
  lower.bound = parse.bound(pdeco[[2]], TRUE)
  upper.bound = parse.bound(pdeco[[3]], FALSE)

  allow.inf = TRUE
  if (lower.bound == -quasi.inf || upper.bound == quasi.inf) {
    if (lower.bound != -Inf && upper.bound != Inf) {
      # no true 'Inf' occurs, so we can translate quasi.inf to Inf and
      # instead use the 'allow.inf' parameter
      if (lower.bound == -quasi.inf) {
        lower.bound = -Inf
      }
      if (upper.bound == quasi.inf) {
        upper.bound = Inf
      }
      allow.inf = FALSE
    }
  }
  rl = list(lower = lower.bound, upper = upper.bound)
  if (ptype == "numeric") {
    rl$allow.inf = allow.inf
  }
  rl
}

### parsing single parameter
# this function does the heavy lifting:
# it takes the name and expression of a given parameter and returns
# the constructed ParamSet.
parseSingleParameter = function(name, thispar, is.learner, pss.env) {
  constructor.params = list()
  additional.settings = list()
  is.vector = FALSE
  pstring = deparseJoin(thispar)
  if (name != "") {
    pstring = paste(name, "=", pstring)
  }

  if (!identical(thispar[[1]], quote(`:`))) {
    formerr(pstring, "`:` was missing or at unexpected position.")
  }

  if (name == "") {  # no default
    constructor.params$id = as.character(thispar[[2]])
  } else {
    constructor.params$id = name
    constructor.params$default = eval(thispar[[2]], envir = pss.env)
  }

  pdeco = thispar[[3]]
  if (is.recursive(pdeco) && identical(pdeco[[1]], quote(`^`))) { # dimension
    rl = parseDimension(pdeco, pstring, pss.env)
    pdeco = rl$pdeco
    constructor.params$len = rl$len
    is.vector = TRUE
  }
  if (is.recursive(pdeco) && identical(pdeco[[1]], quote(`[`))) {  # settings
    additional.settings = as.list(pdeco)
    additional.settings[[1]] = NULL  # delete `[`
    additional.settings[[1]] = NULL  # delete part before `[`
    additional.settings = lapply(additional.settings, function(x) eval(x, envir = pss.env))
    pdeco = pdeco[[2]]
  }
  if (!is.recursive(pdeco)) {
    pdeco = list(pdeco)  # so we can access [[1]]
  }
  ptype = as.character(pdeco[[1]])
  if (!ptype %in% c("integer", "numeric", "logical", "discrete")) {
    formerr(pstring, sprintf("Unknown parameter type %s", deparseJoin(pdeco[[1]])))
  }

  # check sanity of 'range' part
  if (ptype == "logical" && length(pdeco) > 1) {
    formerr(pstring, sprintf("Logical parameter with unexpected postfix: %s", deparseJoin(pdeco)))
  }
  if (ptype != "logical" && length(pdeco) == 1) {
    formerr(pstring, sprintf("Parameter of type '%s' needs range postfix.", ptype))
  }

  # interpret range of discrete parameters
  if (ptype == "discrete") {
    constructor.params$values = parseDiscrete(pdeco, pstring, pss.env)
  }
  if (ptype %in% c("numeric", "integer")) {
    constructor.params = insert(constructor.params, parseNumeric(pdeco, ptype, pstring, pss.env))
  }
  constructor.params = insert(constructor.params, additional.settings)
  do.call(getConstructor(ptype, is.learner, is.vector), constructor.params)
}


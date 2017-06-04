
#' CPO Composition
#'
#' @export
`%>>%` = function(cpo1, cpo2) {
  UseMethod("%>>%")
}

#' @export
`%>>%.default` = function(cpo1, cpo2) {
  stopf("%%>>%% not defined for objects of class c(%s)", paste0('"', class(cpo1), '"', collapse=", "))
}

#' @export
`%>>%.CPOConstructor` = function(cpo1, cpo2) {
  stop("Cannot compose CPO Constructors.")
}



# deparseJoin: deparse, but work with longer than 500 char expressions, mostly.
# Note that this is a heuristic for user messages only, the result can not be
# parsed again!
deparseJoin = function(what, sep = " ") {
  collapse(deparse(what, 500), sep = sep)
}

getParamSetDefaults = function(ps) {
  lapply(ps$pars[vlapply(ps$pars, function(x) x$has.default)], function(x) x$default)
}

makeFunction = function(expr, required.arglist, env = parent.frame()) {
  if (is.recursive(expr) && identical(expr[[1]], quote(`{`))) {
    # we have a headless list of expressions
    # so we build our own function
    args = as.pairlist(required.arglist)
    newfun = eval(call("function", args, expr), envir = env)
  } else {
    newfun = eval(expr, envir = env)
    assertFunction(newfun)
    if (!"..." %in% names(formals(newfun))) {
      # with a vararg function we tentatively trust the function
      # handles its arguments

      assertFunction(newfun, args = names(required.arglist))
    }
    for (arg in names(formals(newfun))) {
      if (arg == "...") {
        # can't say anything about ellipses
        next
      }
      if (identical(formals(newfun)[[arg]], substitute())) {
        # the argument has no default, so we only care that this it will always be given
        if (!arg %in% names(required.arglist)) {
          stopf("Bad argument %s.\nNeed a function with arguments %s.", arg, collapse(names(required.arglist)), sep = ", ")
        }
        next
      }
      if (!arg %in% names(required.arglist)) {
        # the argument is not required, but it has a default; that is fine.
        next
      }
      if (!identical(formals(newfun)[[arg]], required.arglist[[arg]])) {
        if (identical(formals(newfun)[[arg]], substitute())) {
          funargstr = "no default"
          funarg = "no default"
        } else {
          funarg = eval(formals(newfun)[[arg]], envir = env)
          funargstr = sprintf("default %s", collapse(funarg))
        }
        if (identical(required.arglist[[arg]], substitute())) {
          reqarg = "no default"
        } else {
          reqarg = required.arglist[[arg]]
        }
        # there is a chance that they evaluate to the same value, so we check again
        if (!identical(funarg, reqarg) && (!length(funarg) == 1 || !length(reqarg) == 1 || !is.na(funarg) || !is.na(reqarg))) {
          stopf("Given function parameter %s has %s, but required default is %s.",
                arg, funargstr, collapse(reqarg))
        }
      }
    }
  }
  newfun
}


setCPOId = function(cpo, id) {
  if (!is.null(id)) {
    assertString(id)
  }

  pasteIdIfNN = function(names) {
    if (is.null(id)) {
      names
    } else {
      paste(id, names, sep=".")
    }
  }
  par.names = names(cpo$par.set$pars)
  bare.par.vals.names = cpo$bare.par.names[match(names(cpo$par.vals), par.names)]
  names(cpo$par.vals) = pasteIdIfNN(bare.par.vals.names)

  newparnames = pasteIdIfNN(cpo$bare.par.names)
  names(cpo$par.set$pars) = newparnames
  for (n in newparnames) {
    cpo$par.set$pars[[n]]$id = n
  }
  # FIXME: need to handle requirement changes
  cpo$id = id
  cpo$name = collapse(c(cpo$barename, id), sep=".")
  cpo
}


#' @export
print.CPO = function(x, ...) {
  argstring = paste(names(x$par.vals), sapply(x$par.vals, deparseJoin, sep="\n"), sep=" = ", collapse=", ")
  template = ifelse("CPOPrimitive" %in% class(x), "%s(%s)", "(%s)(%s)")
  catf(template, x$name, argstring)
}

#' @export
print.DetailedCPO = function(x, ...) {
  NextMethod("print", x)
  cat("\n")
  print(x$par.set)
}

#' @export
summary.CPO = function(object, ...) {
  if (!"DetailedCPO" %in% object) {
    class(object) = c(head(class(object), -1), "DetailedCPO", "CPO")
  }
  object
}

#' @export
print.CPOConstructor = function(x, ...) {
  args = formals(x)
  argvals = sapply(args, function(y) if (identical(y, substitute())) "" else paste(" =", deparseJoin(y, "\n")))
  argstring = paste(names(args), argvals, collapse=", ", sep="")
  catf("<<CPO %s(%s)>>", environment(x)$name, argstring)
}

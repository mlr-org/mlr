
#' @export
makeCPOObject = function(..., par.set = NULL, par.vals = NULL, cpo.trafo, cpo.retrafo) {
  if (is.null(par.set)) {
    par.set = paramSetSugar(..., pss.env=parent.frame())
  }
  if (is.null(par.vals)) {
    par.vals = getParamSetDefaults(par.set)
  }
  assertSubset(names(par.vals), names(par.set$pars))
  required.arglist = lapply(par.set$pars, function(dummy) substitute())
  required.arglist = insert(required.arglist, par.vals)
  required.arglist.trafo = required.arglist
  required.arglist.trafo$data = substitute()
  cpo.trafo = makeFunction(substitute(cpo.trafo), required.arglist.trafo, env = parent.frame())

  required.arglist.retrafo = required.arglist
  required.arglist.retrafo$data = substitute()
  required.arglist.retrafo$control = substitute()
  cpo.retrafo = makeFunction(substitute(cpo.retrafo), required.arglist.retrafo, env = parent.frame())
  # TODO: make the actual object
}



#' @export
makeCPOObject = function(name, ..., par.set = NULL, par.vals = NULL, cpo.trafo, cpo.retrafo) {
  assertString(name)
  if (is.null(par.set)) {
    par.set = paramSetSugar(..., pss.env = parent.frame())
  }
  reservedParams = c("data", "control", "id")
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

concatTrafoCPOObject = function(fun1, fun2) {

}

concatRetrafoCPOObject = function(fun1, fun2) {

}

#' @export
`%>>%.CPOObject` = function(cpo1, cpo2) {
  assertClass(cpo2, "CPOObject")
  samenames = intersect(names(cpo1$par.set$pars), names(cpo2$par.set$pars))
  if (length(samenames)) {
    plur = length(samenames) > 1
    stopf("Parameter%s %s occur%s in both %s and %s\n%s", ifelse(plur, "s", ""),
      paste0('"', samenames, '"', collapse=", "), ifelse(plur, "", "s"), cpo1$name, cpo2$name,
      "Use the id parameter when constructing, or setCPOId, to prevent name collisions.")
  }
  makeS3Obj(c("CPOObject", "CPO"),
    name = paste(cpo1$name, cpo2$name, sep=" >> "),
    bare.par.names = c(cpo1$bare.par.names, cpo2$bare.par.names),
    par.set = c(cpo1$par.set, cpo2$par.set),
    par.vals = c(cpo1$par.vals, cpo2$par.vals),
    trafo = concatTrafoCPOObject(cpo1$trafo, cpo2$trafo),
    retrafo = concatRetrafoCPOObject(cpo1$retrafo, cpo2$retrafo))
}


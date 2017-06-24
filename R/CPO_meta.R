
#' @title CPO Multiplexer
#'
#' @template cpo_description
#'
#' @param cpos [\code{list} of (\code{CPO} | \code{CPOConstructor})]\cr
#'   The CPOs to multiplex. If this is a named list, the
#'   names must be unique and represent the IDs that will
#'   be given to the CPOs upon construction.
#' @param selected.cpo [\code{character(1)}]\cr
#'   Selected CPO. Will default to the first item of \code{cpos}
#'   if \code{NULL}. Default is \code{NULL}.
#' @template arg_cpo_id
#' @family CPO
#' @export
cpoMultiplex = function(cpos, selected.cpo = NULL, id = NULL) {
  assertList(cpos, c("CPO", "CPOConstructor"), min.len = 1)  # FIXME: require databound
  has.names = !is.null(names(cpos))
  if (!has.names) {
    names(cpos) = sapply(cpos, function(c) {
      if ("CPOConstructor" %in% class(c)) {
        getCPOName(c)
      } else if ("CPOPrimitive" %in% class(c)) {
        coalesce(getCPOId(c), getCPOName(c))
      } else {
        c = as.list(c)[[1]]
        coalesce(getCPOId(c), getCPOName(c))
      }
    })
  }
  dupnames = unique(names(cpos)[duplicated(names(cpos))])
  if (length(dupnames)) {
    stopf("%s must be unique, but duplicates found: %s",
          ifelse(has.names, "names of parameter 'cpos'", "CPO types given"), collapse(dupnames, sep = ", "))
  }
  assertList(cpos, names = "unique")

  if (is.null(selected.cpo)) {
    selected.cpo = names(cpos)[1]
  }
  assertChoice(selected.cpo, names(cpos))

  constructed = lapply(names(cpos), function(n) {
    if ("CPOConstructor" %in% class(cpos[[n]])) {
      cpos[[n]](id = n)
    } else {
      cpos[[n]]
    }
  })
  names(constructed) = names(cpos)

  paramset.list = do.call(base::c, lapply(names(constructed), function(n) {
    ps = getParamSet(constructed[[n]])
    ps$pars = lapply(ps$pars, function(p) {
      if (is.null(p$requires)) {
        p$requires = substitute(selected.cpo == n, list(n = n))
      } else {
        p$requires = substitute((selected.cpo == n) && (otherreq), list(n = n, otherreq = p$requires))
      }
      p
    })
    ps
  }))

  paramset = c(paramSetSugar(selected.cpo = selected.cpo: discrete[names(cpos)]), paramset.list)

  pv = unlist(unname(lapply(constructed, getHyperPars)), recursive = FALSE)

  pr = collectProperties(constructed)

  makeCPO("multiplex", .par.set = paramset, .par.vals = pv, .datasplit = "task", .properties = pr$properties, .properties.adding = pr$properties.adding,
          .properties.needed = pr$properties.needed, .properties.target = pr$properties.target, cpo.trafo = function(data, target, selected.cpo, ...) {
            cpo = constructed[[selected.cpo]]
            cpo = setHyperPars(cpo, par.vals = list(...)[names(getParamSet(cpo)$pars)])
            res = data %>>% cpo
            control = retrafo(res)
            retrafo(res) = NULL
            res
          }, cpo.retrafo = function(data, control, ...) { data %>>% control })(id = id)
}
registerCPO(list(name = "cpoMultiplex", cponame = "multiplex"), "meta", NULL, "Apply one of a given set of CPOs, each having their hyperparameters exported.")

#' @title CPO Applicator
#'
#' @template cpo_description
#'
#' @param cpos [\code{list} of (\code{CPO} | \code{CPOConstructor})]\cr
#'   The CPOs to multiplex. If this is a named list, the
#'   names must be unique and represent the IDs that will
#'   be given to the CPOs upon construction.
#' @param selected.cpo [\code{character(1)}]\cr
#'   Selected CPO. Will default to the first item of \code{cpos}
#'   if \code{NULL}. Default is \code{NULL}.
#' @template arg_cpo_id
#' @family CPO
#' @export
cpoApply = makeCPO("apply", .par.set = makeParamSet(makeUntypedLearnerParam("cpo")), .datasplit = "task",  # nolint
                   cpo.trafo = { control = retrafo({res = data %>>% cpo}) ; res }, cpo.retrafo = { data %>>% control })
# FIXME: require databound
registerCPO(cpoApply, "meta", NULL, "Apply a freely chosen CPOs, without exporting its hyperparameters.")



#' @title Build data-dependent CPOs
#'
#' @description
#' The Meta-CPO determines what CPO to apply to a data depending on
#' a provided function
#'
#' @export
cpoMeta = function(..., .cpo.name = "meta", .par.set = NULL, .par.vals = list(), .export = list(),
                   .datasplit = c("target", "most", "all", "no", "task", "factor", "onlyfactor", "ordered", "numeric"),
                   .properties = NULL, .properties.adding = NULL, .properties.needed = NULL,
                   .properties.target = NULL, cpo.build) {
  .datasplit = match.arg(.datasplit)
  if (is.null(names(.export))) {
    names(.export) = sapply(.export, function(c) {
      if ("CPOConstructor" %in% class(c)) {
        stopf("If .export has no names, all CPOs must be constructed. %s is not.",
          getCPOName(c))
      } else if ("CPOPrimitive" %in% class(c)) {
        id = getCPOId(c)
        if (is.null(id)) {
          stopf("If .export has no names, all CPOs must have (unique) IDs.")
        }
        id
      } else {
        stopf("If .export has no names, compound CPOs can't be given.")
      }
    })
  }
  assertList(.export, types = c("CPO", "CPOConstructor"), names = "unique")  # FIXME: target bound separately

  constructed = lapply(names(.export), function(n) {
    if ("CPOConstructor" %in% class(.export[[n]])) {
      .export[[n]](id = n)
    } else {
      .export[[n]]
    }
  })
  names(constructed) = names(.export)

  if (is.null(.par.set)) {
    .par.set = paramSetSugar(..., .pss.env = parent.frame())
  }

  paramset.pass.on = .par.set
  pv.pass.on = insert(getParamSetDefaults(paramset.pass.on), .par.vals)
  if (length(paramset.pass.on$pars)) {
    assertSubset(names(pv.pass.on), names(paramset.pass.on$pars))
  } else {
    assert(length(pv.pass.on) == 0)
  }
  name.collision = intersect(names(paramset.pass.on$pars), names(.export))
  if (length(name.collision)) {
    stopf("Names of cpo.build function arguments and .export elements clash: %s", collapse(name.collision, sep = ", "))
  }

  paramset.others = do.call(base::c, lapply(names(constructed), function(n) getParamSet(constructed[[n]])))
  pv.others = unlist(unname(lapply(constructed, getHyperPars)), recursive = FALSE)


  pr = collectProperties(constructed)
  .properties = coalesce(.properties.adding, pr$properties, (cpo.dataproperties))
  .properties.adding = coalesce(.properties.adding, pr$properties.adding, character(0))
  .properties.needed = coalesce(.properties.needed, pr$properties.needed, character(0))
  .properties.target = coalesce(.properties.target, pr$properties.target, c(cpo.targetproperties, cpo.tasktypes))

  required.arglist = lapply(c(paramset.pass.on$pars, .export), function(dummy) substitute())
  required.arglist = insert(required.arglist, pv.pass.on)
  required.arglist$data = substitute()
  required.arglist$target = substitute()

  buildfun = makeFunction(substitute(cpo.build), required.arglist, env = parent.frame())

  fullaffect = list(type = c("numeric", "factor", "ordered", "other"),
    index = integer(0), names = character(0), pattern = NULL, invert = FALSE, pattern.ignore.case = FALSE,
    pattern.perl = FALSE, pattern.fixed = FALSE)

  makeCPO(.cpo.name, .par.set = c(paramset.pass.on, paramset.others), .par.vals = c(pv.pass.on, pv.others),
    .datasplit = "task", .properties = .properties, .properties.adding = .properties.adding,
    .properties.needed = .properties.needed, .properties.target = .properties.target,
    cpo.trafo = function(data, target, ...) {
      args = list(...)
      buildfunargs = c(args[names(paramset.pass.on$pars)], lapply(.export, function(cpo)
        setHyperPars(cpo, par.vals = args[names(getParamSet(cpo)$pars)])))
      tin = prepareTrafoInput(data, .datasplit, c(.properties, .properties.target, cpo.predict.properties), fullaffect, FALSE, .cpo.name)

      cpo = do.call(buildfun, insert(buildfunargs, tin$indata))
      result = data %>>% cpo
      control = retrafo(result)
      retrafo(result) = NULL
      result
    },
    cpo.retrafo = function(data, control, ...) {
      data %>>% control
    })
}

# intersect: properties get intersected instead of union'd
collectProperties = function(constructed, do.intersect = FALSE) {
  oc = constructed
  constructed = Filter(function(x) !is.nullcpo(x), constructed)
  allprops = lapply(constructed, getCPOProperties, only.data = TRUE)
  all.allprops = lapply(constructed, function(x) setdiff(getCPOProperties(x)$properties, c(cpo.dataproperties, cpo.predict.properties)))

  if (do.intersect) {
    un = union
    isct = intersect
    union.first = character(0)
    isct.first = cpo.dataproperties
    isct.first.adding = if (length(oc) > length(constructed)) character(0) else isct.first
    isct.first.target = c(cpo.tasktypes, cpo.targetproperties)
  } else {
    un = intersect
    isct = union
    union.first = cpo.dataproperties
    isct.first = character(0)
    isct.first.target = character(0)
  }
  list(
    properties = Reduce(isct, extractSubList(allprops, "properties", simplify = FALSE), isct.first),
    properties.needed = Reduce(un, extractSubList(allprops, "properties.needed", simplify = FALSE), union.first),
    properties.adding = Reduce(isct, extractSubList(allprops, "properties.adding", simplify = FALSE), isct.first.adding),
    properties.target = Reduce(isct, all.allprops, isct.first.target))
}

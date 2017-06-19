
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

  allprops = lapply(constructed, getCPOProperties, only.data = TRUE)
  properties = Reduce(union, extractSubList(allprops, "properties", simplify = FALSE))
  properties.needed = Reduce(intersect, extractSubList(allprops, "properties.needed", simplify = FALSE))
  properties.adding = Reduce(union, extractSubList(allprops, "properties.adding", simplify = FALSE))

  all.allprops = lapply(constructed, function(x) setdiff(getCPOProperties(x)$properties, c(cpo.dataproperties, cpo.predict.properties)))
  properties.target = Reduce(union, all.allprops)

  makeCPO("multiplex", .par.set = paramset, .par.vals = pv, .datasplit = "task", .properties = properties, .properties.adding = properties.adding,
          .properties.needed = properties.needed, .properties.target = properties.target, cpo.trafo = function(data, target, selected.cpo, ...) {
            cpo = constructed[[selected.cpo]]
            cpo = setHyperPars(cpo, par.vals = list(...)[names(getParamSet(cpo)$pars)])
            res = data %>>% cpo
            control = retrafo(res)
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
cpoMeta = function(..., .cpo.name = "meta", .par.set = NULL, .par.vals = list(), .export = list(),
                   .properties = NULL, .properties.adding = NULL, .properties.needed = NULL,
                   .properties.target = NULL, cpo.build) {
  .datasplit = match.arg(.datasplit)
  if (!is.null(names(.export))) {
    names(cpos) = sapply(cpos, function(c) {
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

  assertList(.export, types = "CPO", names = "unique")  # FIXME: target bound separately

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

  pass.on.par.set = .par.set

  paramset.others = do.call(base::c, lapply(names(constructed), function(n) getParamSet(constructed[[n]])))

  allprops = lapply(constructed, getCPOProperties, only.data = TRUE)
  .properties = coalesce(.properties, Reduce(union, extractSubList(allprops, "properties", simplify = FALSE)))
  .properties.needed = coalesce(.properties.needed, Reduce(intersect, extractSubList(allprops, "properties.needed", simplify = FALSE)))
  .properties.adding = coalesce(.properties.adding, Reduce(union, extractSubList(allprops, "properties.adding", simplify = FALSE)))

  makeCPO(.cpo.name, .par.set = .par.set, .par.vals = .par.vals,
    .datasplit = "task", .properties = .properties, .properties.adding = .properties.adding,
    .properties.needed = .properties.needed, .properties.target = .properties.target,
    cpo.trafo = function() { },
    cpo.retrafo = function() { })
}


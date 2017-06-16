#' @include ParamSetSugar.R

#' @title Construct a CPO for PCA preprocessing
#'
#' @template cpo_description
#'
#' @param center [\code{logical(1)}]\cr
#'   Whether to center the data before performing PCA.
#'   Default is \code{TRUE}.
#' @param scale [\code{logical(1)}]\cr
#'   Whether to scale the data before performing PCA. The centering / scaling algorithm
#'   of R's dQuote{scale} is used.
#'   Default is \code{TRUE}.
#' @template arg_cpo_id
#' @family CPO
#' @export
cpoPca = makeCPO("pca", center = TRUE: logical, scale = TRUE: logical, .datasplit = "numeric", cpo.trafo = {  # nolint
  pcr = prcomp(as.matrix(data), center = center, scale. = scale)
  data = as.data.frame(pcr$x)
  control = list(rotation = pcr$rotation, center = pcr$center, scale = pcr$scale)
  data
}, cpo.retrafo = {
  as.data.frame(scale(as.matrix(data), center = control$center, scale = control$scale) %*% control$rotation)
})

#' @title Construct a CPO for scaling / centering
#'
#' @template cpo_description
#'
#' @param center [\code{logical(1)}]\cr
#'   Whether to center the data.
#'   Default is \code{TRUE}.
#' @param scale [\code{logical(1)}]\cr
#'   Whether to scale the data.
#'   Default is \code{TRUE}.
#' @template arg_cpo_id
#' @family CPO
#' @export
cpoScale = makeCPO("scale", center = TRUE: logical, scale = TRUE: logical, .datasplit = "numeric", cpo.trafo = {  # nolint
  result = scale(as.matrix(data), center = center, scale = scale)
  data[] = result
  control = list(center = attr(result, "scaled:center"), scale = attr(result, "scaled:scale"))
  data
}, cpo.retrafo = {
  as.data.frame(scale(as.matrix(data), center = control$center, scale = control$scale))
})

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
  assertList(cpos, c("CPO", "CPOConstructor"), min.len = 1)
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

  allprops = lapply(constructed, getCPOProperties, only.data = TRUE)
  properties = Reduce(intersect, extractSubList(allprops, "properties", simplify = FALSE))
  properties.needed = Reduce(intersect, extractSubList(allprops, "properties.needed", simplify = FALSE))
  properties.adding = Reduce(union, extractSubList(allprops, "properties.adding", simplify = FALSE))

  makeCPO("multiplex", .par.set = paramset, .datasplit = "task", .properties = properties, .properties.adding = properties.adding,
          .properties.needed = properties.needed, cpo.trafo = function(data, target, selected.cpo, ...) {
            cpo = constructed[[selected.cpo]]
            cpo = setHyperPars(cpo, par.vals = list(...)[names(getParamSet(cpo)$pars)])
            res = data %>>% cpo
            control = retrafo(res)
            res
          }, cpo.retrafo = function(data, control, ...) { data %>>% control })(id = id)
}


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



#' @title Drop All Columns Except Certain Selected Ones from Data
#'
#' @description
#' Select columns by type or name. The parameters \dQuote{type} and
#' \dQuote{pattern} are additive; if both are given, all column that match
#' either will be returned.
#'
#' @template cpo_description
#'
#' @param type [\code{character}]\cr
#'   One or more out of \dQuote{numeric}, \dQuote{ordered}, \dQuote{factor}, \dQuote{other}.
#'   The type of columns to keep. Default is \code{character(0)}.
#' @param pattern [\code{character(1)}]\cr
#'   A pattern to match against the column names. Same as in \code{\link{grep}}.
#'   Default is \code{NULL} for no matching.
#' @param ignore.case [\code{logical(1)}]\cr
#'   Whether to perform case insensitive matching. Same as in \code{\link{grep}}.
#'   Default is \code{FALSE}.
#' @param perl [\code{logical(1)}]\cr
#'   Should Perl-compatible regexps be used? Same as in \code{\link{grep}}.
#'   Default is \code{FALSE}.
#' @param fixed [\code{logical(1)}]\cr
#'   Whether to use match \code{pattern} as as is. Same as in \code{\link{grep}}.
#'   Default is \code{FALSE}.
cpoSelect = makeCPO("select",  # nolint
  .par.set = c(
      paramSetSugar(type = list(): discrete[numeric, ordered, factor, other]^NA),
      makeParamSet(makeCharacterParam("pattern", NULL, special.vals = list(NULL))),
      paramSetSugar(
          ignore.case = FALSE: logical, perl = FALSE: logical,
          fixed = FALSE: logical)),
  .datasplit = "target", cpo.trafo = {
    coltypes = vcapply(data, function(x) class(x)[1])
    coltypes[coltypes == "integer"] = "numeric"
    coltypes[!coltypes %in% c("numeric", "factor", "ordered")] = "other"
    matchcols = coltypes %in% type
    if (!is.null(pattern)) {
      matchcols = grepl(pattern, colnames(data), ignore.case, perl, fixed)
    }
    cpo.retrafo = function(data) {
      data[matchcols]
    }
    cpo.retrafo(data)
  }, cpo.retrafo = NULL)






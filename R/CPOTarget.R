
##################################
### Creator                    ###
##################################

#' @title Create a custom target-bound CPO constructor
#'
#' @description
#' Create a CPO constructor.
#'
#' Most of the parameters are as in \link{makeCPO}.
#'
#' @family CPO
#' @export
makeCPOTargetOp = function(.cpo.name, ..., .par.set = NULL, .par.vals = list(),
                           .datasplit = c("target", "most", "all", "no", "task", "factor", "onlyfactor", "ordered", "numeric"),
                           .data.dependent = TRUE,
                           .properties = character(0),
                           .properties.adding = character(0), .properties.needed = character(0),
                           .properties.data = c("numerics", "factors", "ordered", "missings"),
                           .type = c("cluster", "classif", "multilabel", "regr", "surv"),
                           .type.out = .type,
                           cpo.trafo, cpo.retrafo) {

  .type = match.arg(.type)
  .type.out = match.arg(.type.out, choices = c("cluster", "classif", "multilabel", "regr", "surv"))

  possible.properties = list(multilabel = character(0), regr = character(0), cluster = character(0),
      classif = c("oneclass", "twoclass", "multiclass"), surv = c("lcens", "rcens", "icens"))

  .datasplit = match.arg(.datasplit)

  assertFlag(.data.dependent)

  if (!.data.dependent) {
    if (.datasplit %in% c("no", "task")) {
      stop("When .data.dependent is FALSE, .datasplit must not be 'no' or 'task'")
    }
    if (!setequal(.properties.data, c("numerics", "factors", "ordered", "missings"))) {
      stop("When .data.dependent is FALSE, .properties.data must have the default value.")
    }
  }

  if (length(possible.properties[[.type]])) {
    assertSubset(.properties, possible.properties[[.type]])
    if (.type.out != .type && length(setdiff(.properties, .properties.adding))) {
      stopf("For conversion away from %s, .properties.adding must equal .properties.", .type)
    }
  } else if (length(.properties)) {
    stopf("CPO handling type %s must have empty properties.", .type)
  }

  if (length(possible.properties[[.type.out]])) {
    if (.type.out == "surv" && .type != "surv" && length(.properties.needed) != 1) {
      stop("For conversion to 'surv', there must be exactly one '.properties.needed' argument given.")
    }
    assertSubset(.properties.needed, possible.properties[[.type.out]])
  } else if (length(.properties.needed)) {
    stopf("Output type is %s, so .properties.needed must be empty.", .type.out)
  }


  eval.parent(substitute(makeCPOGeneral(.cpotype = "targetbound",
    .cpo.name = .cpo.name, .par.set = .par.set, .par.vals = .par.vals,
    .datasplit = .datasplit, .data.dependent = .data.dependent, .properties = .properties,
    .properties.adding = .properties.adding, .properties.needed = .properties.needed,
    .properties.target = .properties.data, .type.from = .type, .type.to = .type.out,
    cpo.trafo = cpo.trafo, cpo.retrafo = cpo.retrafo)))
}



# getLearnerProperties: 'type' adden
# nicht ueber missing 'control' beschweren wenn kein 'control 'referenziert
# always remove 'weights'


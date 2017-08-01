
##################################
### Creator                    ###
##################################

#' @title Create a custom Target Operation CPO Constructor
#'
#' @rdname makeCPO
#' @export
makeCPOTargetOp = function(.cpo.name, ..., .par.set = NULL, .par.vals = list(),
                           .datasplit = c("target", "most", "all", "no", "task", "factor", "onlyfactor", "ordered", "numeric"),
                           .data.dependent = TRUE, .retrafo.format = c("separate", "combined", "stateless"),
                           .export.params = TRUE,
                           .properties.data = c("numerics", "factors", "ordered", "missings"),
                           .properties.adding = character(0), .properties.needed = character(0),
                           .properties.target = character(0),
                           .type = c("cluster", "classif", "multilabel", "regr", "surv"),
                           .type.out = .type,
                           .predict.type = c(response = "response"),
                           .packages = character(0),
                           cpo.trafo, cpo.retrafo) {

  .type = match.arg(.type)
  .retrafo.format = match.arg(.retrafo.format)
  .type.out = match.arg(.type.out, choices = c("cluster", "classif", "multilabel", "regr", "surv"))

  possible.properties = list(multilabel = character(0), regr = character(0), cluster = character(0),
      classif = c("oneclass", "twoclass", "multiclass"))

  .datasplit = match.arg(.datasplit)

  assertFlag(.data.dependent)

  if (!.data.dependent) {
    if (.datasplit %in% c("no", "task")) {
      stop("When .data.dependent is FALSE, .datasplit must not be 'no' or 'task'")
    }
    if (!setequal(.properties, c("numerics", "factors", "ordered", "missings"))) {
      stop("When .data.dependent is FALSE, .properties must have the default value.")
    }
  }

  if (length(possible.properties[[.type]])) {
    assertSubset(.properties.target, possible.properties[[.type]])
    if (.type.out != .type && length(setdiff(.properties.target, .properties.adding))) {
      stopf("For conversion away from %s, .properties.adding must equal .properties.", .type)
    }
  } else if (length(.properties.target)) {
    stopf("CPO handling type %s must have empty properties.", .type)
  }

  if (length(possible.properties[[.type.out]])) {
    assertSubset(.properties.needed, possible.properties[[.type.out]])
  } else if (length(.properties.needed)) {
    stopf("Output type is %s, so .properties.needed must be empty.", .type.out)
  }

  predtypes = list(classif = c("response", "prob"), regr = c("response", "se"),
    cluster = c("response", "prob"), multilabel = c("response", "prob"),
    surv = c("response", "prob"))

  if (is.list(.predict.type)) {
    .predict.type = sapply(.predict.type, identity)
  }
  if (!isTRUE(checkCharacter(.predict.type, any.missing = FALSE, min.len = 1, names = "unique"))) {
    stop(".predict.type argument is not, and could not be converted into, a uniquely named character vector.")
  }
  if (!isTRUE(checkSubset(names(.predict.type), predtypes[[.type]]))) {
    stop("names of .predict.type must be a subset of the possible prediction types %s of input Task type %s.", predtypes[[.type]], .type)
  }
  if (!isTRUE(checkSubset(.predict.type, predtypes[[.type.out]]))) {
    stop(".predict.type values must be a subset of the possible prediction types %s of output Task type %s.", predtypes[[.type.out]], .type.out)
  }

  if (!"response" %in% names(.predict.type)) {
    stop("CPO must always support predict.type 'response', so .predict.type must have one value named 'response'.")
  }

  if (.predict.type["response"] != "response") {
    # the lower learner must provide what we need for 'response' prediction.
    # alternatively, we could drop the requirement that every learner / CPO must always be able to deliver response.
    .properties.needed = c(.properties.needed, unname(.predict.type["response"]))
  }

  .properties.adding = c(.properties.adding, setdiff(names(.predict.type), c("response", unname(.predict.type))))
  .properties.target = c(.properties.target, setdiff(names(.predict.type), "response"))

  makeCPOGeneral(.cpotype = "targetbound",
    .cpo.name = .cpo.name, .par.set = .par.set, .par.vals = .par.vals,
    .datasplit = .datasplit, .fix.factors = FALSE, .data.dependent = .data.dependent,
    .retrafo.format = .retrafo.format, .export.params = .export.params, .properties = .properties.target,
    .properties.adding = .properties.adding, .properties.needed = .properties.needed,
    .properties.target = .properties, .type.from = .type, .type.to = .type.out,
    .predict.type = .predict.type, .packages = .packages,
    cpo.trafo = substitute(cpo.trafo), cpo.retrafo = substitute(cpo.retrafo), ...)
}



# INVERTER main function
# - errors out if not the right kind
# - does superficial test whether the input format is compatible with what to expect for the task type
# - applies the re-transformation
# - check result for plausibility
invertCPO.CPORetrafo = function(inverter, prediction, predict.type) {
  assertString(predict.type)
  cpo = inverter$cpo
  if ("inverter" %in% inverter$kind) {
    # make sure some things that should always be true are actually true
    assertString(cpo$convertfrom)
    assertString(cpo$convertto)
    assert(!"retrafo" %in% inverter$kind || cpo$stateless)  # for data caching inverters, no hybrids are created
    assert(("retrafo" %in% inverter$kind) == is.null(inverter$inverter.indata))

    if (!predict.type %in% names(inverter$predict.type)) {
      stop("Inverter %s cannot convert to requested predict.type %s", getCPOName(inverter), predict.type)
    }
    input.predict.type = inverter$predict.type[predict.type]
    assertString(input.predict.type)

    output.predict.type = ifelse(is.null(inverter$prev.retrafo), predict.type, inverter$prev.retrafo$predict.type[predict.type])
    assertString(output.predict.type)
    assertSubset(output.predict.type, names(cpo$predict.type))
    assert(cpo$predict.type[output.predict.type] == input.predict.type)

    prediction = validateSupposedPredictionFormat(prediction, cpo$convertto, input.predict.type, predict.type, "input", inverter)
    args = list(target = prediction, predict.type = output.predict.type)
    assertChoice(cpo$type, c("functional", "object"))
    if (cpo$type == "functional") {
      result = do.call(cpo$state, args)
    } else {
      args = insert(args, cpo$par.vals)
      if (!cpo$stateless) {
        args$control = inverter$state
      }
      result = do.call(cpo$retrafo, args)
    }


    result = sanitizePrediction(result)
    result = validateSupposedPredictionFormat(result, cpo$convertfrom, output.predict.type, predict.type, "output", inverter)
  }
  if (is.null(inverter$prev.retrafo)) {
    return(list(new.prediction = prediction, new.td = inverter$indatatd, new.truth = inverter$truth))
  }
  invertCPO(inverter$prev.retrafo, prediction, predict.type)
}

# type: the Task type that the prediction should conform to
# predict.type: what predict.type should prediction conform to?
# ultimate.predict.type: for output: what is the ultimate type we want?
validateSupposedPredictionFormat = function(prediction, type, predict.type, ultimate.predict.type, direction = c("input", "output"), inverter) {
  direction = match.arg(direction)
  name = inverter$cpo$bare.name
  if (!type %in% inferPredictionTypePossibilities(prediction)) {
    # data format in 'prediction' is not compatible with what this CPO is supposed to have converted to
    stopf("Prediction %s of CPO Inverter %s is not compatible with supposed type %s",
      direction, name, type)
  }
  if (!predict.type %in% getPredResponseType(prediction, type)) {
    if (direction == "input") {
      stopf("To make a %s prediction, %s needs input of predict.type %s %s, but input seems incompatible with this.",
        ultimate.predict.type, getCPOName(inverter), type, predict.type)
    } else {
      stopf("Return of %s inverter did not conform with necessary predict.type %s.", name, predict.type)
    }
  }

  if (predict.type == "prob" && !is.matrix(prediction)) {
    assert(is.atomic(prediction))  # we should have filtered out data frames before this.
    if (type %in% c("classif", "cluster")) {  # if there is one cluster / one class, convert to a 1-D matrix
      prediction = matrix(prediction, ncol = 1)
    } else {
      stop("%s of inverter %s was not a matrix even though supposed prediction type is %s prob.", stri_trans_totitle(direction), name, type)
    }
  }
  prediction
}


# get one el from each row of a matrix, given indices or col names (factors for colnames are converted to characters)
getRowEls = function(mat, inds) {
  if (is.factor(inds)) {
    inds = as.character(inds)
  }
  if (is.character(inds)) {
    inds = match(inds, colnames(mat))
  }
  inds = cbind(seq_row(mat), inds)
  mat[inds]
}

# get one el from each col of a matrix, given indices or row names
getColEls = function(mat, inds) {
  getRowEls(t(mat), inds)
}

# Do fuzzy string matching between input and a set of valid inputs
# and return the most similar valid inputs.
getNameProposals = function(input, possible.inputs, nproposals = 3L) {

  assertString(input)
  assertCharacter(possible.inputs)
  assertInt(nproposals, lower = 1L)

  # compute the approximate string distance (using the generalized Levenshtein / edit distance)
  # and get the nproposals most similar valid inputs.
  indices = order(adist(input, possible.inputs))[1:nproposals]
  possibles = na.omit(possible.inputs[indices])
  return(possibles)
}

# shorter way of printing debug dumps
#' @export
print.mlr.dump = function(x, ...) {
  cat("<debug dump>\n")
  invisible(NULL)
}


# applys the appropriate getPrediction* helper function
getPrediction = function(object, newdata, ...) {
  pred = do.call("predict", c(list("object" = object, "newdata" = newdata), list(...)))
  point = switch(object$task.desc$type,
    "regr" = getPredictionResponse(pred),
    "surv" = getPredictionResponse(pred),
    "classif" = if (object$learner$predict.type == "response") {
      getPredictionResponse(pred)
    } else {
      getPredictionProbabilities(pred)
    })

  if (object$learner$predict.type == "se") {
    cbind("preds" = point, "se" = getPredictionSE(pred))
  } else {
    point
  }
}

# replacement for purrr::imap()
imap = function(.x, .f) {
  Map(.f, .x = .x, .y = seq_along(.x))
}

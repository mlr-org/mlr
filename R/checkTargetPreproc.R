# helper function that does a few saniyt checks on target columns in preproc functions
# we use this in several preprocessing functions like impute, capLargeValues
# as we need to know what objects we are dealing with after suer input, we also do some
# basic assertions here

# obj: task or df
# target: only given for df
# cols: character vec for columns we operate on, or NULL if no specific ols are requested
checkTargetPreproc = function(obj, target, cols) {
  assert(checkClass(obj, "data.frame"), checkClass(obj, "Task"))
  assertCharacter(target, any.missing = FALSE)
  if (!is.null(cols)) {
    assertCharacter(cols, any.missing = FALSE)
  }

  if (inherits(obj, "data.frame")) {
    # if we habe a target, check that it exists in df 'obj'
    if (length(target > 0L)) {
      not.ok = which.first(target %nin% names(obj))
      if (length(not.ok) != 0L) {
        stopf("Target column '%s' must be present in data", target[not.ok])
      }
    }
  } else { # if we have a Task
    # check that user does not pass target for Task
    if (length(target) > 0L) {
      stop("Don't provide target names if you pass a task!")
    }
  }

  # if we habe a target, check that the user does not request preprocessing on it
  if (length(target > 0L) && !is.null(cols)) {
    not.ok = which.first(target %in% cols)
    if (length(not.ok) > 0L) {
      stopf("Preprocessing of target column '%s' not possible", target[not.ok])
    }
  }
}

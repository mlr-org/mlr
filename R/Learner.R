# this is an INTERNAL attract base constructor, that should be called at the end,
# when a learner object is constructed.
# in contains a minimal number of member vars, that every Learner object should have
# derived constructors can of course add more member vars
makeLearnerBaseConstructor = function(classes, id, type, package, properties, par.set, par.vals, predict.type, cache = FALSE) {
  if (length(par.vals) == 0L) {
    names(par.vals) = character(0L)
  }

  learner = makeS3Obj(c(classes, "Learner"),
    id = id,
    type = type,
    package = package,
    properties = unique(properties),
    par.set = par.set,
    par.vals = par.vals,
    predict.type = predict.type,
    cache = cache
  )
  return(learner)
}


#' @export
print.Learner = function(x, ...) {
  cat(
    "Learner ", x$id, " from package ", collapse(cleanupPackageNames(x$package)), "\n",
    "Type: ", x$type, "\n",
    "Name: ", x$name, "; Short name: ", x$short.name, "\n",
    "Class: ", class(x)[1L], "\n",
    "Properties: ", collapse(getLearnerProperties(x)), "\n",
    "Predict-Type: ", x$predict.type, "\n",
    "Hyperparameters: ", getHyperParsString(x, show.missing.values = TRUE), "\n\n",
    sep = ""
  )
}

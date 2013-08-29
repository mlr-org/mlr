# Generates MBO task.
#
# @param design [\code{\link[ParamHelpers]{ParamSet}}]\cr
#   Initial design.
# @param y.name [\code{character(1)}]\cr
#   Name of y-column for target values in optimization path.
# @return [\code{\link[mlr]{SupervisedTask}]:
#   List of repaired points.
makeMBOTask = function(design, y.name, control) {
  design$dob = design$eol = NULL
  if (any(sapply(design, is.integer)))
    design = as.data.frame(lapply(design, function(x) if(is.integer(x)) as.numeric(x) else x))
  if (any(sapply(design, is.logical)))
    design = as.data.frame(lapply(design, function(x) if(is.logical(x)) as.factor(x) else x))
  design = refactorNAs(design)
  #if (control$rank.trafo)
  #  design[,y.name] = rank(design[,y.name])
  makeRegrTask(target=y.name, data=design)
}

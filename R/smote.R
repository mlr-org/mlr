# FIXME: add paper reference

#' @title Synthetic Minority Oversampling Technique to handle class imbalancy in binary classification.
#'
#' @description
#' In each iteration, samples one minority class element x1, then one of x1's nearest neighbors: x2.
#' Both points are now interpolated / convex-combined, resulting in a new virtual data point x3
#' for the minority class.
#'
#' The method handles factor features, too. The gower distance is used for nearest neighbor
#' calculation, see \code{\link[cluster]{daisy}}.
#' For interpolation, the new factor level for x3
#' is sampled from the two given levels of x1 and x2 per feature.
#'
#' @template arg_task
#' @param rate [\code{numeric(1)}]\cr
#'   Factor to upsample the smaller class.
#'   Must be between 1 and \code{Inf},
#'   where 1 means no oversampling and 2 would mean doubling the class size.
#' @param nn [\code{integer(1)}]\cr
#'   Number of nearest neighbors to consider.
#'   Default is 5.
#' @template ret_task
#' @family imbalancy
#' @export
#' @useDynLib mlr c_smote
smote = function(task, rate, nn = 5L) {
  checkTask(task, binary = TRUE)
  assertNumber(rate, lower = 1)
  nn = asInt(nn, lower = 1L)

  requirePackages("cluster", why = "smote")
  # check for changeData later
  if (!is.null(task$weights))
    stopf("SMOTE cannot be used with weights in task!")

  # shortcuts
  data = getTaskData(task)
  target = task$task.desc$target
  y = data[, target]
  x = dropNamed(data, target)
  z = getMinMaxClass(y)
  if (z$min.size < nn)
    stopf("You cannot set nn = %i, when the minimal class has size %i!", nn, z$min.size)
  x.min = x[z$min.inds, , drop = FALSE]
  n.min = nrow(x.min)
  n.new  = round(rate * n.min) - n.min
  if (n.new <= 0L)
    return(task)
  is.num = sapply(x, is.numeric)
  res = matrix(0, n.new, ncol(x))
  # convert xmin to matrix, so we can handle it better in C
  # factors are integer levels
  x.min.matrix = x.min
  if (any(!is.num)) {
    for (i in 1:ncol(x.min.matrix)) {
      if (!is.num[i])
        x.min.matrix[, i] = as.numeric(as.integer(x.min.matrix[, i]))
    }
  }
  x.min.matrix = as.matrix(x.min.matrix)
  # dist matrix on smaller class, diag = 0 so we dont find x as neighnor of x
  minclass.dist = as.matrix(daisy(x.min))
  diag(minclass.dist) = NA
  # get n nearest neighbors, we have an index matrix now
  # nearneigh[7, 3] is 3rd nearest neighbor of observation 7
  nearneigh = t(apply(minclass.dist, 1, order))
  nearneigh = nearneigh[, 1:nn, drop = FALSE]
  res = .Call(c_smote, x.min.matrix, is.num, nearneigh, res)
  res = as.data.frame(res)

  # convert ints back to factors
  if (any(!is.num)) {
    for (i in 1:ncol(res)) {
      if (!is.num[i])
        res[, i] = as.factor(as.integer(res[, i]))
      levels(res[, i]) = levels(x[, i])
    }
  }

  colnames(res) = colnames(x)
  res[[target]] = z$min.name
  data2 = rbind(data, res)
  # we can neither allow costssens (!= classif anyway nor weights)
  changeData(task, data2)
}

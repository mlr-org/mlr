# FIXME add paper reference

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
smote = function(task, rate, nn = 5) {
  checkTask2(task, binary = TRUE)
  checkArg(rate, "numeric", len = 1L, lower = 1)
  nn = convertInteger(nn)
  checkArg(nn, "integer", len = 1L, lower = 1L)

  requirePackages("cluster", why = "smote")
  # check for changeData later
  if (!is.null(task$weights))
    stopf("Smote cannot be used with weights in task!")

  # shortcuts
  data = getTaskData(task)
  target = task$task.desc$target
  y = data[, target]
  x = dropNamed(data, target)
  z = getMinMaxClass(y)
  x.min = x[z$min.inds, ]
  x.max = x[z$max.inds, ]
  n.min = nrow(x.min)
  n.new  = round(rate * n.min) - n.min
  row1 = x[1L, ]
  is.num = sapply(row1, is.numeric)
  is.fac = sapply(row1, is.factor)
  has.num = any(is.num)
  has.fac = any(is.fac)
  p.fac = sum(is.fac)
  res = Reduce(rbind, replicate(n.new, row1, FALSE))

  # dist matrix on smaller class, diag = 0 so we dont find x as neighnor of x
  minclass.dist = as.matrix(daisy(x.min))
  diag(minclass.dist) = NA
  # get n nearest neighbors, we have an index matrix now
  # nearneigh[7, 3] is 3rd nearest neighbor of observation 7
  nearneigh = apply(minclass.dist, 1, order)
  nearneigh = nearneigh[, 1:nn, drop = FALSE]

  for (i in 1:n.new) {
    # select a random minority obs
    j.sel = sample(1:n.min, 1L)
    x1 = x.min[j.sel, ]
    # select a random neighbor
    j.nn = nearneigh[j.sel, sample(1:nn, 1L)]
    x2 = x.min[j.nn, ]
    # do convex combination for numerics
    p = runif(1)
    if (has.num) {
      res[i, is.num] = p * x1[is.num] + (1 - p) * x2[is.num]
    }
    # sample level for factors from x0 and x1
    if (has.fac) {
      # concat factors, then index either 1st or 2nd half for each position
      x12 = c(x1[is.fac], x2[is.fac])
      j = 1:p.fac + sample(c(0, p.fac), p.fac, replace = TRUE)
      res[i, col.fac] = x12[j]
    }
  }
  res[[target]] = z$min.name
  data2 = rbind(data, res)
  # we can neither allow costssens (!= classif anyway nor weights)
  changeData(task, data2)
}





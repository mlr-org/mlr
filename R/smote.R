#' @title Synthetic Minority Oversampling Technique to handle class imbalancy in binary classification.
#'
#' @description
#' In each iteration, samples one minority class element x1, then one of x1's nearest neighbors: x2.
#' Both points are now interpolated / convex-combined, resulting in a new virtual data point x3
#' for the minority class.
#'
#' The method handles factor features, too. The gower distance is used for nearest neighbor
#' calculation, see [cluster::daisy].
#' For interpolation, the new factor level for x3
#' is sampled from the two given levels of x1 and x2 per feature.
#'
#' @template arg_task
#' @param rate (`numeric(1)`)\cr
#'   Factor to upsample the smaller class.
#'   Must be between 1 and `Inf`,
#'   where 1 means no oversampling and 2 would mean doubling the class size.
#' @param nn (`integer(1)`)\cr
#'   Number of nearest neighbors to consider.
#'   Default is 5.
#' @param standardize (`integer(1)`)\cr
#'   Standardize input variables before calculating the nearest neighbors
#'   for data sets with numeric input variables only. For mixed variables
#'   (numeric and factor) the gower distance is used and variables are
#'   standardized anyway.
#'   Default is `TRUE`.
#' @param alt.logic (`integer(1)`)\cr
#'   Use an alternative logic for selection of minority class observations.
#'   Instead of sampling a minority class element AND one of its nearest
#'   neighbors, each minority class element is taken multiple times (depending
#'   on rate) for the interpolation and only the corresponding nearest neighbor
#'   is sampled.
#'   Default is `FALSE`.
#' @template ret_task
#' @references
#' Chawla, N., Bowyer, K., Hall, L., & Kegelmeyer, P. (2000)
#' *SMOTE: Synthetic Minority Over-sampling TEchnique.*
#' In International Conference of Knowledge Based Computer Systems, pp. 46-57.
#' National Center for Software Technology, Mumbai, India, Allied Press.
#' @family imbalancy
#' @export
#' @useDynLib mlr c_smote
smote = function(task, rate, nn = 5L, standardize = TRUE, alt.logic = FALSE) {

  checkTask(task, binary = TRUE)
  assertNumber(rate, lower = 1)
  nn = asInt(nn, lower = 1L)

  requirePackages("cluster", why = "smote", default.method = "load")
  # check for changeData later
  if (!is.null(getTaskWeights(task))) {
    stopf("SMOTE cannot be used with weights in task!")
  }

  # shortcuts
  data = getTaskData(task)
  target = getTaskTargetNames(task)
  y = data[, target]
  x = dropNamed(data, target)
  z = getMinMaxClass(y)
  if (z$min.size < nn) {
    stopf("You cannot set nn = %i, when the minimal class has size %i!", nn, z$min.size)
  }
  x.min = x[z$min.inds, , drop = FALSE]
  n.min = nrow(x.min) # number of NEW cases
  n.new = ifelse(alt.logic, as.integer(rate - 1) * n.min, round((rate - 1) * n.min))
  if (n.new <= 0L) {
    return(task)
  }
  res = matrix(0, n.new, ncol(x))

  is.num = vlapply(x, is.numeric)
  # convert xmin to matrix, so we can handle it better in C
  # factors are integer levels
  x.min.matrix = x.min
  if (any(!is.num)) {
    for (i in seq_col(x.min.matrix)) {
      if (!is.num[i]) {
        x.min.matrix[, i] = as.numeric(as.integer(x.min.matrix[, i]))
      }
    }
  }
  x.min.matrix = as.matrix(x.min.matrix)
  # ensure that x.min.matrix is numeric and not integer since c_smote requires a real valued matrix
  storage.mode(x.min.matrix) = "numeric"

  if (alt.logic == TRUE) {
    n.xmin = dim(x.min.matrix)[1]

    # range per variable
    ranges = apply(x.min.matrix, 2, max) - apply(x.min.matrix, 2, min)

    # loop for each member of x.min
    for (i in 1:n.xmin) {
      # calculate nn next neighbors of element x.min.matrix[i,]
      x.scaled = scale(x.min.matrix, x.min.matrix[i, ], ranges)
      if (any(!is.num)) {
        for (j in seq_col(x.scaled)) {
          if (!is.num[j]) {
            x.scaled[, j] = (x.scaled[, j] != 0)
          }
        }
      }
      dist = drop(x.scaled^2 %*% rep(1, ncol(x.scaled)))
      knns = order(dist)[2:(nn + 1)]

      # new cases per min obs
      n.new.obs = n.new / n.xmin

      # loop for each new member
      for (n in 1:n.new.obs) {
        # randomly select one of the knns
        neigh = sample(1:nn, 1)

        diffs = x.min.matrix[knns[neigh], ] - x.min.matrix[i, ]
        res[(i - 1) * n.new.obs + n, ] = x.min.matrix[i, ] + runif(1) * diffs
        if (any(!is.num)) {
          for (j in seq_col(x.min.matrix)) {
            if (!is.num[j]) {
              res[(i - 1) * n.new.obs + n, j] = c(x.min.matrix[knns[neigh], j],
                x.min.matrix[i, j])[1 + round(runif(1), 0)]
            }
          }
        }
      }
    }
  }
  else {
    # dist matrix on smaller class, diag = 0 so we dont find x as neighbor of x
    minclass.dist = as.matrix(cluster::daisy(x.min, stand = standardize))
    diag(minclass.dist) = NA
    # get n nearest neighbors, we have an index matrix now
    # nearneigh[7, 3] is 3rd nearest neighbor of observation 7
    nearneigh = t(apply(minclass.dist, 1, order))
    nearneigh = nearneigh[, 1:nn, drop = FALSE]
    res = .Call(c_smote, x.min.matrix, is.num, nearneigh, res)
  }

  res = as.data.frame(res)
  # convert ints back to factors
  if (any(!is.num)) {
    for (i in seq_len(ncol(res))) {
      if (!is.num[i]) {
        res[, i] = factor(levels(x[, i])[as.integer(res[, i])],
          levels = levels(x[, i]))
      }
    }
  }

  colnames(res) = colnames(x)
  res[[target]] = z$min.name
  data2 = rbind(data, res)
  # we can neither allow costssens (!= classif anyway nor weights)
  changeData(task, data2)
}

#' Instantiates a resampling strategy object.
#'
#' This class encapsulates training and test sets generated from the data set for a number
#' of iterations. It mainly stores a set of integer vectors indicating the training and
#' test examples for each iteration.
#'
#' Object slots:
#' \describe{
#' \item{desc [\code{\link{ResampleDesc}}]}{See argument.}
#' \item{size [integer(1)]}{See argument.}
#' \item{train.inds [list of \code{integer}]}{List of of training indices for all iterations.}
#' \item{test.inds [list of \code{integer}]}{List of of test indices for all iterations.}
#' \item{group [\code{factor}]}{Optional grouping of resampling iterations. This encodes whether
#'   specfic iterations 'belong together' (e.g. repeated CV), and it can later be used to
#'   aggregate performance values accordingly. Default is 'factor()'.}
#' }
#'
#' @param desc [\code{\link{ResampleDesc}} | \code{character(1)}]\cr
#'   Resampling description object or name of resampling strategy.
#'   In the latter case \code{\link{makeResampleDesc}} will be called internally on the string.
#' @param task [\code{\link{SupervisedTask}}]\cr
#'   Data of task to resample from.
#'   Prefer to pass this instead of \code{size}.
#' @param size [\code{\link{integer}}]\cr
#'   Size of the data set to resample.
#'   Can be used instead of \code{task}.
#' @param ... [any]\cr
#'   Passed down to \code{\link{makeResampleDesc}} in case
#'   you passed a string in \code{desc}.
#'   Otherwise ignored.
#' @return [\code{\link{ResampleInstance}}].
#' @family resample
#' @export
#' @aliases ResampleInstance
#' @examples
#' rdesc = makeResampleDesc("Bootstrap", iters = 10)
#' rin = makeResampleInstance(rdesc, task = iris.task)
#'
#' rdesc = makeResampleDesc("CV", iters = 50)
#' rin = makeResampleInstance(rdesc, size = nrow(iris))
#'
#' rin = makeResampleInstance("CV", iters = 10, task = iris.task)
makeResampleInstance = function(desc, task, size, ...) {
  assert(checkClass(desc, "ResampleDesc"), checkString(desc))
  if (is.character(desc))
    desc = makeResampleDesc(desc, ...)
  if (!missing(task)) {
    assertClass(task, classes = "SupervisedTask")
    size = task$task.desc$size
    blocking = task$blocking
  } else {
    task = NULL
    blocking = factor()
  }
  if (!missing(size))
    size = asCount(size)

  if (length(blocking) && desc$stratify)
    stop("Blocking can currently not be mixed with stratification in resampling!")

  if (length(blocking)) {
    if (is.null(task))
      stop("Blocking always needs the task!")
    levs = levels(blocking)
    size2 = length(levs)
    # create instance for blocks
    inst = instantiateResampleInstance(desc, size2)
    # now exchange block indices with indices of elements of this block and shuffle
    inst$train.inds = lapply(inst$train.inds, function(i) sample(which(blocking %in% levs[i])))
    ti = sample(size)
    inst$test.inds = lapply(inst$train.inds, function(x) setdiff(ti, x))
    inst$size = size
  } else if (desc$stratify) {
    if (is.null(task))
      stop("Stratification always needs the task!")
    if (task$task.desc$type != "classif")
      stop("Stratification is currently only supported for classification!")
    y = getTaskTargets(task)
    # resample on every class
    class.inds = lapply(task$task.desc$class.levels, function(x) which(x == y))
    train.inds = vector("list", length(class.inds))
    test.inds = vector("list", length(class.inds))
    for (i in seq_along(class.inds)) {
      ci = class.inds[[i]]
      if (length(ci)) {
        inst = instantiateResampleInstance(desc, length(ci))
        train.inds[[i]] = lapply(inst$train.inds, function(j) ci[j])
        test.inds[[i]] = lapply(inst$test.inds, function(j) ci[j])
      } else {
        train.inds[[i]] = test.inds[[i]] = replicate(desc$iters, integer(0L), simplify = FALSE)
      }
    }
    inst = instantiateResampleInstance(desc, size)
    inst$train.inds = Reduce(function(i1, i2) Map(c, i1, i2), train.inds)
    inst$test.inds = Reduce(function(i1, i2) Map(c, i1, i2), test.inds)
  } else {
    inst = instantiateResampleInstance(desc, size)
  }
  return(inst)
}

makeResampleInstanceInternal = function(desc, size, train.inds, test.inds, group = factor(c())) {
  if (missing(test.inds) && !missing(train.inds)) {
    # shuffle data set and remove inds
    test.inds = sample(size)
    test.inds = lapply(train.inds, function(x) setdiff(test.inds, x))
  }
  if (!missing(test.inds) && missing(train.inds)) {
    # shuffle data set and remove inds
    train.inds = sample(size)
    train.inds = lapply(test.inds, function(x) setdiff(train.inds, x))
  }
  inst = makeS3Obj("ResampleInstance",
    desc = desc,
    size = size,
    train.inds = train.inds,
    test.inds = test.inds,
    group = group
  )
}

#' @export
print.ResampleInstance = function(x, ...) {
  catf("Resample instance for %i cases.", x$size)
  print(x$desc)
}

#' Instantiates a resampling strategy object.
#'
#' This class encapsulates training and test sets generated from the data set for a number
#' of iterations. It mainly stores a set of integer vectors indicating the training and
#' test examples for each iteration.
#'
#' Object slots:
#' \describe{
#' \item{desc ([ResampleDesc])}{See argument.}
#' \item{size (`integer(1)`)}{See argument.}
#' \item{train.inds (list of [integer])}{List of of training indices for all iterations.}
#' \item{test.inds (list of [integer])}{List of of test indices for all iterations.}
#' \item{group ([factor])}{Optional grouping of resampling iterations. This encodes whether
#'   specfic iterations 'belong together' (e.g. repeated CV), and it can later be used to
#'   aggregate performance values accordingly. Default is 'factor()'.}
#' }
#'
#' @param desc ([ResampleDesc] | `character(1)`)\cr
#'   Resampling description object or name of resampling strategy.
#'   In the latter case [makeResampleDesc] will be called internally on the string.
#' @param task ([Task])\cr
#'   Data of task to resample from.
#'   Prefer to pass this instead of `size`.
#' @param size ([integer])\cr
#'   Size of the data set to resample.
#'   Can be used instead of `task`.
#' @param ... (any)\cr
#'   Passed down to [makeResampleDesc] in case
#'   you passed a string in `desc`.
#'   Otherwise ignored.
#' @return ([ResampleInstance]).
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
  if (!xor(missing(task), missing(size))) {
    stop("One of 'size' or 'task' must be supplied")
  }
  if (!missing(task)) {
    assertClass(task, classes = "Task")
    size = getTaskSize(task)
    blocking = task$blocking
  } else {
    task = NULL
    blocking = factor()
  }
  if (!missing(size))
    size = asCount(size)

  if (length(blocking) && desc$stratify)
    stop("Blocking can currently not be mixed with stratification in resampling!")

  inst = instantiateResampleInstance(desc, size, task)
  return(inst)
}

makeResampleInstanceInternal = function(desc, size, train.inds, test.inds, group = factor()) {
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
  makeS3Obj("ResampleInstance",
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

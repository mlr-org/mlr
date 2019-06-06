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
#'   specific iterations 'belong together' (e.g. repeated CV), and it can later be used to
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
  if (is.character(desc)) {
    desc = makeResampleDesc(desc, ...)
  }
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
  if (!missing(size)) {
    size = asCount(size)
  }

  if (length(blocking) && desc$stratify) {
    stop("Blocking can currently not be mixed with stratification in resampling!")
  }

  # 'fixed' only exists by default for 'CV' -> is.null(desc$fixed)
  # only use this way of blocking if 'fixed = FALSE' -> is.null(desc$fixed)

  fixed = desc$fixed
  blocking.cv = desc$blocking.cv
  if (fixed == FALSE) {
    ### check if blocking should be used or not
    blocking.cv = desc$blocking.cv
  }

  if (length(blocking) > 0 && !fixed && blocking.cv) {
    if (is.null(task)) {
      stop("Blocking always needs the task!")
    }
    levs = levels(blocking)
    size2 = length(levs)
    # create instance for blocks
    inst = instantiateResampleInstance(desc, size2, task)
    # now exchange block indices with indices of elements of this block and shuffle
    inst$train.inds = lapply(inst$train.inds, function(i) sample(which(blocking %in% levs[i])))
    ti = sample(size)
    inst$test.inds = lapply(inst$train.inds, function(x) setdiff(ti, x))
    inst$size = size
  } else if (desc$stratify || !is.null(desc$stratify.cols)) {
    if (is.null(task)) {
      stop("Stratification always needs the task!")
    }
    if (desc$stratify) {
      td = getTaskDesc(task)
      stratify.cols = switch(td$type,
        "classif" = getTaskTargetNames(task),
        "surv" = getTaskTargetNames(task)[2L],
        stopf("Stratification for tasks of type '%s' not supported", td$type))
    } else {
      stratify.cols = desc$stratify.cols
    }

    cn = c(getTaskFeatureNames(task), getTaskTargetNames(task))
    i = which(stratify.cols %nin% cn)
    if (length(i) > 0L) {
      stopf("Columns specified for stratification, but not present in task: %s", collapse(stratify.cols[i]))
    }
    index = getTaskData(task, features = stratify.cols, target.extra = FALSE)[stratify.cols]
    if (any(vlapply(index, is.double))) {
      stop("Stratification on numeric double-precision variables not possible")
    }
    grp = tapply(seq_row(index), index, simplify = FALSE)
    grp = unname(split(seq_row(index), grp))

    # resample on every class
    train.inds = vector("list", length(grp))
    test.inds = vector("list", length(grp))
    for (i in seq_along(grp)) {
      ci = grp[[i]]
      if (length(ci)) {
        inst = instantiateResampleInstance(desc, length(ci), task)
        train.inds[[i]] = lapply(inst$train.inds, function(j) ci[j])
        test.inds[[i]] = lapply(inst$test.inds, function(j) ci[j])
      } else {
        train.inds[[i]] = test.inds[[i]] = replicate(desc$iters, integer(0L), simplify = FALSE)
      }
    }
    inst = instantiateResampleInstance(desc, size, task)
    inst$train.inds = Reduce(function(i1, i2) Map(c, i1, i2), train.inds)
    inst$test.inds = Reduce(function(i1, i2) Map(c, i1, i2), test.inds)
  } else {
    inst = instantiateResampleInstance(desc, size, task)
  }
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

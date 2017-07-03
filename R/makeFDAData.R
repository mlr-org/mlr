#' Create a functional feature from a numeric matrix
#'
#' @param m [\code{matrix}] \cr Numeric matrix.
#' @param fname [\code{character}] \cr Name of the functional feature
#' @param fd.grids [\code{numeric}] \cr fd.grids
#' @return [\code{matrix}] \cr Matrix of class c("functional", "matrix") containing the functional values
#' @export
makeFunctionalFeature = function(m, fname = deparse(substitute(m)), fd.grids = NULL) {
  if (is.data.frame(m))
    m = as.matrix(m)
  assertMatrix(m, mode = "numeric")
  if(is.null(fd.grids))
    fd.grids = seq_len(ncol(m))
  assertCharacter(fname, len = 1L)
  assertNumeric(fd.grids, len = ncol(m))
  attr(m, "fd.grids") = fd.grids
  attr(m, "fname") = fname
  addClasses(m, "functional")
}

if (FALSE) {
  # Case1: Outside of make...Task()
  # Functional Feature m
  nrw = 40L
  ncl = 50L
  m = matrix(rnorm(nrw * ncl), ncol = ncl)
  # Create a functional feature from m
  fd1 = makeFunctionalFeature(m)

  # The data.frame we want to create a task for.
  df = data.frame(x1 = sin(1:40), x2 = as.factor(sample(letters[1:2], size = 40L, replace = TRUE)))
  tsk = makeClassifTask(data = df, target = "x2")

}

#' Add a functional feature to a data.frame or a task
#'
#' Adds a functional feature to an existing data.frame or task.
#' @param obj [\code{data.frame}] | [\code{Task}] \cr
#'   Object the functional feature should be added to.
#' @param fd [\code{functional}] | [\code{matrix}] \cr
#'   Functional feature matrix obtained from
#'   \code{\link{makeFunctionalFeature}}.
#' @param fname [\code{character}] \cr Name of the functional feature
#' @return [\code{data.frame}] | [\code{Task}]  of class c("functional", "matrix")
#' @export
addFunctionalFeature = function(obj, fname = deparse(substitute(m)), fd) {
  # FIXME: We can make this an S3 Method.
  # FIXME: Maybe get a list of fd's and add all of them?
  assertClass(fd, "functional")
  if("Task" %in% class(obj)) {
    df = getTaskData(obj)
    assertMatrix(fd, nrows = nrow(df))
    df[, attr(fd, "fname")] = fd
    obj = mlr:::changeData(obj, df)
  } else if (class(obj) == "data.frame") {
    assertMatrix(fd, nrows = nrow(df))
    obj[, attr(fd, "fname")] = fd
  }
  return(obj)
}

if(FALSE) {
  # Add functional to a Task
  tsk1 = addFunctional(tsk, fd1)

  # Make a Task from a data.frame containing a functional
  df2 = df
  df2$fd1 = fd1
  tsk2 = makeClassifTask(data = df2, target = "x2")

  # Add a functional to a data.frame
  df3 = addFunctional(df, fd1)
  tsk3 = makeClassifTask(data = df3, target = "x2")

  # This aswell alread works for regression tasks and other tasks.
  tsk4 = makeRegrTask(data = df3, target = "x1")
}


# FIXME_1: Do we want the following to work? Some learners do not allow factors, how is this done there?
# Suggestion: Allow it to be treated as numeric, but add a warning?

#  lrn = makeLearner("classif.rpart")
#  mod = train(lrn, tsk1)

# FIXME_2: Specify for each learner if it can deal with functional, much like we have "ordered" and so on.
# This has to be done for the former "fdalearners" only.

# FIXME_3: Get rid of FDATask and FDALearners. Maybe keep the "fda" in the ID, e.g. classif.fdaknn

# FIXME_4: [DO?] Allow the user to construct tasks using the original api, i.e by specifying fd.features and fd.grids.
#          In this case, we simply lapply over the fd.features, extract the columns, create a functional feature and
#          add them back into the data.frame, while erasing the old. This is semi-intelligent, as we introduce I/O
#          when creating the task.


# FIXME: Remove, old.
# # add dataframe extra attributes and append new class attribute
# makeFDAData = function(df, fd.features, fd.grids) {
#   attr(df,"fd.features") = fd.features
#   attr(df,"fd.grids") = fd.grids
#   addClasses(df, "fda.data.frame")
# }

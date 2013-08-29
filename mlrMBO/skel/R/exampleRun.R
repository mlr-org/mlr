#' Perform a model based optimization on a 1D function and and visualize what happens.
#'
#' Run \code{plot} on the resulting object.
#' Useful for figuring out how stuff works and for teaching purposes.
#'
#' The plot will show the following elements per iteration:
#' (a) Above plot
#' - The true objective function (solid line).
#' - The surrogate approximation, represented by its mean response (dotted line).
#' - Surrogate mean +- 1 standard deviation, from local uncertainty (dotted line).
#' (b) Lower plot
#' - Infill criterion.
#' 
#' In both plots the following elements are present
#' - Initial design points (black)
#' - Points from previous sequentail iteraions (green)
#' - Proposed point in current iteration. 
#'
#' @param fun [\code{function}]\cr
#'   Objective function. 
#'   First argument must be a numeric decision variable.
#'   The function has to return a single numerical value.
#' @param surrogate [\code{\link[mlr]{Learner}}]\cr
#'   Surrogate model used for the optimization of \code{fun}.
#FIXME regr.km might be renamed
#'   Default is mlr learner \dQuote{regr.km}, which is kriging from package
#'   DiceKriging. \code{nugget.estim} is set to \code{TRUE} depending on whether we have 
#'   noisy observations or not.
#' @param control [\code{\link{MBOControl}}]\cr
#'   MBO control object.
#FIMXE doc
#' @param  points.per.dim [\code{integer}]\nr
#'   Number of locations at which to sample the \code{fun} function.
#' @param noisy.evals [\code{integer(1)}]\cr
#'   Number of evaluations if \code{fun} is noisy.
#'   Default is 10 for noisy functions
#' @return [\code{list}]:
#'   \item{xseq [\code{numeric}]}{Sequence of x values from the domain of \code{fun}.}
#'   \item{yseq [\code{numeric}]}{Sequence of evaluated points.}
#'   \item{ymat [\code{numeric}]}{Sequence of evaluated points.}
#'   \item{mbo.res [\code{\link{MBOResult}}]}{MBO result object.}
#'   \item{par.set [\code{\link[ParamHelpers]{ParamSet}}]}{See argument.}
#'   \item{learner [\code{\link[mlr]{Learner}}]}{See argument.}
#'   \item{control [\code{\link{MBOControl}}]}{See argument.}
exampleRun = function(fun, par.set, learner, control, points.per.dim=50, noisy.evals=5) {
  checkArg(fun, "function")
  checkArg(par.set, "ParamSet")
  if (missing(learner)) {
    learner = makeLearner("regr.km", predict.type = "se", nugget.estim = control$noisy)
  } else {
    checkArg(learner, "Learner")
  }
  checkArg(control, "MBOControl")
  points.per.dim = convertInteger(points.per.dim)
  checkArg(points.per.dim, "integer", len = 1L, na.ok = FALSE, lower=1L)
  noisy.evals = convertInteger(noisy.evals)
  checkArg(noisy.evals, "integer", len = 1L, na.ok = FALSE, lower=1L)
  n.params = sum(getParamLengths(par.set))
  par.types = extractSubList(par.set$pars, "type")
  #FIXME: what do we allow?
  #if (n.params != 1L)
  #  stopf("exampleRun can currently only be used for 1D functions, but you have: %iD.", n.params)
  #ps2 = filterParams(par.set, "numeric")
  #if (length(ps2$pars) != length(par.set$pars))
  #  stopf("exampleRun can currently only be used numeric parameters.")
  
  #show some info on console
  messagef("Peforming MBO on function.")
  messagef("Initial design: %i. Sequential iterations: %i.", control$init.design.points, control$iters)
  messagef("Learner: %s. Settings:\n%s", learner$id, mlr:::getHyperParsString(learner))

  control$save.model.at=0:control$iters
  names.x = getParamIds(par.set, repeated=TRUE, with.nr=TRUE)
  name.y = control$y.name
  
  #FIXME maybe allow 1 discrete or int param as well!
  
  res = mbo(fun, par.set, learner = learner, control = control)
  
  lower = getLower(par.set)
  upper = getUpper(par.set)
  if (n.params == 1L) {
    if (par.types %in% c("numeric", "numericvector")) {
      xs = seq(lower, upper, length.out=points.per.dim)
      ys = sapply(xs, function(x) {
        if(control$noisy) {
          # do replicates if noisy
          mean(replicate(noisy.evals, fun(list(x=x))))
        } else {
          fun(list(x=x))
        }
      })
      evals = data.frame(x=xs, y=ys)
    }
  } else if (n.params == 2L) {
    if (all(par.types %in% c("numeric", "numericvector"))) {
      eval.x = expand.grid(
        x1=seq(lower[1], upper[1], length.out=points.per.dim),
        x2=seq(lower[2], upper[2], length.out=points.per.dim)
      )
      names(eval.x) = names.x
      xs = dfRowsToList(eval.x, par.set)
      ys = sapply(xs, function(x) {
        if(control$noisy) {
          # do replicates if noisy
          mean(replicate(noisy.evals, fun(x)))
        } else {
          fun(x)
        }
     })
     evals = cbind(eval.x, y=ys)
    }
  }
  colnames(evals) = c(names.x, name.y)
  structure(list(par.set=par.set, n.params=n.params, par.types=par.types, 
    names.x=names.x, name.y=name.y, 
    points.per.dim=points.per.dim, evals=evals,
    learner=learner, control=control, mbo.res=res), class="MBOExampleRun")
}

#' @S3method print MBOExampleRun
print.MBOExampleRun = function(x, ...) {
  catf("MBOExampleRun")
  catf("Number of parameters        : %i", x$n.params)
  catf("Parameter names             : %s", collapse(x$names.x))
  catf("Parameter types             : %s", collapse(x$par.types))
  catf("True points per dim.        : %s", collapse(x$points.per.dim))
  print(x$control)
  catf("Learner                     : %s", x$learner$id)
  catf("Learner settings:\n%s", mlr:::getHyperParsString(learner))
}



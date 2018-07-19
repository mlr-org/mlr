#' @title gstat regression learner.
#'
#' @description
#' mlr learner for regression tasks using [gstat::gstat].
#'
#' This learner does not use the krige interface from gstat. This is because we want to make it the most general as possible.
#' Therefore the learner makes use the combination of gstat::gstat and gstat::predict to compute spatial predidction.
#' You can read more this StackOverflow thread : https://stackoverflow.com/questions/13920342/how-to-make-ordinary-kriging-by-using-gstat-predict.
#'
#' The learner handles gstat variogram autofitting functionnality presented in this post https://www.r-spatial.org/r/2016/02/14/gstat-variogram-fitting.html.
#' To use manual fitting, you must provide a list to the argument model that holds the following elements :
#' psill, model, range and nugget.
#' To use auto fitting, simply provide a list containting the types of models to be tested.
#' Manual fitting example : lrn.man = makeLearner(cl = 'regr.gstat', id = 'manual', psill = 1, model = 'Sph', range = 900, nugget = 1, locations = ~x+y).\n
# 'Automatic fitting example : lrn.auto = makeLearner(cl = 'regr.gstat', id = 'auto', psill = c('Sph','Exp','Gau', 'Mat'), locations = ~x+y)"
#'
#' @references Edzer J Pebesma
#' Multivariable geostatistics in S: the gstat package
#' Computers & Geosciences Volume 30, Issue 7, 2004, 683-691.
#'
#'
#' @name regr.gstat
#' @rdname regr.gstat
NULL

#' @export
makeRLearner.regr.gstat = function() {
  makeRLearnerRegr(
    cl = "regr.gstat",
    package = "gstat",
    par.set = makeParamSet(
      # gstat::gstat params
      makeFunctionLearnerParam(id = "g"),
      makeUntypedLearnerParam(id = "id"), # FIXME what should be the type ?
      makeUntypedLearnerParam(id = "locations", default = ~x+y), # FIXME what should be the type ?
      makeUntypedLearnerParam(id = "beta.gstat"),
      makeIntegerLearnerParam(id = "nmax", default = 0),
      makeIntegerLearnerParam(id = "nmin", default = 0),
      makeIntegerLearnerParam(id = "omax", default = 0),
      makeNumericLearnerParam(id = "maxdist", allow.inf = TRUE, default = Inf),
      makeLogicalLearnerParam(id = "dummy", default = FALSE),
      makeUntypedLearnerParam(id = "set"),
      makeFunctionLearnerParam(id = "x"),
      makeLogicalLearnerParam(id = "fill.all", default = FALSE),
      makeLogicalLearnerParam(id = "fill.cross", default = TRUE),
      makeDiscreteLearnerParam(id = "variance", values = c("identity", "mu", "mu(1-mu)"), default = "identity"),
      makeUntypedLearnerParam(id = "merge"),
      makeIntegerLearnerParam(id = "degree", default = 0),
      makeLogicalLearnerParam(id = "vdist", default = FALSE),
      # gstat::variogram params
      makeNumericLearnerParam(id = "cutoff"), # FIXME default value
      makeNumericLearnerParam(id = "width"),
      makeNumericLearnerParam(id = "alpha", default = 0),
      makeNumericLearnerParam(id = "beta.variogram", default = 0),
      makeNumericLearnerParam(id = "tol.hor"),
      makeNumericLearnerParam(id = "tol.ver"),
      makeLogicalLearnerParam(id = "cressie", default = FALSE),
      makeNumericLearnerParam(id = "dX", default = 0),
      makeNumericLearnerParam(id = "boundaries", default = 0),
      makeLogicalLearnerParam(id = "cloud", default = FALSE),
      makeUntypedLearnerParam(id = "trend.beta", default = NULL),
      makeIntegerLearnerParam(id = "debug.level", default = 1),
      makeLogicalLearnerParam(id = "cross", default = TRUE),
      makeLogicalLearnerParam(id = "map", default = FALSE),
      makeLogicalLearnerParam(id = "projected", default = TRUE),
      makeNumericLearnerParam(id = "lambda", default = 1.0),
      makeLogicalLearnerParam(id = "verbose", default = FALSE),
      makeLogicalLearnerParam(id = "covariogram", default = FALSE),
      makeLogicalLearnerParam(id = "PR", default = FALSE),
      makeIntegerLearnerParam(id = "pseudo", default = -1),
      # gstat::fit.variogram params
      makeLogicalLearnerParam(id = "fit.sills", default = TRUE),
      makeLogicalLearnerParam(id = "fit.ranges", default = TRUE),
      makeIntegerLearnerParam(id = "fit.method", default = 7),
      makeLogicalLearnerParam(id = "warn.if.neg", default = FALSE),
      makeLogicalLearnerParam(id = "fit.kappa", default = FALSE),
      # gstat::vgm params
      makeUntypedLearnerParam(id = "psill", default = NA),
      makeUntypedLearnerParam(id = "model"), # FIXME what should be the type ?
      makeUntypedLearnerParam(id = "range", default = NA),
      makeNumericLearnerParam(id = "kappa", default = 0.5),
      makeUntypedLearnerParam(id = "nugget"),
      makeUntypedLearnerParam(id = "add.to"),
      makeUntypedLearnerParam(id = "covtable"),
      makeNumericLearnerParam(id = "Err", default = 0)
    ),
    par.vals = list(locations = ~x+y, nmax = 0, nmin = 0, omax = 0, maxdist = Inf,
      dummy = FALSE, fill.all = FALSE, fill.cross = TRUE, variance = "identity", degree = 0, vdist = FALSE,
      alpha = 0, beta.variogram = 0, cressie = FALSE, dX = 0, boundaries = 0, cloud = FALSE,
      trend.beta = NULL, debug.level = 1, cross = TRUE, map = FALSE, projected = TRUE, lambda = 1.0,
      verbose = FALSE, covariogram = FALSE, PR = FALSE, pseudo = -1,
      fit.sills = TRUE, fit.ranges = TRUE, fit.method = 7, warn.if.neg = FALSE, fit.kappa = FALSE,
      psill = NA, range = NA, kappa = 0.5, Err = 0),
    properties = c("numerics", "factors" , "se", "weights", "missings"),
    name = "Multivariable Geostatistical Prediction And Simulation",
    short.name = "gstat",
    note = "The default prediction (when passing no arguments) is IDW (inverse distance weighted).
    The learner handles gstat variogram autofitting functionnality presented in this post https://www.r-spatial.org/r/2016/02/14/gstat-variogram-fitting.html.\n
    Manual fitting example : lrn.man = makeLearner(cl = 'regr.gstat', id = 'manual', model = list(psill = 1, model = 'Sph', range = 900, nugget = 1), locations = ~x+y).\n
    Automatic fitting example : lrn.auto = makeLearner(cl = 'regr.gstat', id = 'auto', model = list(psill = c('Sph','Exp','Gau', 'Mat')), locations = ~x+y)"
  )
}

#' @export
# https://stackoverflow.com/questions/19075331/passing-a-function-argument-to-other-arguments-which-are-functions-themselves
# https://stackoverflow.com/questions/16774946/passing-along-ellipsis-arguments-to-two-different-functions
trainLearner.regr.gstat = function(.learner, .task, .subset, .weights = NULL, ...) {
  browser()
  dots = list(...)
  # https://stackoverflow.com/questions/11885207/get-all-parameters-as-list
  variogram.names = names(formals(gstat::variogram)) # FIXME cannot retrieve the S3 method for formula arguments
  variogram.names = c("object", "locations", "beta.variogram")
  #variogram.names = replace(names(formals(gstat::variogram)), variogram.names == "beta", "beta.variogram")
  fit.variogram.names = names(formals(gstat::fit.variogram))
  gstat.names = replace(names(formals(gstat::gstat)), names(formals(gstat::gstat)) == "beta", "beta.gstat")

  d = getTaskData(.task, .subset)
  f = getTaskFormula(.task, explicit.features = TRUE)
  # remove location vars as they are handled by gstat - https://stackoverflow.com/questions/40308944/removing-offset-terms-from-a-formula
  f = update(f, .~.-y-x) # FIXME should be the params entered in locations arg
  # check if a variogram model is passed
  if (!is.na(dots$psill)) {
    # build the samples variogram
    v = do.call(gstat::variogram, c(list(object = f, data = d), dots[ names(dots) %in% variogram.names] ))
    # fit the variogram model
    fit = do.call(gstat::fit.variogram,
      c(list(object = v,
        model = gstat::vgm(psill = dots$psill,
          model = dots$model,
          range = dots$range,
          nugget = dots$nugget)),
        dots[names(dots) %in% fit.variogram.names[fit.variogram.names != "model"]])
    )
    # create the gstat object with a model
    g = do.call(gstat::gstat,
      c(list(formula = f,
        data = d,
        model = fit
        ),
        dots[names(dots) %in% gstat.names[gstat.names != "model"]])
    )
  } else {
    # create the gstat object without model
    browser()
    g = do.call(gstat::gstat,
      c(list(formula = f,
        data = d),
        dots[names(dots) %in% gstat.names[gstat.names != "model"]])
    )
  }
  return(g)
}

#' @export
predictLearner.regr.gstat = function(.learner, .model, .newdata, ...) {
  p = predict(
    object = .model$learner.model,
    newdata = .newdata
  )
  if (.learner$predict.type == "response") {
    p = p[["var1.pred"]]
  } else {
    p = as.matrix(p[3:4])
  }
}

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
#' Manual fitting example : lrn.man = makeLearner(cl = 'regr.gstat', id = 'manual', model = list(psill = 1, model = 'Sph', range = 900, nugget = 1), locations = ~x+y).\n
# 'Automatic fitting example : lrn.auto = makeLearner(cl = 'regr.gstat', id = 'auto', model = list(psill = c('Sph','Exp','Gau', 'Mat')), locations = ~x+y)"
#'
#' @references Edzer J Pebesma
#' Multivariable geostatistics in S: the gstat package
#' Computers & Geosciences Volume 30, Issue 7, 2004, 683-691.
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
      makeFunctionLearnerParam(id = "g"),
      makeUntypedLearnerParam(id = "id"), # FIXME what should be the type ?
      makeUntypedLearnerParam(id = "locations", default = ~x+y), # FIXME what should be the type ?
      makeUntypedLearnerParam(id = "model", default = NULL), # FIXME what should be the type ?
      makeIntegerVectorLearnerParam(id = "beta"),
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
      makeUntypedLearnerParam(id = "lambda")
    ),
    par.vals = list(locations = ~x+y, model = NULL, nmax = 0, nmin = 0, omax = 0, maxdist = Inf, force = FALSE,
      dummy = FALSE, fill.all = FALSE, fill.cross = TRUE, variance = "identity", degree = 0, vdist = FALSE),
    properties = c("numerics", "factors" , "se", "weights", "missings"),
    name = "Multivariable Geostatistical Prediction And Simulation",
    short.name = "gstat",
    note = "The default prediction (when passing no arguments) is IDW (inverse distance weighted). To make the learner work, you cannot use the standard krige interface from gstat.\n
    You must first create a gstat object. The train function handles it for you.\n
    More information here : https://stackoverflow.com/questions/13920342/how-to-make-ordinary-kriging-by-using-gstat-predict.\n
    The learner handles gstat variogram autofitting functionnality presented in this post https://www.r-spatial.org/r/2016/02/14/gstat-variogram-fitting.html.\n
    Manual fitting example : lrn.man = makeLearner(cl = 'regr.gstat', id = 'manual', model = list(psill = 1, model = 'Sph', range = 900, nugget = 1), locations = ~x+y).\n
    Automatic fitting example : lrn.auto = makeLearner(cl = 'regr.gstat', id = 'auto', model = list(psill = c('Sph','Exp','Gau', 'Mat')), locations = ~x+y)"
  )
}

#' @export
trainLearner.regr.gstat = function(.learner, .task, .subset, .weights = NULL, model = NULL, ...) {
  args = list(...)
  d = getTaskData(.task, .subset)
  f = getTaskFormula(.task, explicit.features = TRUE)
  # remove location vars as they are handled by gstat - https://stackoverflow.com/questions/40308944/removing-offset-terms-from-a-formula
  f = update(f, .~.-y-x) # FIXME should be the params entered in locations arg
  # check if a variogram model is passed
  if (!is.null(model)) {
    # build the samples variogram
    v = gstat::variogram(object = f, data = d, ...)#...
    # fit the variogram model
    fit = gstat::fit.variogram(v, gstat::vgm(psill = model$psill, model = model$model,
      range = model$range, nugget = model$nugget))
    # create the gstat object
    g = gstat::gstat(
      formula = f,
      data = d,
      model = fit,
      ...
    )
  } else {
    g = gstat::gstat(
      formula = f,
      data = d,
      ...
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

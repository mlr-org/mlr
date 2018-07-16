#' @export
makeRLearner.regr.gstat = function(){#https://www.rdocumentation.org/packages/gstat/versions/1.1-6/topics/fit.variogram
  makeRLearnerRegr(
    cl = "regr.gstat",
    package = "gstat",
    par.set = makeParamSet(#https://www.rdocumentation.org/packages/ParamHelpers/versions/1.8/topics/LearnerParam
      makeFunctionLearnerParam(id = "g"),
      makeUntypedLearnerParam(id = "id"),#FIXME
      makeUntypedLearnerParam(id = "locations"),#FIXME
      makeUntypedLearnerParam(id = "model", default = NULL),#FIXME what should be the type
      makeIntegerVectorLearnerParam(id = "beta"),
      makeIntegerLearnerParam(id = "nmax", default = 0),
      makeIntegerLearnerParam(id = "nmin", default = 0),
      makeIntegerLearnerParam(id = "omax", default = 0),
      makeNumericLearnerParam(id = "maxdist", default = 100000000000),#FIXME should be Inf
      makeLogicalLearnerParam(id = "force", default = FALSE),
      makeLogicalLearnerParam(id = "dummy", default = FALSE),
      makeUntypedLearnerParam(id = "set"),
      makeFunctionLearnerParam(id = "x"),
      makeLogicalLearnerParam(id = "fill.all", default = FALSE),
      makeLogicalLearnerParam(id = "fill.cross", default = TRUE),
      makeDiscreteLearnerParam(id = "variance", values = c("identity", "mu", "mu(1-mu)"), default = "identity"),
      #makeNumericVectorLearnerParam(id = "weights", default = NULL),#FIXME
      makeUntypedLearnerParam(id = "merge"),
      makeIntegerLearnerParam(id = "degree", default = 0),
      makeLogicalLearnerParam(id = "vdist", default = FALSE),
      makeUntypedLearnerParam(id = "lambda")
    ),
    par.vals = list(model = NULL, nmax = 0, nmin=0, omax=0, maxdist=Inf, force=FALSE, dummy=FALSE, fill.all=FALSE, fill.cross=TRUE, variance="identity", degree=0, vdist=FALSE),
    properties = c("numerics", "factors" , "se", "weights", "missings"),
    name = "Multivariable Geostatistical Prediction And Simulation",
    short.name = "gstat",
    note = "To make the learner work, you cannot use the standard krige interface from gstat.\n
    You must fisrt create a gstat object. The train function handles it for you.\n
    More here : https://stackoverflow.com/questions/13920342/how-to-make-ordinary-kriging-by-using-gstat-predict.\n
    The learner handles gstat variogram autofitting functionnality presented in this post https://www.r-spatial.org/r/2016/02/14/gstat-variogram-fitting.html.\n
    Manual fitting : lrn.man = makeLearner(cl = 'regr.gstat', id= 'manual', model = list(psill=1, model='Sph', range=900, nugget=1), locations = ~x+y).\n
    Automatic fitting : lrn.auto = makeLearner(cl = 'regr.gstat', id= 'auto', model = list(psill=c('Sph','Exp','Gau', 'Mat')), locations = ~x+y)"
  )
}

#' @export
trainLearner.regr.gstat = function(.learner, .task, .subset, .weights = NULL, model = NULL, ...) {
  #browser()
  # data
  d = getTaskData(.task, .subset)
  #sp::coordinates(d) = ~x+y
  # formula
  f <- getTaskFormula(.task, explicit.features = TRUE)
  # remove location vars as handled by gstat - https://stackoverflow.com/questions/40308944/removing-offset-terms-from-a-formula
  f <- update(f, .~.-y-x)
  if(!is.null(model)){
    # variogram
    v = variogram(object = f, locations = ~x+y, data =d)
    fit <- gstat::fit.variogram(v, gstat::vgm(psill=model$psill, model=model$model, range=model$range, nugget = model$nugget))
    # plot(v, fit)
    # create the gstat object
    g <- gstat::gstat(
      formula = f,
      data = d,
      model = fit,
      ...
    )
  }else{
    g <- gstat::gstat(
      formula = f,
      data = d,
      ...
    )
  }
}

#' @export #multivariate prediction https://www.rdocumentation.org/packages/gstat/versions/1.0-2/topics/predict.gstat
predictLearner.regr.gstat = function(.learner, .model, .newdata, ...) {
  #browser()
  p = predict( # https://www.rdocumentation.org/packages/gstat/versions/1.0-2/topics/predict
    object = .model$learner.model,
    newdata = .newdata
  )
  if (.learner$predict.type == "response") {
    p = p[["var1.pred"]]
  } else {
    p = as.matrix(p[3:4])
  }
}

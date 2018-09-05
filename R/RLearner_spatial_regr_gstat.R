#' @title gstat regression learner.
#'
#' @description
#' mlr learner for regression tasks using [gstat::gstat].
#'
#' This learner does not use the krige interface from gstat. This is because we want to make it the most general as possible.
#' Therefore the learner makes use the combination of gstat::gstat and gstat::predict to compute spatial predidction.
#' You can read more this StackOverflow thread : https://stackoverflow.com/questions/13920342/how-to-make-ordinary-kriging-by-using-gstat-predict.
#'
#' The columns holding the longitude and latitude values must be respectively named x and y
#'
#' The learner handles gstat variogram autofitting functionnality presented in this post https://www.r-spatial.org/r/2016/02/14/gstat-variogram-fitting.html.
#' To use manual fitting, you must provide a list to the argument model that holds the following elements :
#' psill, model, range and nugget.
#' To use auto fitting, simply provide a list containting the types of models to be tested.
#'
#' @examples
#' # loading datasets
#' library(sp)
#' data(meuse)
#' data(meuse.grid)
#' # imputing values to missing data
#' meuse = impute(meuse, classes = list(numeric = imputeMean(), factor = imputeMode()),
#'   dummy.classes = "integer")$data
#' meuse.grid = impute(meuse.grid, classes = list(numeric = imputeMean(), factor = imputeMode()),
#'   dummy.classes = "integer")$data
#' # adding a column with log zinc
#' meuse = meuse %>% dplyr::mutate(log_zinc = log(zinc))
#' # defining the regression task
#' task = makeRegrTask(id = "meuse",  data = meuse, target = "log_zinc")
#' task.krg = dropFeatures(task = task, features = getTaskFeatureNames(task)[-c(1,2)])
#' # defining the learner with manual variogram fitting
#' lrn.krg = makeLearner(cl = "regr.gstat", id = "ln(zn) mlr ordinary kriging", predict.type = "response", psill = 1, model = "Sph", range = 900, nugget = 1)
#' # in case, you want to define the same learner but with automatic variogram fitting use this line instead
#' lrn.krg.auto = makeLearner(cl = 'regr.gstat', id = 'ln(zn) mlr ordinary kriging autofit',  predict.type = "response", psill = c('Sph','Exp','Gau', 'Mat'))
#' # training the model
#' mod.krg = train(lrn.krg, task.krg)
#' # kriging
#' newdata.pred.krg = predict(object = mod.krg, newdata = meuse.grid)
#' ok.mlr = bind_cols(data.frame(meuse.grid), newdata.pred.krg$data)
#' # mapping
#' sp::coordinates(ok.mlr) = ~x+y
#' sp::gridded(ok.mlr) = TRUE
#' ok.mlr.plot = sp::spplot(ok.mlr["response"], do.log = T, colorkey = TRUE, main = "log(zn) : ok interpolation (gstat)")
#' # SE - defining the standard error learner by altering the previous one.
#' se.lrn.krg = setPredictType(lrn.krg, predict.type = "se")
#' # training the SE model
#' se.mod.krg = train(se.lrn.krg, task.krg)
#' # SE kriging
#' se.newdata.pred.krg = predict(object = se.mod.krg, newdata = meuse.grid)
#' ok.se.mlr = bind_cols(data.frame(meuse.grid), se.newdata.pred.krg$data)
#' # SE mapping
#' sp::coordinates(ok.se.mlr) = ~x+y
# sp::gridded(ok.se.mlr) = TRUE
# ok.se.mlr.plot = sp::spplot(ok.se.mlr["se"], do.log = T, colorkey = TRUE, main = "se(log(zn)) : ok interpolation (mlr)")
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
      makeUntypedLearnerParam(id = "id", tunable = FALSE), # FIXME what should be the type ?
      makeUntypedLearnerParam(id = "beta.gstat"),
      makeNumericLearnerParam("nmax", allow.inf = TRUE, default = Inf, lower = 0, upper = Inf),
      makeIntegerLearnerParam(id = "nmin", default = 0, lower = 0, upper = Inf),
      makeIntegerLearnerParam(id = "omax", default = 0, lower = 0, upper = Inf),
      makeNumericLearnerParam(id = "maxdist", default = Inf, lower = 0, upper = Inf, allow.inf = TRUE),
      makeLogicalLearnerParam(id = "dummy", default = FALSE),
      makeUntypedLearnerParam(id = "set"),
      makeFunctionLearnerParam(id = "x"),
      makeLogicalLearnerParam(id = "fill.all", default = FALSE),
      makeLogicalLearnerParam(id = "fill.cross", default = TRUE),
      makeDiscreteLearnerParam(id = "variance", values = c("identity", "mu", "mu(1-mu)"), default = "identity"),
      makeUntypedLearnerParam(id = "merge"),
      makeIntegerLearnerParam(id = "degree", default = 0, lower = 0, upper = 3),
      makeLogicalLearnerParam(id = "vdist", default = FALSE),
      # gstat::variogram params
      makeNumericLearnerParam(id = "cutoff", lower = 0, upper = Inf), # default value is calculated according to spatial extent of data
      makeNumericLearnerParam(id = "width"),
      makeNumericLearnerParam(id = "alpha", default = 0, lower = 0, upper = 360),
      makeNumericLearnerParam(id = "beta.variogram", default = 0, lower = 0, upper = 360),
      makeNumericLearnerParam(id = "tol.hor", lower = 0, upper = 360),
      makeNumericLearnerParam(id = "tol.ver", lower = 0, upper = 360),
      makeLogicalLearnerParam(id = "cressie", default = FALSE),
      makeNumericLearnerParam(id = "dX", default = 0),
      makeLogicalLearnerParam(id = "cloud", default = FALSE),
      makeUntypedLearnerParam(id = "trend.beta", default = NULL),
      makeIntegerLearnerParam(id = "debug.level", default = 1, lower = -1, upper = 1),
      makeLogicalLearnerParam(id = "cross", default = TRUE),
      makeLogicalLearnerParam(id = "map", default = FALSE),
      makeLogicalLearnerParam(id = "projected", default = TRUE),
      makeNumericLearnerParam(id = "lambda", default = 1.0), # from gstat doc : test feature; not working (yet)
      makeLogicalLearnerParam(id = "verbose", default = FALSE),
      makeLogicalLearnerParam(id = "covariogram", default = FALSE),
      makeLogicalLearnerParam(id = "PR", default = FALSE),
      makeIntegerLearnerParam(id = "pseudo", default = -1, lower = -1, upper = 1),
      # gstat::fit.variogram params
      makeUntypedLearnerParam(id = "model.auto"), # The types of model you want gstat::fit.variogram to try. Might be any combinations of e.g. "Exp", "Sph", "Gau", "Mat".
      makeLogicalLearnerParam(id = "fit.sills", default = TRUE),
      makeLogicalLearnerParam(id = "fit.ranges", default = TRUE),
      makeIntegerLearnerParam(id = "fit.method", default = 7, lower = 1, upper = 7),
      makeLogicalLearnerParam(id = "warn.if.neg", default = FALSE),
      makeLogicalLearnerParam(id = "fit.kappa", default = FALSE),
      # gstat::vgm params
      makeNumericLearnerParam(id = "psill", default = NA, lower = 0, upper = Inf, special.vals = list(NA)),
      makeUntypedLearnerParam(id = "model.manual", default = "Sph"), # The type of model you want gstat::vgm to generates. Might be e.g. "Exp", "Sph", "Gau", "Mat". See https://www.rdocumentation.org/packages/gstat/versions/1.1-6/topics/vgm.
      makeNumericLearnerParam(id = "range", default = NA, lower = 0, upper = Inf, special.vals = list(NA)),
      makeNumericLearnerParam(id = "kappa", default = 0.5, lower = 0, upper = 1),
      makeNumericLearnerParam(id = "nugget", lower = 0, upper = Inf),
      makeUntypedLearnerParam(id = "add.to"),
      makeUntypedLearnerParam(id = "covtable"),
      makeNumericLearnerParam(id = "Err", default = 0) #FIXME impossible to find the lower and upper in the doc https://www.rdocumentation.org/packages/gstat/versions/1.1-6/topics/vgm
    ),
    properties = c("numerics", "factors" , "se", "weights", "missings"),
    name = "Multivariable Geostatistical Prediction And Simulation",
    short.name = "gstat",
    note = "The default prediction (when passing no arguments) is IDW (inverse distance weighted).
    The learner handles gstat variogram autofitting functionnality presented in this post https://www.r-spatial.org/r/2016/02/14/gstat-variogram-fitting.html.\n
    Manual fitting example : lrn.man = makeLearner(cl = 'regr.gstat', id = 'manual', psill = 1, model = 'Sph', range = 900, nugget = 2, locations = ~x+y).\n
    Automatic fitting example : lrn.auto = makeLearner(cl = 'regr.gstat', id = 'auto', psill = c('Sph','Exp','Gau', 'Mat'), locations = ~x+y)"
  )
}

#' @export
# https://stackoverflow.com/questions/19075331/passing-a-function-argument-to-other-arguments-which-are-functions-themselves
# https://stackoverflow.com/questions/16774946/passing-along-ellipsis-arguments-to-two-different-functions
trainLearner.regr.gstat = function(.learner, .task, .subset, .weights = NULL, ...) {
  # FIXME explain the chaining of the pars into the functions

  # Getting the list of params passed to the learner
  pars = list(...)

  # Getting the passed dataset
  data = getTaskData(.task, .subset)

  # User must name lat and lon columns y and x respectively otherwise the function stops
  if (is.null(data$x) || is.null(data$y)) {
    stop("Longitude and latitude data must be stored in columns named x and y respectively")
  }

  # Retrieving the name of the target var
  target = getTaskTargetNames(.task)

  # Assigning the data that is not location to the object element of pars list (must be a list for gstat::variogram.default)
  pars$object = as.list(data[colnames(data) != c("x","y", target)])

  # list with for each variable the vector with responses
  # browser()

  # Assigning the location data to the location element of pars list (must be a list for gstat::variogram.default)
  pars$locations = as.list(data[colnames(data) == c("x","y")])

  # Getting the task formula
  pars$formula = getTaskFormula(.task, explicit.features = TRUE)
  # Removing location vars as they are handled by gstat - https://stackoverflow.com/questions/40308944/removing-offset-terms-from-a-formula
  pars$formula = update(pars$formula, .~.-y-x)


  # Assigning forced x and y location formula
  # pars$locations = ~x+y
  #pars$locations = list("x","y")

  # Extracting the names of the parameters that can be passed to the various gstat functions required to train the learner.
  # These functions are gstat::variogram, gstat::fit.variogram, gstat::vgm and gstat::gstat
  # To extract the arguments names : https://stackoverflow.com/questions/11885207/get-all-parameters-as-list
  # To extract the arguments names of S3 method default https://stackoverflow.com/questions/45083015/getting-arguments-of-s3-method-in-r
  # As some of the gstat functions required to the train the learner use arguments with the same names, we rename these arguments according to the id declared in makeRLearner.regr.gstat.
  gstat.args = replace(names(formals(gstat::gstat)), names(formals(gstat::gstat)) == "beta", "beta.gstat")
  variogram.args = replace(names(formals(gstat:::variogram.default)), names(formals(gstat:::variogram.default)) == "beta", "beta.variogram")
  fit.variogram.args = replace(names(formals(gstat::fit.variogram)), names(formals(gstat::fit.variogram)) == "model", "model")
  vgm.args = replace(names(formals(gstat::vgm)), names(formals(gstat::vgm)) == "model", "model")

  # Getting the task formula and assigning it to the pars$object
  #pars$object = getTaskFormula(.task, explicit.features = TRUE)
  # Removing location vars as they are handled by gstat - https://stackoverflow.com/questions/40308944/removing-offset-terms-from-a-formula
  #pars$object = update(pars$object, .~.-y-x)

  # If model.manual or model.auto arguments are passed, we use it to build the variogram model required to perform kriging interpolation
  if (!is.null(pars$model.manual) || !is.null(pars$model.auto)) {

    # calculate sample variogram (https://www.rdocumentation.org/packages/gstat/versions/1.1-6/topics/variogram)
    pars$object = do.call(
      gstat:::variogram.default,
        as.list(
          pars[names(pars) %in% variogram.args ]) # FIXME hack because we use variogram in formula method
    )
    # pars$object = do.call(
    #   gstat::variogram,
    #   c(
    #     list(
    #       object = fml,
    #       data = d,
    #       locations = ~x+y),
    #   pars[names(pars) %in% variogram.args && !names(pars) %in% c("object", "data", "locations")])
    # )
    # Check if we are in auto-fitting mode (i.e. vector of model types to be tested passed to model.auto argument) and re-assigning the pars to the proper args
    if (!is.null(pars$model.auto)) {
      pars$psill = pars$model.auto # this is the way gstat works ! If a set of models are passed to the psill argument, gstat performs an autofitting by testing all the kind of models passed
      pars = pars[names(pars) != "model.auto"]
    }
    if (!is.null(pars$model.manual)) {
      pars$model = pars$model.manual
      pars = pars[names(pars) != "model.manual"]
    }

    # generate the variogram model (https://www.rdocumentation.org/packages/gstat/versions/1.1-6/topics/vgm)
    pars$model = do.call(
      gstat::vgm,
        as.list(
          pars[names(pars) %in% vgm.args]) # passing all the pars corresponding to args of gstat::vgm function
    )

    # (auto)fit the variogram model (https://www.rdocumentation.org/packages/gstat/versions/1.1-6/topics/fit.variogram)
    pars$model = do.call(
      gstat::fit.variogram,
        as.list(
          pars[names(pars) %in% fit.variogram.args])
    )
    # fit = do.call(
    #   gstat::fit.variogram,
    #   c(
    #     list(
    #       object = v,
    #       model = model),
    #     pars[names(pars) %in% fit.variogram.args && !names(pars) %in% c("object", "model")])
    #   #pars[names(pars) %in% fit.variogram.args[fit.variogram.args != "model"]])
    # )

  } else { # Case where no models are passed. We don't use any predictor to make the spatial interpolation. Se solely use x and y data to interpolate the target var
    pars$formula = update(pars$formula, .~1) # https://stackoverflow.com/questions/18070131/update-formula-in-r
    pars$model = NULL #fit = NULL
  }

  # as the gstat::gstat function requires to pass the location information as a formula, we update pars accordingly
  pars$locations = ~x+y
  # as the gstat::gstat function requires to pass the dataset in the data arg we update pars accordindly  :
  pars$data = data
  browser()

  # Creating the gstat object
  g = do.call(gstat::gstat,
      as.list(
      pars[names(pars) %in% gstat.args])
  )
  # g = do.call(gstat::gstat,
  #   c(
  #     list(
  #       formula = fml,
  #       data = d,
  #       model = fit,
  #       locations = ~x+y),
  #     pars[names(pars) %in% gstat.args && !names(pars) %in% c("formula", "data", "model", "locations")])
  # )

  return(g)
}

#' @export
predictLearner.regr.gstat = function(.learner, .model, .newdata, ...) {
  browser()
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

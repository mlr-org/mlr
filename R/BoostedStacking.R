#' @title Create a BoostedStacking object.
#' 
#' @description
#'   A boosted stacking learner runs a tuning using a \code{\link{ModelMultiplexer}} and adds the predictions from the best model as a new feature to the training data set. This procedure is repleated several times until \code{niter} is reached. 
#' 
#' @param model.multiplexer [\code{\link{ModelMultiplexer}}]\cr
#'   The multiplexer learner.
#' @param mm.ps Collection of parameters and their constraints for optimization.
#'   Dependent parameters with a \code{requires} field must use \code{quote} and not
#'   \code{expression} to define it.
#' @param control [\code{\link{TuneControlRandom}}]\cr
#'   Control object for search method. 
#' @param resampling [\code{\link{ResampleDesc}} \cr
#'   Resampling strategy.
#' @param predict.type [\code{character(1)}]\cr 
#'   Classification: \dQuote{response} (= labels) or \dQuote{prob} (= probabilities and labels by selecting the ones with maximal probability).
#'   Regression: \dQuote{response} (= mean response) or \dQuote{se} (= standard errors and mean response).
#'   par.set [\code{\link[ParamHelpers]{ParamSet}}]\cr
#' @param measures [\code{\link{Measure}}]\cr
#'   Performance measures.
#' @param niter [\code{integer}]\cr
#'   Number of boosting iterations.
#' @param tolerance [\code{numeric}]\cr
#'   Tolerance for stopping criterion. For measures which are to minimize: Positive values indicate that in each iteration an improvement is forced. 
#'   Worsening of performance is allowed for negative values. For measures to maximize the opposite is true. 
#' @param subsemble.prop  [\code{numeric}]\cr 
#'   Proportion of data where tuning is run on. 
#' @examples 
#' \dontrun{
#' lrns = list(
#'   #makeLearner("classif.ksvm", kernel = "rbfdot"), # no implm for response and multiclass
#'   makeLearner("classif.gbm"),
#'   makeLearner("classif.randomForest"))
#' mm = makeModelMultiplexer(lrns)
#' ctrl = makeTuneControlRandom(maxit = 3L)
#' ps = makeModelMultiplexerParamSet(mm,
#'   makeIntegerParam("n.trees", lower = 1L, upper = 500L),
#'   makeIntegerParam("interaction.depth", lower = 1L, upper = 10L),
#'   makeIntegerParam("ntree", lower = 1L, upper = 500L),
#'   makeIntegerParam("mtry", lower = 1L, upper = getTaskNFeats(pid.task)))
#' lrns = lapply(lrns, setPredictType, "prob")
#' stb = makeBoostedStackingLearner(model.multiplexer = mm,
#'   predict.type = "prob", resampling = cv5, mm.ps = ps, control = ctrl,
#'   measures = mmce, niter = 2L)
#' r = resample(stb, task = pid.task, resampling = cv2)
#' }
#' @export  

makeBoostedStackingLearner = function(model.multiplexer, mm.ps, 
  control, resampling = cv2, predict.type = "prob",
  measures, niter = 2L, tolerance = -0.1, subsemble.prop = 0.8) {
  # checks
  assertClass(model.multiplexer, "ModelMultiplexer")
  assertClass(mm.ps, "ParamSet")
  assertClass(control, "TuneControlRandom") # for now
  assertClass(resampling, "ResampleDesc")
  assertChoice(predict.type, choices = c("response", "prob"))
  checkMeasures(measures)
  assertInt(niter, lower = 1L)
  assertNumber(tolerance)
  # 
  if (model.multiplexer$type == "classif" & model.multiplexer$predict.type == "response" & "factors" %nin% model.multiplexer$properties) {
    stop("Base models in model multiplexer does not support classifcation with factor features, which are created by using predict.type='response' within base learners.")
  }
  # FIXME input check: measures and type
  #
  par.set = makeParamSet(makeIntegerParam("niter", lower = 1, tunable = FALSE))

  bsl = makeLearnerBaseConstructor(classes = "BoostedStackingLearner", 
  	id = "boostedStacking", 
  	type = model.multiplexer$type,
  	package = model.multiplexer$package,
  	properties = model.multiplexer$properties,
  	par.set = par.set, 
  	par.vals = list(niter = niter), 
  	predict.type = predict.type)

  bsl$fix.factors.prediction = TRUE
  bsl$model.multiplexer = model.multiplexer
  bsl$mm.ps = mm.ps
  bsl$resampling = resampling
  bsl$measures = measures
  bsl$control = control
  bsl$tolerance = tolerance
  bsl$subsemble.prop = subsemble.prop
  return(bsl)
}


#' Train Boosted Stacking object.
#' 
#' @param .learner Learner
#' @param .task [\code{Task}].
#' @param .subset Subset of task.
#' @param ... ...
#' @export

trainLearner.BoostedStackingLearner = function(.learner, .task, .subset, ...) {
  # checks
  if (.task$type == "regr") {
    bpt = unique(extractSubList(.learner$model.multiplexer$base.learners, "predict.type"))
    spt = .learner$predict.type
    if (any(c(bpt, spt) == "prob")) 
      stopf("Base learner predict types are '%s' and final predict type is '%s', but both should be 'response' for regression.", bpt, spt)
  }
  # body
  niter = .learner$par.vals$niter
  tolerance = .learner$tolerance
  subsemble.prop = .learner$subsemble.prop
  bms.pt = unique(extractSubList(.learner$model.multiplexer$base.learner, "predict.type"))
  if (length(bms.pt) > 1) stopf("Different predict.types for Base Learners is not supported.")
  new.task = subsetTask(.task, subset = .subset)
  task.size = getTaskSize(new.task)
  base.models = preds = vector("list", length = niter)
  
  score = rep(ifelse(.learner$measures$minimize, Inf, -Inf), 1 + niter)
  names(score) = c("init.score", paste("not.set", 1:niter, sep = "."))

  for (i in seq_len(niter)) {
    # Parameter Tuning
    subset.idx = sample(seq_len(task.size), subsemble.prop * task.size)
    res = tuneParams(learner = .learner$model.multiplexer, task = subsetTask(new.task, subset.idx), 
      resampling = .learner$resampling, measures = .learner$measures, 
      par.set = .learner$mm.ps, control = .learner$control, show.info = FALSE)
    # Stopping criterium
    score[i+1] = res$y[1]
    names(score)[i+1] = paste(res$x$selected.learner, i, sep = ".")
    shift = score[i] - score[i+1]
    tol.reached = ifelse(.learner$measures$minimize, shift < tolerance, shift > tolerance)
    #messagef(">force.stop is %s", tol.reached
    if (tol.reached) {
      messagef("[Tolerance Reached] Boosting stopped in iteration %s", i)
      to.rm = i:niter
      score = score[-c(to.rm + 1)] # FIXME should it be removed!?
      base.models[to.rm] = NULL
      preds[to.rm] = NULL
      break()
    }
    # create learner, model, prediction
    best.lrn = makeXBestLearnersFromMMTuneResult(tune.result = res,
      model.multiplexer = .learner$model.multiplexer, mm.ps = .learner$mm.ps,
      x.best = 1, measure = .learner$measures)[[1]] # TODO x.best > 1
    messagef("Best Learner: %s", best.lrn$id)
    base.models[[i]] = train(best.lrn, new.task, subset = subset.idx)
    preds[[i]] = resample(best.lrn, new.task, resampling = .learner$resampling, 
      measures = .learner$measures, show.info = FALSE)
    # create new task
    if (bms.pt == "prob") {
      new.feat = getPredictionProbabilities(preds[[i]]$pred)
      # FIXME if new.feat is constant, NA then use the second pred!?
      # makeTaskWithNewFeat extra below
      new.task = makeTaskWithNewFeat(task = new.task, 
        new.feat = new.feat, feat.name = paste0("feat.", i))
    } else {
      new.feat = getPredictionResponse(preds[[i]]$pred)
      # FIXME if new.feat is constant, NA then use the second pred
      new.task = makeTaskWithNewFeat(task = new.task, 
        new.feat = new.feat, feat.name = paste0("feat.", i))
      }
  }
  # FIXME pred.train returns acc to bms.pt...is that correct?
  list(base.models = base.models, score = score[-1], final.task = new.task, pred.train = preds[[length(preds)]])
}


#' Predict Boosted Stacking object.
#' 
#' @param .learner Learner
#' @param .model Model.
#' @param .newdata New data.
#' @param ... ...
#' @export

predictLearner.BoostedStackingLearner = function(.learner, .model, .newdata, ...) {
  new.data = .newdata
  sm.pt = .learner$predict.type
  bms.pt = unique(extractSubList(.learner$model.multiplexer$base.learner, "predict.type"))
  
  td = getTaskDescription(.model)
  niter = length(.model$learner.model$base.models)
  for (i in seq_len(niter)) {
    newest.pred = predict(.model$learner.model$base.models[[i]], newdata = new.data)
    #FIXME for pred with response (or forbid it!?)
    # new.feat = getResponse(newest.pres) # is nicer
    if (bms.pt == "prob") {
      new.feat = getPredictionProbabilities(newest.pred)
      new.data = makeDataWithNewFeat(data = new.data, 
        new.feat = new.feat,
        feat.name = paste0("feat.", i), td)
    } else {
      new.feat = getPredictionResponse(newest.pred)
      new.data = makeDataWithNewFeat(data = new.data, 
        new.feat = new.feat, feat.name = paste0("feat.", i), td)
    }
  }
  if (sm.pt == "prob") {
    if (bms.pt == "prob") {
      return(as.matrix(getPredictionProbabilities(newest.pred, cl = td$class.levels)))
    } else { # if bms.pt="response" and sm.pt="prob" predict must be repeated
      last.model = .model$learner.model$base.models[[niter]]
      last.model$learner$predict.type = "prob"
      newest.pred = predict(last.model, newdata = new.data[, -ncol(new.data)]) #FIXMENOW
      return(as.matrix(getPredictionProbabilities(newest.pred, cl = td$class.levels)))
    }
  } else { #sm.pt = "response" and  bms.pt = "prob"/"response"
    return(getPredictionResponse(newest.pred)) #
  }
}


#' Creates x best learners from ModelMultiplexer TuneResult.
#' 
#' @param tune.result [\code{\link{TuneResult}}]\cr
#'   \code{TuneResult} from \code{ModelMultiplexer}
#' @param model.multiplexer [\code{\link{ModelMultiplexer}}]\cr
#'   Needed to exract fix parameters.
#' @param mm.ps [\code{list containing \link{ParamSet}}]\cr
#'   Needed to extract fix parameters.
#' @param x.best [\code{integer(1)}]
#'   Number of best learners to extract.
#' @param measure [\code{\link{Measure}}]\cr
#'   One Measure to apply to "extract from best".
#' @return [\code{list of x best learners}].
#' @examples 
#' \dontrun{
#' tsk = pid.task
#' lrns = list(
#'   makeLearner("classif.gbm", distribution = "bernoulli"),
#'   makeLearner("classif.randomForest", ntree = 3))
#' bpt = spt = "prob"
#' lrns = lapply(lrns, setPredictType, bpt)
#' mm = makeModelMultiplexer(lrns)
#' ctrl = makeTuneControlRandom(maxit = 3L)
#' ps = makeModelMultiplexerParamSet(mm,
#'   makeIntegerParam("n.trees", lower = 1L, upper = 3L),
#'   makeIntegerParam("interaction.depth", lower = 1L, upper = 2L),
#'   makeIntegerParam("ntree", lower = 1L, upper = 3L),
#'   makeIntegerParam("mtry", lower = 1L, upper = getTaskNFeats(tsk)))
#' tune.res = tuneParams(learner = mm, task = tsk, resampling = cv2, par.set = ps, control = ctrl)
#' best = makeXBestLearnersFromMMTuneResult(tune.result = tune.res, 
#'   model.multiplexer = mm, mm.ps = ps, x.best = 2, measure  =mmce)
#' best
#' }

makeXBestLearnersFromMMTuneResult = function(tune.result, model.multiplexer, mm.ps, x.best = 5, measure) {
  # checks
  assertClass(tune.result, "TuneResult")
  assertClass(model.multiplexer, "ModelMultiplexer")
  assertClass(measure, "Measure")
  assertInt(x.best, lower = 1)
  # body
  measure.name = paste0(measure$id, ".test.mean")
  opt.grid = as.data.frame(trafoOptPath(tune.result$opt.path), stringsAsFactors = FALSE)
  #FIXME minimize
  if (measure$minimize) {
    ord = order(opt.grid[, measure.name])[1:x.best]
  } else {
    ord = rev(order(opt.grid[, measure.name]))[1:x.best]
  }
  opt.grid = opt.grid[ord, ]
  j = sapply(opt.grid, is.factor)
  opt.grid[j] = lapply(opt.grid[j], as.character)
  # checks2
  if (NROW(opt.grid) < x.best) {
    stopf("'x.best' is %s and cannot be set larger than the number of tuning results in '%s", x.best, quote(tune.result))
  }
  #
  lrns = vector("list", x.best)
  for (i in 1:nrow(opt.grid)) {
    # get tuned parameter
    cl = as.character(opt.grid[i, 1])
    pars = opt.grid[i, grepl(pattern = cl, names(opt.grid)), drop = FALSE]
    pars.names.long = names(pars)
    pars.names = substr(names(pars), nchar(cl) + 2, nchar(names(pars)))
    names(pars) = pars.names
    pars.list = as.list(pars)
    # get and apply trafo
    #trafo = lapply(pars.names.long, function(x) mm.ps$pars[[x]]$trafo)
    #trafo = lapply(trafo, function(x) if(is.null(x)) {function(x) x} else x)
    #names(trafo) = pars.names
    #pars.list = Map(do.call, trafo, lapply(par.list[names(trafo)], list)) #mapply(do.call, trafo, lapply(par.list[names(trafo)], list))
    # get fix parameters, and final parameter set
    pars.list.fix = getHyperPars(model.multiplexer$base.learners[[cl]])
    idx = setdiff(names(pars.list.fix), names(pars.list))
    pars.fin = c(pars.list, pars.list.fix[idx])
    # apply all parameters
    lrns[[i]] = makeLearner(cl, id = paste(cl, i, sep = "_"),
      predict.type = tune.result$learner$predict.type,
      fix.factors.prediction = tune.result$learner$fix.factors.prediction,
      par.vals = pars.fin)
  }
  lrns
}


# Adds new feature(s) to task
makeTaskWithNewFeat = function(task, new.feat, feat.name) {
  # FIXME make me nicer
  assertClass(task, "Task")
  td = getTaskDescription(task)
  # check raw.data
  raw.data = getTaskData(task)
  
  if (class(new.feat) == "data.frame") {
    new.feat = new.feat[, -1, drop = FALSE]
    if (ncol(new.feat) > 1) 
      feat.name = paste(feat.name, td$class.levels[-1], sep = "_")
    data = cbind(raw.data, new.feat)
    data = data[, complete.cases(t(data))] 
    colnames(data)[(NCOL(raw.data)+1):NCOL(data)] = feat.name
  } else {
    data = cbind(raw.data, data.frame(new.feat))
    data = data[, complete.cases(t(data))] 
    colnames(data)[(NCOL(raw.data)+1)] = feat.name
  }
  if (td$type == "classif") {
    removeConstantFeatures(makeClassifTask(data = data, target = td$target, positive = td$positive))
  } else {
    removeConstantFeatures(makeRegrTask(data = data, target =  td$target))
  }
}

# Adds new feature(s) to data
makeDataWithNewFeat = function(data, new.feat, feat.name, task.desc) {
  assertClass(data, "data.frame")
  raw.data = data
  if (class(new.feat) == "data.frame") {
    new.feat = new.feat[, -1, drop = FALSE]
    if (ncol(new.feat) > 1) 
      feat.name = paste(feat.name, task.desc$class.levels[-1], sep = "_")
    data = cbind(raw.data, new.feat)
    colnames(data)[(NCOL(raw.data)+1):NCOL(data)] = feat.name
  } else {
    data = cbind(raw.data, data.frame(new.feat))
    colnames(data)[(NCOL(raw.data)+1)] = feat.name
  }
  return(data)
}

# S3
makeWrappedModel.BoostedStackingLearner = function(learner, learner.model, task.desc, subset, features, factor.levels, time) {
  x = NextMethod(x)
  addClasses(x, "BoostedStackingModel")
}


#' TODO
#' [ ] setPredictType.BoostedStackingLearner
#' [ ] makeBoostedStackingLearner: check if mm and ps fit together
#' [ ] Do we need an id?
#' [ ] Input checks: measure and type



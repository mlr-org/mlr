#' Resample a StackedLearner object.
#'
#' @description
#' Instead of rerunning `resample` with a new setting just use
#' `resampleStackedLearnerAgain`. `resampleStackedLearnerAgain` reuses
#' the already done work from a `ResampleResult`, i.e.
#' reuse fitted base models (needed for level 1 test data) and reuse level 1 training data.
#' Note: This function does not support resample objects with single broken base models (no error
#' handling implemented). Moreover models need to present (i.e. `save.preds = TRUE` in
#' `makeStackedLearner`). When using `save.on.disc = TRUE` in `makeStackedLearner`
#' resampling procedure `"Holdout"` is allowed only (model names are not unique
#' regarding CV fold number).
#' This function does four things internally (in that order) to obtain the new predictions:
#' \describe{
#' \item{1.}{Use saved base models (from `obj`) on test data to predict level 1 test data.
#' \item{2.}{Extract level 1 train data (from `obj`).
#' \item{3.}{Fit new super learner or apply new ensemble selection setting using level 1 train data from (2).}
#' \item{4.}{Apply model from (3) on level 1 test data from (1) to obtain final prediction.}
#' }
#' Following parameters need to be set for the single methods. For
#' \describe{
#' \item{method  = "superlearner"}{`super.learner` and `use.feat` need to be set.`
#' \item{method = "ensembleselection"}{`es.par.vals` need to be set.`
#' \item{method = "aggregate"}{no arguemnt of those abvoe need to be set.}
#' }
#' @param id (`character(1)`)\cr Unique ID for object.
#' @param obj [`ResampleResult`]\cr `ResampleResult` from `StackedLearner`.
#' @param task [`Task`].
#' @param measures [`Measure`].
#' @param super.learner [`Learner`]\cr New `super.learner` to apply.
#' @param use.feat (`logical(1)`)\cr Whether the original features should be passed to the super learner.
#' @param es.par.vals (`list`)\cr
#' List containing parameters for `ensembleselection`. See [`makeStackedLearner`].
#' @return Object of classes `RecombinedResampleResult` and `ResampleResult`.
#'   `RecombinedResampleResult` differ from classical `ResampleResult` in that way, that it
#'   contains parameters from the StackedLearner (i.e. super.learner, use.feat, es.par.vals),
#'   but has no error handling (err.msgs = NULL) and no extract functionality (extract = NULL).
#'   The returned values of 'pred' as well as 'models' differ as well.
#'   Moreover the performance of the base models evaluated on the test set is accessable in 'test.bls.perfs'.
#' @examples
#' \dontrun{
#' tsk = pid.task
#' # Base learners need unique names (id)
#' bls = list(makeLearner("classif.kknn"),
#'   makeLearner("classif.randomForest"),
#'   makeLearner("classif.rpart", id = "rp1", minsplit = 5),
#'   makeLearner("classif.rpart", id = "rp2", minsplit = 10),
#'   makeLearner("classif.rpart", id = "rp3", minsplit = 15),
#'   makeLearner("classif.rpart", id = "rp4", minsplit = 20),
#'   makeLearner("classif.rpart", id = "rp5", minsplit = 25)
#' )
#' # For classification predict.type = "prob" might lead to better results.
#' bls = lapply(bls, function(x) setPredictType(x, predict.type = "prob"))
#' ste = makeStackedLearner(id = "stack", bls, resampling = cv3,
#'   predict.type = "prob", method = "ensembleselection", es.par.vals = list(init = 1,
#'   bagprop = 0.5, bagtime = 3, metric = mmce), save.on.disc = FALSE)
#' # To use resampleStackedLearnerAgain
#' # - cross validation in outer resampling can be used only if 'save.on.disc = FALSE'
#'   in resampleStackedLearnerAgain,
#' # - in resample 'models = TRUE' must be set.
#' res = resample(ste, tsk, cv2, models = TRUE)
#' re2 = resampleStackedLearnerAgain(obj = res, task = tsk,
#'   es.par.vals = list(init = 2, bagtime = 15))
#' re3 = resampleStackedLearnerAgain(obj = res, task = tsk, measures = list(mmce, auc),
#'   es.par.vals = list(bagprop = .2, bagprop = 10, metric = auc))
#' re3 = resampleStackedLearnerAgain(obj = res, task = tsk, measures = mmce,
#'   es.par.vals = list(replace = FALSE, init = 2, bagprop = .2))
#' re4 = resampleStackedLearnerAgain(obj = res, task = tsk, measures = mmce,
#'   super.learner = bls[[2]], use.feat = TRUE)
#' sapply(list(res, re2, re3, re4), function(x) x$aggr)
#'
#' # Compare running time of idential settings
#' ste2 = makeStackedLearner(id = "stack", bls, resampling = cv3,
#'   predict.type = "prob", method = "ensembleselection",
#'   es.par.vals = list(init = 2, bagtime = 15))
#' res2 = resample(ste2, tsk, cv2, models = TRUE)
#' sapply(list(res2, re2), function(x) x$runtime)
#' }
#' @export
#' #' @importFrom utils stack
resampleStackedLearnerAgain = function(id = NULL, obj, task, measures = NULL,
  super.learner = NULL, use.feat = NULL, es.par.vals = NULL) {
  # checks
  if (is.null(id))
    id = paste("recombined", super.learner$id, collapse = ".") #TODO what about ES uniq name
  assertClass(id, "character")
  assertClass(obj, "ResampleResult")
  assertClass(task, "Task") # TODO check if tasks from obj and tasks fits
  if (is.null(measures))
    measures = list(getDefaultMeasure(task))
  if (class(measures) == "Measure")
    measures = list(measures)
  lapply(measures, function(x) assertClass(x, "Measure"))
  if (!is.null(super.learner)) {
    assertClass(super.learner, "Learner")
    if (is.null(use.feat)) {
      use.feat = FALSE
    }
    assertLogical(use.feat)
    assertClass(es.par.vals, "NULL")
  }
  if (!is.null(es.par.vals)) {
    assertClass(es.par.vals, "list")
  }
  # method
  org.method = obj$models[[1]]$learner$method
  if (org.method == "aggregate") {
    stopf("Method %s needs cross validated predictions from method 'superlearner.' or
      'ensembleselection' in resample, which is not the case for used method 'aggregate'.", method)
  }
  if (!is.null(super.learner)) {
    assertClass(super.learner, "Learner")
    method = "superlearner"
    if (!is.null(es.par.vals)) stopf("Method 'superlearner' does not need argument 'es.par.vals'.")
  } else {
    if (!is.null(es.par.vals)) {
      assertClass(es.par.vals, "list")
      method = "ensembleselection"
    } else {
      method = "aggregate"
    }
  }

  # setup
  type = getTaskType(task)
  tn = getTaskTargetNames(task)
  bls.length = length(obj$models[[1]]$learner.model$base.models)
  bls.names = names(obj$models[[1]]$learner.model$base.models)
  bm.pt = unique(unlist(lapply(seq_len(bls.length), function(x)
    obj$models[[1]]$learner$base.learners[[x]]$predict.type)))
  folds = length(obj$models)
  if (length(bm.pt) > 1) stopf("All Base Learner must be of the same predict.type.")
  task.size = getTaskSize(task)
  save.on.disc = obj$models[[1]]$learner$save.on.disc

  time1 = Sys.time()

  # get base models (for save.on.disc=FALSE: WrappedModels; for TRUE: names directing to RData on disc.)
  base.models_f = lapply(seq_len(folds), function(x) obj$models[[x]]$learner.model$base.models)
  # get train and test idxs
  train.idxs_f = lapply(seq_len(folds), function(x) obj$models[[x]]$subset)
  test.idxs_f = lapply(seq_len(folds), function(x) setdiff(seq_len(task.size), train.idxs_f[[x]]))
  #
  test.level1.task_f = vector("list", length = folds)
  test.bls.perfs_f = vector("list", length = folds)

  ### superlearner ###
  if (method == "superlearner") {
    ### 1.
    ### Get level 1 TEST data, i.e. apply bls models from training on testing data
    # saved in "test.level1.task_f"
    for (f in seq_len(folds)) {
      test.idx = test.idxs_f[[f]]
      pred.list = createPreds(fold.i = f, bls = base.models_f[[f]], idx = test.idx, task, save.on.disc)
      #test.bls.perfs_f[[f]] = ldply(lapply(pred.list, function(x) performance(x, measures)), .id = "bls")
      test.bls.perfs_f[[f]] = utils::stack(lapply(pred.list, function(x) performance(x, measures))) # names: value,ind
      pred.data.list = lapply(seq_len(length(pred.list)),
        function(x) getPredictionDataNonMulticoll(pred.list[[x]]))
      names(pred.data.list) = bls.names
      if (use.feat) {
        feat = getTaskData(task, subset = test.idx, target.extra = TRUE)$data
        pred.data.list[["feat"]] = feat
      }
      pred.data.list[[tn]] = getTaskTargets(task)[test.idx]
      pred.data = as.data.frame(pred.data.list)
      test.level1.task_f[[f]]  = createTask(type, data = pred.data, target = tn)
      rm(pred.data.list, pred.data)
    }
    ### 2.
    ### Get level 1 TRAIN data
    # preds saved in "train.level1.preds_f" (one list entry per fold)
    # task saved as "train.level1.task_f" (one task per fold)
    train.level1.preds_f = lapply(obj$models, function(x) x$learner.model$pred.train)
    train.level1.task_f = vector("list", length = folds)
    for (f in seq_len(folds)) {
      train.idx = train.idxs_f[[f]]
      pred.data.list = lapply(seq_len(bls.length),
        function(x) getPredictionDataNonMulticoll(train.level1.preds_f[[f]][[x]]))
      names(pred.data.list) = bls.names
      if (use.feat) {
        feat = getTaskData(task, subset = train.idx, target.extra = TRUE)$data
        pred.data.list[["feat"]] = feat
      }
      pred.data.list[[tn]] = getTaskTargets(task)[train.idx]
      pred.data = as.data.frame(pred.data.list)
      train.level1.task_f[[f]] = createTask(type, pred.data, target = tn, id = paste0("fold", f))
    }
    ### 3
    ### Fit super.learner on every fold
    #+++ here tuneParam could be applied +++
    train.supermodel_f = lapply(seq_len(folds),
      function(f) train(super.learner, task = train.level1.task_f[[f]]))
    train.preds_f = lapply(seq_len(folds),
      function(f) predict(train.supermodel_f[[f]], task = train.level1.task_f[[f]]))
    ### 4.
    ### Apply train.supermodel from line above on level 1 TEST preds from (1).
    test.preds_f = lapply(seq_len(folds),
      function(f) predict(train.supermodel_f[[f]], task = test.level1.task_f[[f]]))
    res.model_f = train.supermodel_f
    ###
    ###
    ### ensembleselection and aggregate ###
    ###
    ###
  } else if (method %in% c("aggregate", "ensembleselection")) {
    # use settings from new es.par.vals
    if (method == "ensembleselection") {
      es.par.vals = createNewEsParVals(org.es.par.vals = obj$models[[1]]$learner$es.par.vals,
        new.es.par.vals = es.par.vals)
    }
    ### 1.
    ### get level 1 TRAIN preds and perfs
    # saved as "train.level1.preds"
    # saved as "train.level1.perfs" (only for ensembleselection)
    if (method == "ensembleselection") {
      train.level1.preds_f = lapply(seq_len(folds),
        function(x) obj$models[[x]]$learner.model$pred.train) # cross-validated
      train.level1.perfs_f = lapply(seq_len(folds),
        function(x) obj$models[[x]]$learner.model$bls.performance)
    } else {
      train.level1.preds_f = vector("list", folds) # see for-loop
    }
    #
    train.preds_f = test.preds_f = vector("list", length = folds)
    res.model_f = vector("list", length = folds)
    for (f in seq_len(folds)) {
      ###  1. for aggregate
      if (method == "aggregate") {
        train.level1.preds_f[[f]] = createPreds(fold.i = f, bls = base.models_f[[f]],
          idx = train.idxs_f[[f]], task, save.on.disc) #train-predict
      }
      ### 2.
      ### get level 1 TEST preds, i.e. apply bls models from training on testing data
      # saved as "test.level1.preds"
      test.level1.preds = createPreds(fold.i = f, bls = base.models_f[[f]], idx = test.idxs_f[[f]], task, save.on.disc)
      test.bls.perfs_f[[f]] = stack(lapply(test.level1.preds, function(x) performance(x, measures)))
      ### 3.
      ### Run Ensemble Selection on level 1 TRAIN preds (1) with new parameters.
      if (method == "ensembleselection") {
        res.model_f[[f]] = applyEnsembleSelection(pred.list = train.level1.preds_f[[f]],
          bls.performance = train.level1.perfs_f[[f]], es.par.vals = es.par.vals)
        freq = res.model_f[[f]]$freq
      } else {
        res.model_f[[f]] = "Method 'aggregate' does not have a model."
      }
      # Create train prediction (needes for train measure)
      current.pred.list = train.level1.preds_f[[f]] # names(current.pred.list) = bls.names
      if (method == "ensembleselection") current.pred.list = expandPredList(current.pred.list, freq = freq)
      train.preds_f[[f]] = aggregatePredictions(pred.list = current.pred.list, pL = FALSE)

      ### 4.
      ### Apply Model (i.e. freq) on level 1 TEST preds from (3)
      if (method == "ensembleselection") test.level1.preds = expandPredList(test.level1.preds, freq = freq)
      test.preds_f[[f]] = aggregatePredictions(pred.list = test.level1.preds, pL = FALSE)
    }
  }
  ###
  ###
  ### measures and runtime
  ###
  ###
  m.train = lapply(train.preds_f, function(x) performance(x, measures = measures))
  measures.train = as.data.frame(do.call(rbind, m.train))
  measures.train = cbind(iter = 1:NROW(measures.train), measures.train)

  m.test = lapply(test.preds_f, function(x) performance(x, measures = measures))
  measures.test = as.data.frame(do.call(rbind, m.test))
  measures.test = cbind(iter = 1:NROW(measures.test), measures.test)
  aggr = colMeans(measures.test[, -1, drop = FALSE])
  names(aggr) = paste0(names(aggr), ".test.mean")

  time2 = Sys.time()
  runtime = as.numeric(difftime(time2, time1, "sec"))

  ### return
  X = list(learner.id = id,
    task.id = task$task.desc$id,
    measures.train = measures.train,
    measures.test = measures.test,
    aggr = aggr,
    train.preds = train.preds_f, # rarely needed, only for getStackedBLPreds if .newdata=NULL
    pred = test.preds_f, # not 1 ResampleRediction object as in resample but a list of single ResamplePredictions.
    models = res.model_f, # Ensemble model consist of 'freq', 'freq.list', 'weights'; Super Learner of super model
    err.msgs = NULL, # no error handling so far.
    extract = NULL, # not implemented.
    runtime = runtime,
    # extra returns
    test.bls.perfs = test.bls.perfs_f, # Performance of single bls for easier interpretability if ensemble was successful.
    super.learner = super.learner,
    use.feat = use.feat,
    es.par.vals = es.par.vals)
  class(X) = c("RecombinedResampleResult", "ResampleResult")
  return(X)
}


###############################################################################
# helpers
###############################################################################

# Create a classif or regr Task.
#
# @param type [`character(1)`]\cr Use "classif" for classification and "regr" for regression.
# @param data [`data.frame`]\cr  Data to use.
# @param target [`character(1)`]\cr Target to use.
# @param id [`character(1)`] Task id.
createTask = function(type, data, target, id = deparse(substitute(data))) {
  if (type == "classif") {
    task = makeClassifTask(id = id, data = data, target = target)
  } else if (type == "regr") {
    task = makeRegrTask(id = id, data = data, target = target)
  }
  task
}

# Get precise type of the task, i.e. distinguish between "classif" (binary target),
# "multiclassif" and all other types.
#
# @param x `Task} or `TaskDesc`.
getPreciseTaskType = function(x) {
  if (any(class(x) == "Task"))
    x = getTaskDescription(x)
  type = getTaskType(x)
  if (type == "classif" & length(x$class.levels) > 2)
    type = "multiclassif"
  type
}


# Create predictions for training or testing set (depends on idx)
# (with base model or saved one in RDS).
#
# @param fold.i Current fold number.
# @param bls base.learner to use.
# @param idx idx for subsetting.
# @param task task.
# @param save.on.disc wether model are present in `bls` or must be loaded using readRDS.
createPreds = function(fold.i, bls, idx, task, save.on.disc) {
  bls.len = length(bls)
  if (save.on.disc) {
    # This only works if outer resampling is Holdout (save model does not
    # get infos about the fold figure, therefore only one fold is allowed):
    if (fold.i != 1) {
      stopf("Using 'save.on.disc = TRUE' and outer resampling strategies others
        than Holdout is not supported. Switch save.on.disc to FALSE or use Holdout.")
    }
    all.preds = vector("list", length = bls.len)
    for (b in seq_len(bls.len)) { # do it seqentially
      bm = readRDS(bls[[b]]) # i is always 1
      all.preds[[b]] = predict(bm, subsetTask(task, idx))
      names(all.preds)[b] = bm$learner$id
      rm(bm)
    }
  } else { # save.on.disc = FALSE
    all.preds = lapply(seq_len(bls.len), function(b) predict(bls[[b]], subsetTask(task, idx)))
    all.preds.names = unlist(lapply(seq_len(bls.len), function(b) bls[[b]]$learner$id))
    names(all.preds) = all.preds.names
  }
  all.preds
}


# Create new `es.par.vals`.
#
# @param org.es.par.vals orginal/old es.par.vals from obj.
# @param new.es.par.vals parameters which should be updated.
createNewEsParVals = function(org.es.par.vals, new.es.par.vals) {
  org.keep = setdiff(names(org.es.par.vals), names(new.es.par.vals))
  org.es.par.vals = org.es.par.vals[org.keep]
  final.es.par.vals = c(org.es.par.vals, new.es.par.vals)
  allowed =  c("replace", "init", "bagprop", "bagtime", "maxiter", "tolerance", "metric")
  unallowed = setdiff(names(new.es.par.vals), allowed)
  if (length(unallowed) > 0)
    stopf("'%s' is no an allowed argument for es.par.vals.", unallowed)
  final.es.par.vals
}


# Print `RecombinedResampleResult`.
#
# @param x RecombinedResampleResult
# @param ... ...
#' @export
print.RecombinedResampleResult = function(x, ...) {
  cat("Recombined Resample Result\n")
  catf("Task: %s", x$task.id)
  catf("Learner: %s", x$learner.id)
  m = x$measures.test[, -1L, drop = FALSE]
  Map(function(name, x, aggr) {
    catf("%s.aggr: %.2f", name, aggr)
    catf("%s.mean: %.2f", name, mean(x, na.rm = TRUE))
    catf("%s.sd: %.2f", name, sd(x, na.rm = TRUE))
  }, name = colnames(m), x = m, aggr = x$aggr)
  catf("Runtime: %g", x$runtime)
  invisible(NULL)
}


# TODO
#
# [ ] allow "extract = onlybase.models" in resample (not only compulsory
#   models = TRUE) to save more memory.
# [ ] allow idx arguments that a special fraction of base learners will be used
#   (redo resampling with another base learners setting and use the alreday fitted
#   models -- so you just have to run a (big) number of base leaernes once and
#   then make use of base learners you want to use).
# [ ] allow tuneParam so that superlearner can be tuned. Should be easy.
# [ ] run in parallel (but maybe a outer parallelization is more usefull).
# [x] add aggregate method
# Nomenclature:
# 'Singular' indicates objects which contain only one object (e.g. one model from
#   base models list from one fold).
# 'Plural' indicates a list of objects (e.g. list of learners, models, data sets).
# List ends with '_f' if information from all f folds are saved in that object
#   (may be a list of lists).

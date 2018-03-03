#' @title Creates the measure Area under the Mass-Volume Curve (AMV) for Anomaly detection (oneclass) for high dimensional data
#'
#' @description
#' Creates a measure for oneclass classification on high dimensional data
#' (recommend for dimension greater than 8) called AMVhd, which is based on the
#' Area under the Mass-Volume Curve (AMV) (see \code{makeAMVMeasure}).
#' The basic idea is to do several feature subsamplings (of dimension less than 8)
#' to reduce the dimension of the data, therefore AMV can be applied on each
#' subsamples, yielding partial scores AMV_k. The mean of the partial scores is
#' the new performancecriteria AMVhd.
#' @param amv.feats [\code{numeric}] \cr
#' Number of features to be drawn in the feature subsamples.
#' Default is 5.
#' @param amv.iters [\code{numeric}] \cr
#' Number of subsamples.
#' Default is 50.
#' @note the ID of the learner have to to be the default id!
#' @return [\code{numeric(1)}]
#'   Area under the Mass-Volume Curve (AMV) for high dimensional data.
#' @references Nicolas, G. How to Evaluate the Quality of Unsupervised Anomaly Detection Algorithms,
#' arXiv preprint arXiv:1607.01152
#' @inheritParams makeMeasure
#' @inheritParams makeAMVMeasure
#' @template ret_measure
#' @export
#' @family performance.
#' @examples
#' # create anomaly data with feature size nine
#' sigma = matrix(0, 9, 9)
#' diag(sigma) = c(4, 5, 8, 3, 2, 6, 9, 3, 1)
#' normal = MASS::mvrnorm(n = 1000, rep(0, 9), sigma)
#' colnames(normal) = paste0("V", 1:9)
#' normal = as.data.frame(normal)
#' normal$normal = TRUE
#'
#' anomaly = matrix(sample(size = 50 * 9, x = 20:100, replace = TRUE), 50, 9)
#' colnames(anomaly) = paste0("V", 1:9)
#' anomaly = as.data.frame(anomaly)
#' anomaly$normal = FALSE
#' data = rbind(normal, anomaly)
#' data = na.omit(data)
#'
#' # create train and test sets
#' library(BBmisc)
#' inds.split = BBmisc::chunk(seq_len(nrow(data)), shuffle = TRUE, props = c(0.6, 0.4))
#' train.inds = inds.split[[1]]
#' test.inds = inds.split[[2]]
#'
#' # create an AMVhd measure which calculates the Area under the Mass-Volume Curve between 0.8 and 0.99
#' # with 50 steps for high dimensional data.
#' AMVhd = makeAMVhdMeasure(id = "AMV", minimize = TRUE, alphas = c(0.8, 0.99),
#' n.alpha = 50, n.sim = 10e3, best = 0, worst = NULL)
#'
#' task = makeOneClassTask(data = data, target = "normal", positive = "TRUE", negative = "FALSE")
#' lrn = makeLearner("oneclass.svm", predict.type = "prob")
#' mod = train(lrn, task, subset = train.inds)
#' pred = predict(mod, task, subset = test.inds)
#'
#' # calculate AMVhd performance
#' set.seed(123)
#' performance(pred = pred, measures = list(AMVhd), model = mod,
#' task = task, feats = data[test.inds, 1:9])

makeAMVhdMeasure = function(id = "AMVhd", minimize = TRUE, amv.iters = 50, amv.feats = 5, alphas = c(0.9, 0.99), n.alpha = 50, n.sim = 1e3, best = 0, worst = NULL, name = id, note = "") {
  assertString(id)
  assertFlag(minimize)
  assertNumeric(alphas, lower = 0, upper = 1)
  assertCount(n.alpha)
  assertCount(n.sim)
  assertString(name)
  assertString(note)

  makeMeasure(id = id, minimize = minimize, extra.args = list(alphas = alphas, n.sim = n.sim, amv.iters = amv.iters, amv.feats = amv.feats, n.alpha = n.alpha),
    properties = c("oneclass", "req.model", "req.pred", "predtype.prob", "req.feats"),
    best = best, worst = worst,
    fun = function(task, model, pred, feats, extra.args) {
      alphas = extra.args[[1]]
      n.sim = extra.args[[2]]
      amv.iters = extra.args[[3]]
      amv.feats = extra.args[[4]]
      n.alpha = extra.args[[5]]
      measure.amv = makeAMVMeasure(id = "AMV", minimize = minimize, alphas = alphas, n.alpha = n.alpha, n.sim = n.sim, best = best, worst = worst, name = id)

      data = getTaskData(task, target.extra = TRUE)$data
      train.inds = model$subset
      test.inds = setdiff(seq_row(data), train.inds)
      if (length(test.inds) == 0) stop("Pass argument subset in the train model.")

      if (model$learner$id %nin% listLearners(task)$class) {
        lrn.id = model$learner$id
        if(grepl(".AMVhd", lrn.id)) lrn.id = gsub(".AMVhd", "",lrn.id)
        if(grepl(".tuned", lrn.id)) lrn.id = gsub(".tuned", "",lrn.id)
      } else {
        lrn.id = model$learner$id
      }

      lrn.amv = makeLearner(lrn.id, predict.type = "prob", par.vals = model$learner$par.vals)
      lrn.amvw = makeAMVhdWrapper(lrn.amv, amv.iters = amv.iters, amv.feats = amv.feats)
      # wrapped model
      mod.amvw = train(lrn.amvw, task, subset = train.inds)
      # wrapped prediction
      pred.amvw = predict(mod.amvw, task, subset = test.inds)

      measure.amv = makeAMVMeasure(id = "AMV", minimize = minimize, alphas = alphas,
        n.alpha = n.alpha, n.sim = n.sim, best = best, worst = worst, name = id)

      # get the prediction of submodels, which has sampled features
      subpred = attr(pred.amvw, "AMVhdSubpredict")
      submod = getLearnerModel(mod.amvw, more.unwrap = FALSE)
      #delete full model before calculating the amv on each submodel
      submod = submod[-1]
      # calculate amv for each submodel
      amv.k = vector()
      for (i in seq_len(length(submod))) {
        amv.k[i] = performance(pred = subpred[[i]], measures = list(measure.amv),
          model = submod[[i]], feats = feats[, submod[[i]]$features])
      }
      # Return area under the mass-volume curve
      amv = mean(amv.k)
    },
    name = name,
    note = note
  )
}

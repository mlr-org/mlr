#' @title Creates the weighted accuracy measure
#'
#' @description
#' Creates a measure for imbalanced data or anomaly detection with externel ground truth (known label).
#' The measure wac is similar to the balanced accuracy rate (bac).
#' Bac is defined as the mean of true positive rate and true negative rate and wac just ads weights to each rates.
#' For weights $w = 0.5$ the wac is resulted in the bac. The main purpose is to be able to give the true
#' positive weight (tpr) a greater meaning than the true negative rate (tnr), as in anomaly detection the
#' detection of anomaly (positive class) is more of an interests than the detection of the normal class (negative class).
#'
#' @param id [\code{character(1)}]\cr
#'   Name of measure.
#'   Default is \dQuote{costs}.
#' @param w [\code{numeric}] \cr
#'   Weight for the positive class. The weight of the negative class is 1-w.
#' @return [\code{numeric(1)}]
#'  Weighted accuracy (wac)
#' @inheritParams makeMeasure
#' @template ret_measure
#' @export
#' @family performance.
#' @examples
#' # creates an WAC measure which calculates weightes accuracy with
#' # weights = 0.6 for the positive (anomaly) class
#' wac = makeWACMeasure(id = "wac", minimize = FALSE, best = 0, worst = NULL, w = 0.6)
#'
#' data = getTaskData(oneclass2d.task)
#' inds.split = BBmisc::chunk(seq_len(nrow(data)), shuffle = TRUE, props = c(0.6, 0.4))
#' train.inds = inds.split[[1]]
#' test.inds = inds.split[[2]]
#' lrn = makeLearner("oneclass.svm", predict.type = "prob")
#' mod = train(lrn, oneclass2d.task, subset = train.inds)
#' pred = predict(mod, oneclass2d.task, subset = test.inds)
#'
#' # calculate performance for prediction object, pass data of features used for
#' # prediction as feats in performance
#' performance(pred = pred, measures = list(wac), model = mod, task = task)


makeWACMeasure = function(id = "WAC", minimize = FALSE, best = 1, worst = 0, name = id, note = "", w = 0.5) {

  assertString(id)
  assertFlag(minimize)
  assertNumeric(w, lower = 0, upper = 1)
  assertString(name)
  assertString(note)

  makeMeasure(id = id, minimize = minimize, extra.args = list(w),
    properties = c("oneclass", "classif", "req.pred", "req.truth"),
    best = best,
    worst = worst,
    fun = function(task, model, pred, feats, extra.args) {
      truth = pred$data$truth
      positive = pred$task.desc$positive
      negative = pred$task.desc$negative
      response = pred$data$response
      weight.positive = extra.args[[1]]

      if (!(0 <= weight.positive & weight.positive <= 1))
        stop("Weight for the positive class for the weights accuracy must be an element in [0,1].")
      weight.negative = 1 - weight.positive

      denom.positive = sum(truth == positive)
      denom.negative = sum(truth == negative)

      summand.positive = ifelse(denom.positive == 0, 0,  weight.positive * measureTP(truth, response, positive) / denom.positive)
      summand.negative = ifelse(denom.negative == 0, 0,  weight.negative * measureTN(truth, response, negative) / denom.negative)

      sum(c(summand.positive, summand.negative))
    },
    name = name,
    note = note
  )
}

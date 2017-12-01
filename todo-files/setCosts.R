#' @title Determine cost-optimal prediction object.
#'
#' @description
#' Determine cost-optimal prediction object for classification.
#' Creates corresponding discrete class response for a cost matrix, where the class
#' with lowest expected cost is predicted.
#' For binary classification: The corresponding optimal threshold values are calculated from the cost matrix.
#' For multiclass: For costs matrices with row-wise equal off-diagonal elements, an
#' optimal threshold vector exists and is returned.
#' Otherwise, the threshold vector is set to \code{NA}.
#'
#' @template arg_pred
#' @param costs [\code{matrix}]\cr
#'   Matrix of misclassification costs. Rows and columns have to be named with class labels, order does not matter.
#'   Rows indicate true classes, columns predicted classes.
#' @return [\code{\link{Prediction}}] with changed threshold and corresponding response.
#' @export
#' @seealso \code{\link{predict.WrappedModel}}, \code{\link{setThreshold}}
#' @examples
#' ## create task and train learner (LDA)
#' task = makeClassifTask(data = iris, target = "Species")
#' lrn = makeLearner("classif.lda", predict.type = "prob")
#' mod = train(lrn, task)
#'
#' ## predict probabilities and compute performance
#' pred = predict(mod, newdata = iris)
#' performance(pred, measures = list(iris.costs, mmce))
#' head(as.data.frame(pred))
#'
#' ## generate cost matrix and corresponding performance measure
#' costs = matrix(c(0, 50, 5, 30, 0, 2, 80, 4, 1), 3)
#' colnames(costs) = rownames(costs) = getTaskDescription(task)$class.levels
#' iris.costs = makeCostMeasure(id = "iris.costs", costs = costs, task = iris.task,
#'   best = 0, worst = 10)
#'
#' ## predict probabilities and cost-optimal response
#' pred = setCosts(pred, costs)
#' performance(pred, measures = list(iris.costs, mmce))
#' head(as.data.frame(pred))

setCosts = function(pred, costs) {
  assertClass(pred, classes = "Prediction")
  assertMatrix(costs, any.missing = FALSE)
  td = pred$task.desc
  if (td$type != "classif") 
    stop("Costs can only be set for classification predictions!")
  if (pred$predict.type != "prob") 
    stop("Costs can only be set for predict.type 'prob'!")
  levs = td$class.levels
  if (any(dim(costs))) {
    if (any(dim(costs) != length(levs))) 
      stop("Dimensions of costs have to be the same as number of class levels!")
    rns = rownames(costs)
    cns = colnames(costs)
    if (!setequal(rns, levs) || !setequal(cns, levs)) 
      stop("Row and column names of cost matrix have to equal class levels!")
  }
  p = getPredictionProbabilities(pred, cl = levs)
  # resort rows and columns of cost matrix so we have same order as in p
  costs = costs[levs, levs]
  # simplify costs
  costs = costs - diag(costs)
  # calculate expected costs and determine cols with minimum el
  p = as.matrix(p) %*% costs
  ind = getMaxIndexOfRows(-p)
  pred$data$response = factor(ind, levels = seq_along(levs), labels = levs)
  ncl = length(levs)
  if (ncl == 2L) {
  	threshold = costs[td$negative, td$positive]/(costs[td$negative, td$positive] + costs[td$positive, td$negative])
  	threshold = c(threshold, 1 - threshold)
  	names(threshold) = c(td$positive, td$negative)
  } else {
  	# check row-wise equality of off-diagonal elements
  	u = apply(costs, 1, function(x) length(unique(x)))
  	if (all(u < 3)) {
		threshold = (ncl - 1)/rowSums(costs)
		threshold = threshold/sum(threshold)
  	} else {
  		threshold = rep_len(NA, ncl)
  	}
  	names(threshold) = levs
  }
  pred$threshold = threshold
  return(pred)
}

#' @title Method for converting anomaly scores to probability estimates.
#' @description
#' The \code{\link{convertingScoresToProbability}} function converts anomaly scores
#' to probability estimates with the sigmoid function (calibration function)
#' 1 / (1 + exp(-(A + B * score))) and A = 0, B = 1. The higher the probability
#' estimate the more likely the observation belongs to the anomaly class.
#' NOTE: In the referenced paper the authors suggest to use the sigmoid function
#' for converting anomaly scores to probability and applying the EM-Algorithm to
#' find suitable values for parameters A and B. However this approach struggle
#' with convergence problems. After a wide search and talking to some experts in
#' this field, we couldn't find a appropriate default converting method. But all
#' provided measurements in mlr depends only on the anomaly scores or are invariant
#' to monotone increasing transformations. Therefore we are using the above sigmoid
#' function, for now with default parameter A = 0, B = 1 with the goal to normalized
#' the data to the intervall [0, 1] to enable the user to use all beneficial
#' functions of mlr. This function will be updated in the future.
#'
#' \describe{
#' \item{anomaly.score [\code{numeric(1)}]}{ A numeric vector of anomaly score}
#' \item{param [\code{numeric}]}{A vector of values for the parameter A and B of the sigmoid function.}
#' }
#' @references Gao, Jing, and Pang-Ning Tan. "Converting output scores from outlier detection algorithms into probability estimates." Data Mining, 2006. ICDM'06. Sixth International Conference on. IEEE, 2006.
#' @name oneclassProbability
#' @seealso \code{\link{convertingScoresToProbability}}
#' @rdname oneclassProbability
NULL


#' @title Convert anomaly scores to probability estimates.
#'
#' @description
#' Convert anomaly scores to probability estimates with the sigmoid function
#' (calibration function) 1 / (1 + exp(-(0 + 1 * score))). The higher the probability
#' estimate the more likely the observation belongs to the anomaly class.
#' For more information see \code{\link{oneclassProbability}}.
#'
#' @param anomaly.score a numeric vector of anomaly scores.
#' @param param a vector of values for the parameter A and B of the sigmoid function.
#' Default ist A = 0 and B = 1
#' @seealso \code{\link{oneclassProbability}}
#' @return [\code{vector}] with probabilities as entries.
#' @references Gao, Jing, and Pang-Ning Tan. "Converting output scores from outlier detection algorithms into probability estimates." Data Mining, 2006. ICDM'06. Sixth International Conference on. IEEE, 2006.
#' @examples
#' Data = oneclass2d.task$env$data # getTaskData(oneclass2d.task)
#' svm.model = e1071::svm(Data[,1:2], y = NULL, type = 'one-classification',
#' kernel = "radial", nu = 0.05)
#  svm.pred = predict(svm.model, Data[,1:2])
#' dv = svm.model$decision.values
#' prob = convertingScoresToProbability(dv, param = c(0, 1))$probability
#' o = order(prob)
#' col = factor(Data$Target, levels = c("Normal", "Anomaly"), labels =  c("black", "red"))
#' plot(1:length(prob), prob[o], col = col[o])
#' @export
convertingScoresToProbability = function(anomaly.score, param = c(0, 1)){

  f = anomaly.score
  list = list()

  if (length(param) != 2) {
    stop("Too little/many starting parameters for optimization.")
  }

  # probability for outlier given the scores
  prob.outlier = function(p, score) {
    1 / (1 + exp(-p[2] * score - p[1]))
  }

  list$p = param
  list$probability = prob.outlier(param, f)
  list
}

#' @title Convert anomaly scores to probability estimates.
#'
#' @description Convert anomaly scores to probability estimates with the sigmoid function (calibration function). The higher the probability estimate the more likely the observation belongs to the normal class.
#'
#' @param anomaly.score a numeric vector of anomaly scores.
#' @param parainit a vector of starting values for the optimizer
#' @param max.iter Maximum number of iterations. Defaults to 100
#' @return [\code{vector}] with probabilities as entries.
#' @export
#' @references Gao, Jing, and Pang-Ning Tan. "Converting output scores from outlier detection algorithms into probability estimates." Data Mining, 2006. ICDM'06. Sixth International Conference on. IEEE, 2006.
#' @examples
#'
#' Data = iris[, 1:4] # find better dataset later
#' svm.model <- e1071::svm(Data, y = NULL, type = 'one-classification', kernel = "radial", nu = 0.05)
#  svm.pred <- predict(svm.model, Data)
#' dv = svm.model$decision.values
#' prob = convertingScoresToProbability(dv, parainit = c(0, 1))
#' plot(1:length(prob), prob, ylim = c(0, 1))
#'
convertingScoresToProbability = function(anomaly.score, parainit = NULL, max.iter = 100, optim.method = "Newton"){

  assertChoice(optim.method, c("trust region", "BFGS", "glm", "Newton"))
  f = anomaly.score
  p = parainit
  loop = TRUE
  sum.help = rep(1, length(f))
  list = list()

  if (is.null(p)) {
    p = c(B = 0, A = 1)
    messagef("Starting parameter values are missing.
      For calibration using sigmoid function the default is: \n A = 1, B = 0")
  }
  if (length(p) != 2) {
    stop("Too little/many starting parameters for optimization.")
  }

  # probability for outlier given the scores
  prob.outlier = function(p, score) {
    1 / (1 + exp(-p[2] * f - p[1]))
  }

  if (optim.method == "trust region") {
    while (loop) {
      t =  prob.outlier(p, f)
      # LL negative log likelihood
      LL = function(p) { t((1-t)) %*% (p[2] * f + p[1])
        + sum.help %*% log(1 + exp(-p[2] * f - p[1])) }
      # first derivative for A and B
      gA = function(p) { t(f) %*% ( (1-t) - (exp(-p[2] * f - p[1]) / (1 + exp(-p[2] * f - p[1])))) }
      gB = function(p) { sum.help %*% ((1-t) - (exp(-p[2] * f - p[1]) / (1 + exp(-p[2]*f - p[1])))) }
      g = function(p) {
        c(gA(p), gB(p))
      }

      # trust region optimization
      optim = trustOptim::trust.optim(p, fn = LL, gr = g,  method = "BFGS",
        control = list(report.level = 0, maxit = max.iter))

      pnew = optim$solution

      # check if pnew is converging
      diff = sum(abs(pnew - p))
      if ( diff > 1e-4) {
        loop = TRUE
        p = pnew
      } else {
        loop = FALSE
        list$pnew = pnew
        list$probability = prob.outlier(pnew, f)
      }
    }
  } else if (optim.method == "BFGS") {
    while (loop) {
      t =  prob.outlier(p, f)
      # negative Log likelihood
      LL = function(p) { t((1-t)) %*% (p[2] * f + p[1])
        + sum.help %*% log(1 + exp(-p[2] * f - p[1])) }

      # unconstraint optimization
      optim = optim(par = p, fn = LL, method = "BFGS")
      pnew = optim$par
      diff = sum(abs(pnew - p))

      if ( diff > 1e-4) {
        loop = TRUE
        p = pnew
      } else {
        loop = FALSE
        list$pnew = pnew
        list$probability = prob.outlier(pnew, f)
      }
    }
  } else if (optim.method == "glm") {
    while (loop) {
      t =  prob.outlier(p, f)

      # minimizing negative likelihood with glm
      df = data.frame(t, f)
      mod = glm(t ~ f, family = binomial(link = "logit"), data = df)

      pnew = coef(mod)
      diff = sum(abs(pnew - p))

      if ( diff > 1e-4) {
        loop = TRUE
        p = pnew
      } else {
        loop = FALSE
        list$pnew = pnew
        list$probability = prob.outlier(pnew, f)
      }
    }
  } else if (optim.method == "Newton") {
    while (loop) {
      label = ifelse(p[2] * f + p[1] > 0, 1, 0)
      prior1 = sum(label)
      prior0 = length(label) - prior1
      t =  prob.outlier(p, f)

      pnew = newton.optim(t = t, p = p, deci = f, label = label, prior1, prior0)
      diff = sum(abs(pnew - p))

      if ( diff > 1e-2) {
        loop = TRUE
        p = pnew
      } else {
        loop = FALSE
        list$pnew = pnew
        list$probability = prob.outlier(pnew, f)
      }
    }
  }
  #return(probability)
  return(list)
}




newton.optim = function(t, p, deci, label, prior1, prior0, maxiter = 100, minstep = 1e-10, sigma = 1e-12) {
  # Construct initial values:
  # target support in array t
  # initial function value in fval
  hiTarget = (prior1 + 1) / (prior1 + 2)
  loTarget = 1 / (prior0 + 2)
  len = prior1 + prior0 # total number of data
  t = c()

  for (i in 1:len) {
    if(label[i] > 0) t[i] = hiTarget
    else t[i] = loTarget
  }

  A = 0
  B = log( (prior0 + 1) / (prior1 + 1) )
  fval = 0
  for (i in 1:len) {
    fApB = deci[i] * A + B
    if (fApB >= 0) fval = fval + t[i] * fApB + log(1 + exp(-fApB))
    else fval = fval + (t[i]-1) * fApB + log(1 + exp(fApB))
  }

  for (it in 1:maxiter) {
    # Update Gradient and Hessian (use H' = H + sigma * I)
    h11 = h22 = sigma
    h21 = g1 = g2 = 0
    for (i in 1:len) {
      fApB = deci[i] * A + B
      if (fApB >= 0) {
        p = exp(-fApB) / (1.0 + exp(-fApB))
        q = 1.0 / (1.0 + exp(-fApB))
      } else {
        p = 1.0 / (1.0 + exp(fApB))
        q = exp(fApB) / (1.0 + exp(fApB))
      }
      d2 = p * q
      h11 = h11 + deci[i] * deci[i] * d2
      h22 = h22 + d2
      h21 = h21 + deci[i] * d2
      d1 = t[i] - p
      g1 = g1 + deci[i] * d1
      g2 = g2 + d1
    }
    if (abs(g1) < 1e-5 && abs(g2) < 1e-5) break #stopping criteria

    # Compute modified Newton directions
    det = h11 * h22 - h21 * h21
    dA = -(h22 * g1 - h21 * g2) / det
    dB = -(-h21 * g1 + h11 * g2) / det
    gd = g1 * dA + g2 * dB
    stepsize = 1
    while (stepsize >= minstep) { #Line search
      newA = A + stepsize * dA
      newB = B + stepsize * dB
      newf = 0
      for (i in 1:len) {
        fApB = deci[i] * newA + newB
        if (fApB >= 0) newf = newf + t[i] * fApB + log(1 + exp(-fApB))
        else newf = newf + (t[i] - 1) * fApB + log(1 + exp(fApB))
      }
      if (newf < fval + 0.0001 * stepsize * gd) {
        A = newA
        B = newB
        fval = newf
        break #Sufficient decrease satisfied
      } else
        stepsize = stepsize / 2
    }
    if (stepsize < minstep){
      message("Line search fails")
      break
    }
  }
  if (it >= maxiter) messagef("Reaching maximum iterations")
  return(c(A,B))
}


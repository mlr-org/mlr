# We recommend using double precision for the algorithm
maxiter = 100
minstep = 1e-10
sigma = 1e-12

newton.optim = function(deci, label, prior1, prior0, maxiter = 100, minstep = 1e-10, sigma = 1e-12) {
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
B = log( (prior0 + 1)/(prior1 + 1) )
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
  dB = -(-h21 * g1 + h11 * g2)/det
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
    stopf("Line search fails")
  }
}
  if (it >= maxiter) messagef("Reaching maximum iterations")
return(c(A,B))
}


em.sigmoid = function(anomaly.score, parainit = NULL) {
  f = anomaly.score
  p = parainit
  loop = TRUE
  sum.help = rep(1, length(f))
  list = list()

  if (is.null(p)) {
    p = c(B = 0, A = 1)
    message("Starting parameter values are missing. For calibration using sigmoid function the default is: \n A = 1, B = 0")
  }

  if (length(p) != 2) {
    stop("Too little/many starting parameters. For calibration using sigmoid function starting values for parameter A and B are needed.")
  }

  prob.outlier = function(x, score) {
    1 / (1 + exp(-x[2] * f - x[1]))
  }

  while (loop) {
    label = ifelse(p[2] * f + p[1] > 0, 1, 0)
    prior1 = sum(label)
    prior0 = length(label) - prior1

    pnew = newton.optim(deci = f, label = label, prior1, prior0)
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
  return(list)
}

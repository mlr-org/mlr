context("regr_twoStepAIC")

makeData_art_10 = function(n){
  n = 100
  l4c = list()
  l4c$x1 = as.matrix(10 + 5 * rnorm(n))
  l4c$x2 = exp( 3 * rnorm(n))
  l4c$x3 = 5 + 10 * runif(n)
  l4c$x4 = 100 + 50 * rnorm(n)
  l4c$x5 = l4c$x1 + 3 * rnorm(n)
  l4c$x6 = 2 * l4c$x2 + rexp(n)  # lognormal and exponential mixture
  l4c$x7 = 0.5 * exp( 4 * rnorm(n)) # lognormal
  l4c$x8 = 10 + 8 * runif(n)
  l4c$x9 = l4c$x2 + l4c$x8 + 2 * rnorm(n)
  l4c$x10 = 200 + 90 * rnorm(n)
  l4c$y = 3 * l4c$x2 - 4 * l4c$x8 + 5 * l4c$x9 + 3 * rnorm(n) 
  df = Reduce(cbind, x = l4c)
  df = as.data.frame(df)
  colnames(df)[length(l4c)] = "y"
  return(df)
}

makeData_art_10_2 = function(n){
  n = 100
  l4c = list()
  l4c$x1 = 10 + 5 * rnorm(n)
  l4c$x2 = exp( 3 * rnorm(n))
  l4c$x3 = 5 + 10 * runif(n)
  l4c$x4 = 100 + 50 * rnorm(n)
  l4c$x5 = l4c$x1 + 3 * rnorm(n)
  l4c$x6 = 2 * l4c$x2 + rexp(n)  # lognormal and exponential mixture
  l4c$x7 = 0.5 * exp( 4 * rnorm(n)) # lognormal
  l4c$x8 = 10 + 8 * runif(n)
  l4c$x9 = l4c$x2 + l4c$x8 + 2 * rnorm(n)
  l4c$x10 = 200 + 90 * rnorm(n)
  l4c$y = 3 * (l4c$x2 * l4c$x3 ) * - 4 * (l4c$x8 * l4c$x7) + 5 * (l4c$x9 * l4c$x1) + 3 * rnorm(n) 
  df = Reduce(cbind, x = l4c)
  df = as.data.frame(df)
  colnames(df)[length(l4c)] = "y"
  return(df)
}


test_that("regr_twoStepAIC", {
  pars = list(regr.formula, data = regr.train)
  set.seed(getOption("mlr.debug.seed"))
  mmodel = selectForward2Step(d = regr.train, target = "medv", muffle = TRUE) 
  p = predict(mmodel, newdata = regr.test)
  testSimple("regr.twoStepAIC", regr.df, regr.target, regr.train.inds, p)
})

test_that("regr_twoStepAIC_art",{
  df = makeData_art_10(1000)
  regr.train = df[1:40,] # 4 times the feature number
  regr.test = df[501:1000,]
  mmodel = selectForward2Step(d = regr.train, target = "y", muffle = TRUE) 
  p = predict(mmodel, newdata = regr.test)
})

test_that("regr_twoStepAIC_art_2",{
  df = makeData_art_10_2(1000)
  regr.train = df[1:40,]
  regr.test = df[501:1000,]
  mmodel = selectForward2Step(d = regr.train, target = "y", muffle = FALSE) 
  p = predict(mmodel, newdata = regr.test)
  
})
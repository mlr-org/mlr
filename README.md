
![mlr](https://mlr-org.github.io/mlr-tutorial/images/mlrLogo_blue_141x64.png): Machine Learning in R


Updates to MLR for Forecasting: October 15th, 2016
==========================

The goal of this project is to develop:

1. Forecasting time series models in the MLR framework
2. Measures for evaluating forecast models
3. Resampling methods that work for time based data
4. Automated Preprocessing for lag and difference features

As a basic introduction we will simulate data from an arima process and place it into an xts object.
```{r}
library(xts)
library(lubridate)
set.seed(1234)
dat = arima.sim(model = list(ar = c(.5,.2), ma = c(.4), order = c(2,0,1)), n = 200)
times = (as.POSIXlt("1992-01-14")) + lubridate::days(1:200)
dat = xts(dat,order.by = times)
colnames(dat) = c("arma_test")
```

# Forecast Regression Tasks

Just like with `makeRegrTask()` we will use `makeForecastRegrTask()` to create a task for forecasting. The main difference between `Forecast` tasks and the normal tasks is that our data must be an xts object.
```{r}
Timeregr.task = makeForecastRegrTask(id = "test", data = dat, target = "arma_test",
                                     frequency = 7L)
Timeregr.task
# Task: test
# Type: fcregr
# Observations: 200
# Dates:
#  Start: 1992-01-15 
# End: 1992-08-01
# Frequency: 7
# Features:
# numerics  factors  ordered 
#        1        0        0 
# Missings: FALSE
# Has weights: FALSE
# Has blocking: FALSE
```

## makeLearner for forecast regression tasks
Notice that we still inheret a Supervised task and our type is a forecast regression. We also specify a frequency in the task which is equal to our 'seasonality'. Examples of frequency include 7 for weekly seasonal data, 365 for yearly seasonal data, and 52 for yearly weekly data. 

Now we create an arima model from the package `forecast` using `makeLearner()` by calling the learner class `fcregr.Arima`. An important parameter is the `h` parameter, which is used to specify that we are forecasting 10 periods ahead

```{r}
arm = makeLearner("fcregr.Arima", order = c(2L,0L,1L), h = 10L, include.mean = FALSE)
arm
# Learner fcregr.Arima from package forecast
# Type: fcregr
# Name: AutoRegressive Integrated Moving Average; Short name: Arima
# Class: fcregr.Arima
# Properties: numerics,ts,quantile
# Predict-Type: response
# Hyperparameters: order=2,0,1,h=10,include.mean=FALSE
```

## Resampling

We now have two new cross validation resampling strategies, `GrowingCV` and `FixedCV`. They are both rolling forecasting origin techniques established in [Hyndman and Athanasopoulos (2013)](https://www.otexts.org/fpp/2/5) and first made popular in R by the `caret` package's `createTimeSlices()` function. We specify:

1. horizon - the number of periods to forecast
2. initialWindow - our left-most starting time slice (needs to be changed to initial.window for standards)
3. size - The number of rows in our time series
4. skip - An optional parameter that allow to skip every n'th window.
```{r}
resamp_desc = makeResampleDesc("GrowingCV", horizon = 10L,
                               initialWindow = 100L,
                               size = nrow(dat), skip = 15L)
resamp_desc
# Window description:
#  growing with 6 iterations:
#  100 observations in initial window and 10 horizon.
# Predict: test
# Stratification: FALSE
```

Note that we should need to remove stratification, as it does not really make sense in the context of time series to stratify our data (unless we can somehow use this for panel data). The wonderful graphic posted below comes from the `caret` website and gives an intuitive idea of the sliding windows for both the growth and fixed options.

![Build Status](http://topepo.github.io/caret/main_files/figure-html/Split_time-1.png)

Taking our model, task, resampling strategy, and an additonal parameter for scoring our model, we use `resample()` to train our model.
```{r}
resamp_arm = resample(arm,Timeregr.task, resamp_desc, measures = mase)
resamp_arm
# Resample Result
# Task: test
# Learner: fcregr.Arima
# Aggr perf: mase.test.mean=0.0629
# Runtime: 0.238438
```
## Tuning

The forecasting features fully integrate into mlr, allowing us to also make a parameter set to tune over. Here we make a very small parameter space and will use F1-racing to tune our parameters.
```{r}

par_set = makeParamSet(
  makeIntegerVectorParam(id = "order",
                         len = 3L,
                         lower = c(0L,0L,0L),
                         upper = c(2L,1L,1L),
                         tunable = TRUE),
  makeLogicalParam(id = "include.mean", default = FALSE, tunable = TRUE),
  makeIntegerParam(id = "h", default = 10L, lower = 10L, upper = 11L, tunable = FALSE)
)

#Specify tune by grid estimation
ctrl = makeTuneControlIrace(maxExperiments = 180L)

#

configureMlr(on.learner.error = "warn")
res = tuneParams("fcregr.Arima", task = Timeregr.task, resampling = resamp_desc, par.set = par_set,
                 control = ctrl, measures = mase)

```
Note that we have to do something very odd when specifying `h`. We specify the upper bound of `h` as 11 as irace will not work if the lower and upper bound of a parameter is the same value, even if the parameter has been specified to `tune = FALSE`. What this means is that inside of `makePrediction.TaskDescForecastRegr` we have to do a weird thing where, even though our prediction will at times be length 11, we cut it by the length of the truth variable `y[1:length(truth)]`. This is only going to complicate things in the future I'm sure. But irace works.

It is interesting to note that Arima does not select our sample data's original underlying process and instead selects a (1,0,1) model.
```{r}
res

# Tune result:
# Op. pars: order=1,0,1; include.mean=FALSE; h=10
# mase.test.mean=0.0618
```

This may be due to how small the data set is.
```{r}
as.data.frame(res$opt.path)[4,]
#   order1 order2 order3 include.mean  h mase.test.mean dob eol error.message exec.time
# 4      2      0      1        FALSE 10     0.06288381   1  NA          <NA>     0.156
```

We can now use our learned model with tuning to pass over the entire data set and give our final prediction. However there is currently a bug in the predict function.
```{r}
lrn = setHyperPars(makeLearner("fcregr.Arima"), par.vals = res$x)
m = train(lrn, Timeregr.task)

# Should give back the following error
predict(m, task = Timeregr.task)
 
#  Error in (function (..., row.names = NULL, check.rows = FALSE, check.names = TRUE,  : 
#   arguments imply differing number of rows: 200, 10
```
This error is caused by `y[1:length(truth)]` in `makePrediction.TaskDescForecastRegr`. We are only generating 10 new observations (forecasts), but the `predict` function returns a data frame the same size as task data with `NA`s after 10 response observations. Furthermore, predict requires either newdata or task, but for forecasting models we will sometimes not have new data

```{r}
predict(m)
# Error in predict.WrappedModel(m) : 
# Pass either a task object or a newdata data.frame to predict, but not both!
```

We can get back the raw estimates with no truth by making newdata a data frame of `NA`'s, though this is more likely to be a bug as it is a solution. In addition, this requires some strange if loops in `makePrediction.TaskDescForecastRegr` to get around when there is no truth values.
```{r}
predict(m, newdata = data.frame(rep(NA, 10)))
# Prediction: 10 observations
# predict.type: response
# threshold: 
# time: 0.00
#   response
# 1        0
# 2        0
# 3        0
# 4        0
# 5        0
# 6        0
# ... (10 rows, 1 cols)
```

Issues with frequency have arisen when working with the `forecast` package due to frequency. While `xts` is robust, it assumes all dates are continuous, unique, and have a frequency of one. While this gives a robust structure, many of the methods in forecast are dependent on the datas frequency. To allow both packages to mesh we include a new parameter in `makeForecastRegrTask()` for frequency.

```{r}
Timeregr.task = makeForecastRegrTask(id = "test", data = dat,
                                     target = "arma_test", frequency = 1L)
# Task: test
# Type: regr
# Observations: 2000
# Dates:
#  Start: 2016-09-23 23:16:50 
#  End: 2022-03-15 23:16:50
# Frequency: 1
# Features:
# numerics  factors  ordered 
#        1        0        0 
# Missings: FALSE
# Has weights: FALSE
# Has blocking: FALSE
```
A new print statement for objects of class `TimeTask` was created for including dates and frequency.


# Pre-processing

A new function to create arbitrary lags and differences `createLagDiffFeatures()` has also been added. Notice that we get a weird doubel date thing, but our column names look nice

```{r}
Timeregr.task.lag = createLagDiffFeatures(Timeregr.task,lag = 2L:4L, difference = 0L, 
                                          seasonal.lag = 1L:2L)
tail(Timeregr.task.lag$env$data)
```
<!-- html table generated in R 3.3.1 by xtable 1.8-2 package -->
<!-- Sun Oct 16 01:26:45 2016 -->
<table border=1>
<tr> <th>  </th> <th> dates </th> <th> arma_test_lag2_diff0 </th> <th> arma_test_lag3_diff0 </th> <th> arma_test_lag4_diff0 </th> <th> arma_test_lag7_diff0 </th> <th> arma_test_lag14_diff0 </th>  </tr>
  <tr> <td align="right"> 1992-07-27 </td> <td align="right"> 712209600.00 </td> <td align="right"> -1.90 </td> <td align="right"> -0.62 </td> <td align="right"> 0.39 </td> <td align="right"> -0.01 </td> <td align="right"> -2.98 </td> </tr>
  <tr> <td align="right"> 1992-07-28 </td> <td align="right"> 712296000.00 </td> <td align="right"> -0.18 </td> <td align="right"> -1.90 </td> <td align="right"> -0.62 </td> <td align="right"> 0.53 </td> <td align="right"> -3.07 </td> </tr>
  <tr> <td align="right"> 1992-07-29 </td> <td align="right"> 712382400.00 </td> <td align="right"> 0.36 </td> <td align="right"> -0.18 </td> <td align="right"> -1.90 </td> <td align="right"> 0.32 </td> <td align="right"> -3.04 </td> </tr>
  <tr> <td align="right"> 1992-07-30 </td> <td align="right"> 712468800.00 </td> <td align="right"> -0.16 </td> <td align="right"> 0.36 </td> <td align="right"> -0.18 </td> <td align="right"> 0.39 </td> <td align="right"> -0.77 </td> </tr>
  <tr> <td align="right"> 1992-07-31 </td> <td align="right"> 712555200.00 </td> <td align="right"> 0.16 </td> <td align="right"> -0.16 </td> <td align="right"> 0.36 </td> <td align="right"> -0.62 </td> <td align="right"> 1.29 </td> </tr>
  <tr> <td align="right"> 1992-08-01 </td> <td align="right"> 712641600.00 </td> <td align="right"> 2.24 </td> <td align="right"> 0.16 </td> <td align="right"> -0.16 </td> <td align="right"> -1.90 </td> <td align="right"> 1.87 </td> </tr>
   </table>
   
A new preprocessing wrapper `makePreprocWrapperLambert()` has been added. This function uses the
`LambertW` package's `Guassianize()` function to help remove skewness and kurtosis from the data.

```{r}
lrn = makePreprocWrapperLambert("classif.lda", type = "h")
print(lrn)
# Learner classif.lda.preproc from package MASS
# Type: classif
# Name: ; Short name: 
# Class: PreprocWrapperLambert
# Properties: numerics,factors,prob,twoclass,multiclass
# Predict-Type: response
# Hyperparameters: target.proc=FALSE,type=h,methods=IGMM,verbose=FALSE
# train(lrn, iris.task)
```

The lambert W transform is a bijective function, but the current preprocessing wrapper does not allow us to invert our predictions back to the actual values when we make new predictions. This would be helpful if we wanted to use LambertW on our target, then get back answers that match with our real values instead of the transformed values.

# Models

Several new models have been included from forecast:

1. Exponential smoothing state space model with Box-Cox transformation (bats)
2. Exponential smoothing state space model with Box-Cox transformation, ARMA errors, Trend and Seasonal Fourier components (tbats)
3. Exponential smoothing state space model (ets)
4. Neural Network Autoregressive model (nnetar)


The below code will run with the `Timeregr.task` we states above.
```{r}
batMod = makeLearner("fcregr.bats", h = 10)
m = train(batMod, Timeregr.task)
predict(m, newdata = data.frame(rep(NA, 10)))

#fc.tbats
tBatsMod = makeLearner("fcregr.tbats", h = 10)
m = train(tBatsMod, Timeregr.task)
predict(m, newdata = data.frame(rep(NA, 10)))

#fc.ets
etsMod = makeLearner("fcregr.ets", h = 10)
m = train(etsMod, Timeregr.task)
predict(m, newdata = data.frame(rep(NA, 10)))

# NOTE: Sometimes, this produces the error
## Error in .Call("etsTargetFunctionInit", y = y, nstate = nstate, errortype = switch(errortype,  : 
##   "etsTargetFunctionInit" not resolved from current namespace (forecast) 

# This is caused by the namespace of forecast and requires re-downloading forecast.
# Sometimes this can be fixed simply by resetting R.

#fc.nnetar
nnetarMod = makeLearner("fcregr.nnetar", h = 10)
nnetarMod
m = train(nnetarMod, Timeregr.task)
predict(m, newdata = data.frame(rep(NA, 10)))
```

And now we can also do GARCH models! Though we need to talk about how I am coalescing the parameters. The GARCH models come from the package `rugarch`, which has lists of parameter controls. Similar to ada's control function we have to do a little work to translate the GARCH models to `mlr`'s format.

 they set the predict.type to quantile. Fixed typo in tbats unit test. added fcregr to regression measures. Created test files to run all fcregr models. createLagDiffFeatures had a bug that did not return the target variable which is now resolved. iracing now works with forecast models, but a strange line of code that sets predict length to truth length had to be included. mase moved to measures file. Error found in ets that only happens during test. etsInitialFunc... cannot load from forecast namespace. Created makeForecastRegrLearner function and added checks in predict and train specific to fcregr models. Previously this was done by adding bits to regr tests, but enough has been added to become a new branch.

```{r}
garchMod = makeLearner("fcregr.garch", model = "sGARCH",
                       garchOrder = c(1,1), n.ahead = 10,
                       armaOrder = c(2, 1))
m = train(garchMod, Timeregr.task)
predict(m, newdata = as.data.frame(rep(NA,10)))
```

### Tests

There are now tests for each of the forecasting models implimented here. However ets fails with a strange error from the `forecast` package's namespace

```{r}
devtools::test(filter = "fcregr")
# Loading mlr
# Testing mlr
# fcregr_arfima: .....
# fcregr_Arima: .....
# fcregr_bats: .....
# fcregr_ets: 1
# fcregr_garch: .....
# fcregr_tbats: .....
# learners_all_fcregr: ..............2Timing stopped at: 0.003 0 0.003 
# Failed --------------------------------------------------------------------------------------------------
# 1. Error: fcregr_ets (@test_fcregr_ets.R#20)  ------------------------------------------------------------
# "etsTargetFunctionInit" not resolved from current namespace (forecast)
# ...
# 2. Error: learners work: fcregr  (@test_learners_all_fcregr.R#21) ---------------------------------------
# "etsTargetFunctionInit" not resolved from current namespace (forecast)
```

I've posted an issue on `forecasts` github page and will email Dr. Hyndman (author) if he does not respond soon. The strangest thing about this error is that it only happens during testing. With a fresh restart of R you can use the ets functions just fine.

We also have tests for general forecasting and `createLagDiffFeatures`. The test for Lambert W pre-processing is available in the general forecasting tests.
```{r}
devtools::test(filter = "forecast")
Loading mlr
Testing mlr
forecast: .......

DONE ====================================================================================================

devtools::test(filter = "createLagDiffFeatures")
Loading mlr
Testing mlr
createLagDiffFeatures: ....

DONE ====================================================================================================

```

## Updating Models

A new function `updateModel()` has been implimented that is a sort of frankenstein between `train()` and `predict()`.

```{r}
Timeregr.task = makeForecastRegrTask(id = "test", data = dat[1:190,], target = "arma_test",
                                     frequency = 7L)
arm = makeLearner("fcregr.Arima", order = c(2L,0L,1L), h = 10L, include.mean = FALSE)
arm
armMod = train(arm, Timeregr.task)
updateArmMod = updateModel(armMod, Timeregr.task, newdata = dat[192:200,])
updateArmMod
# Model for learner.id=fcregr.Arima; learner.class=fcregr.Arima
# Trained on: task.id = test; obs = 9; features = 1
# Hyperparameters: order=2,0,1,h=10,include.mean=FALSE
```

This works by making a call to `updateLearner.fcregr.Arima()` and updating the model and task data with `newdata`. `predict()` works as it would on a normal model.

```{r}
# predict(updateArmMod, newdata = as.data.frame(rep(NA,10)))
# Prediction: 10 observations
# predict.type: response
# threshold: 
# time: 4.29
#    response
# 1 1.3631309
# 2 1.0226897
# 3 0.7818986
# 4 0.5996956
# 5 0.4601914
# 6 0.3531699
# ... (10 rows, 1 cols)
```

Other models with update functions include `ets`, `bats`, and `tbats`.

## Using ML models in forecasting

To use ML models for forecasting we have to create our autoregressive features using `createLagDiffFeatures`.
```{r}

daty = arima.sim(model = list(ar = c(.5,.2), ma = c(.4), order = c(2,0,1)), n = 200)
datx = daty + arima.sim(model = list(ar = c(.6,.1), ma = c(.3), order = c(2,0,1)), n = 200)
times = (as.POSIXlt("1992-01-14")) + lubridate::days(1:200)
dat = xts(data.frame(dat_y = daty,dat_x = datx),order.by = times)

train.dat = dat[1:190,]
test.dat = dat[191:200,]

Timeregr.task = makeForecastRegrTask(id = "test", data = train.dat,
                                     target = "dat_y", frequency = 7L)

Timeregr.ml = createLagDiffFeatures(Timeregr.task, lag = 5:12L, na.pad = FALSE, 
return.nonlag = FALSE, cols = NULL)
Timeregr.ml
# Task: test
# Type: regr
# Observations: 178
# Dates:
#  Start: 1992-01-27
#  End:   1992-07-22
# Frequency: 7
# Features:
# numerics  factors  ordered 
#       9        0        0 
# Missings: FALSE
# Has weights: FALSE
# Has blocking: FALSE

```

There are two important things to note here.

1. We are forecasting 5 periods ahead and so we start our lags 5 periods in the past.
2. Our type is now `regr`.

(1) happens because we want to forecast 5 periods ahead. The regular schema for forecasting models when you have only one variable is to use prediction `y_{t+1}` to predict `y_{t+2}` ,etc. This is reasonable when your target's forecast is only decided by past forecasts, however when you have multiple variables predicting your target this becomes very difficult. Take for example estimating `y_{t+2}` with `y_{t+1}` and `x_{1,t+1}` and `x_{2,t+1}` when you are at time `t`. In future iterations this may be done in a cleaner manner with a multivariate Kalman filter or a Copula, but for an ad-hoc multivariate forecast our method of lagging `k` forecast periods will suffice.

(2) happens because we did not select any particular columns to lag. If we selected no columns to lag we lag our target variable, which is not something that happens inside of `fcregr` models. If we had other variables and specified them in cols then we would receive back a `fcregr` task type.

Now, just as with a regular regression, we specify our learner and train it.

```{r}
regrGbm <- makeLearner("regr.gbm", par.vals = list(n.trees = 2000,
interaction.depth = 8,
distribution = "laplace"))
gbmMod = train(regrGbm, Timeregr.ml)
```

We now want to predict our new data, but notice that our lags are from 5 through 50 and our testing data is only 10 periods in length. If we did 50 lags we would receive back a bunch of `NA` columns. So to get around this we will do the following.

```{r}
Timeregr.gbm.update = updateLagDiff(Timeregr.ml,test.dat)
# Task: test
# Type: regr
# Observations: 188
# Dates:
#  Start: 1992-01-27 
#  End:   1992-08-01
# Frequency: 7
# Features:
# numerics  factors  ordered 
#       16        0        0 
# Missings: FALSE
# Has weights: FALSE
# Has blocking: FALSE
```

The function `updateData()` passes a task that was modified by `createLagDiffFeatures()` and the new data we would like to include. So now to make a prediction of the next 5 we simply subset our task by the last five observations. Here our last observation is 188 because we had an original data set of size 200 with 12 lag. 
```{r}
predict(gbmMod, task = Timeregr.gbm.update, subset = 184:188)
# Prediction: 5 observations
# predict.type: response
# threshold: 
# time: 0.00
#             id       truth    response
# 1992-07-28 184 -1.45613899  0.47721792
# 1992-07-29 185  0.09614877 -0.01412844
# 1992-07-30 186  0.90837420 -0.34565917
# 1992-07-31 187  0.39279490 -0.23385251
# 1992-08-01 188  0.29235931 -0.17127450
```

#### Ensemble's of Forecasts

It's known that ensembles of forecasts tend to outperform standard forecasting techniques. Here we use mlr's stacked modeling functionality to ensemble multiple forecast techniques by a super learner.

```{r}
library(xts)
library(lubridate)
dat = arima.sim(model = list(ar = c(.5,.2), ma = c(.4), order = c(2,0,1)), n = 500)
times = (as.POSIXlt("1992-01-14")) + lubridate::days(1:500)
dat = xts(dat,order.by = times)
colnames(dat) = c("arma_test")

dat.train = dat[1:490,]
dat.test  = dat[491:500,]

timeregr.task = makeForecastRegrTask(id = "test", data = dat.train, target = "arma_test",
                                     frequency = 7L)
                                     
resamp.sub = makeResampleDesc("GrowingCV",
                          horizon = 5L,
                          initial.window = .85,
                          size = getTaskData(timeregr.task)
                          )
                          
resamp.super = makeResampleDesc("CV", iters = 10)

base = c("fcregr.tbats", "fcregr.bats")
lrns = lapply(base, makeLearner)
lrns = lapply(lrns, setPredictType, "response")
stack.forecast = makeStackedLearner(base.learners = lrns,
                       predict.type = "response",
                       method = "growing.cv",
                       super.learner = makeLearner("regr.earth", penalty = 2),
                       resampling = resamp.sub)

ps = makeParamSet(
  makeDiscreteParam("fcregr.tbats.h", values = 5),
  makeDiscreteParam("fcregr.bats.h", values = 5)
)

## tuning
testm = tuneParams(stack.forecast, timeregr.task, resampling = resamp.super,
                   par.set = ps, control = makeTuneControlGrid(),
                   measures = mase)

mm = makeStackedLearner(base.learners = lrns,
                        predict.type = "response", super.learner = makeLearner("regr.earth", penalty = 2),
                        parset = testm$x)
testmm = train(m,timeregr.task)
testp = predict(testmm, newdata = dat.test)
performance(testp, mase, task = timeregr.task)
```


## Multivariate Forecasting

One common problem with forecasting is that it is difficult to use additional explanatory variables. If we are at time `t` and want to forecast 10 periods in the future, we need to know the values of the explanatory variables at time `t+10`, which is often not possible. A new set of models which treats explanatory variables endogenously instead of exogenously allows us to forecast not only our target, but addititional explanatory variables. This is done by treating all the variables as targets.

```{r}
library(lubridate)
library(xts)
data("EuStockMarkets")
stock.times = date_decimal(as.numeric(time(EuStockMarkets)))
stock.data  = xts(as.data.frame(EuStockMarkets), order.by = stock.times)
stock.data.train = stock.data[1:1850,]
stock.data.test  = stock.data[1851:1855,]
multfore.task = makeMultiForecastRegrTask(id = "bigvar", data = stock.data.train, target = "all")
multfore.task
# Supervised task: bigvar
# Type: mfcregr
# Target: DAX,SMI,CAC,FTSE
# Observations: 1850
# Features:
# numerics  factors  ordered 
#        0        0        0 
# Missings: FALSE
# Has weights: FALSE
# Has blocking: FALSE

bigvar.learn = makeLearner("mfcregr.BigVAR", p = 4, struct = "Basic", gran = c(25, 10),
                    h = 5, n.ahead = 5)
multi.train = train(bigvar.learn, multfore.task)
pred  = predict(multi.train, newdata = stock.data.test)
pred
```
<!-- html table generated in R 3.3.1 by xtable 1.8-2 package -->
<!-- Sat Oct 29 19:51:43 2016 -->
<table border=1>
<tr> <th>  </th> <th> truth.DAX </th> <th> truth.SMI </th> <th> truth.CAC </th> <th> truth.FTSE </th> <th> response.DAX </th> <th> response.SMI </th> <th> response.CAC </th> <th> response.FTSE </th>  </tr>
  <tr> <td align="right"> 1998-08-12 05:04:36 </td> <td align="right"> 5774.38 </td> <td align="right"> 8139.20 </td> <td align="right"> 4095.00 </td> <td align="right"> 5809.70 </td> <td align="right"> 5826.03 </td> <td align="right"> 8233.48 </td> <td align="right"> 4117.87 </td> <td align="right"> 5937.16 </td> </tr>
  <tr> <td align="right"> 1998-08-13 14:46:09 </td> <td align="right"> 5718.70 </td> <td align="right"> 8170.20 </td> <td align="right"> 4047.90 </td> <td align="right"> 5736.10 </td> <td align="right"> 5799.61 </td> <td align="right"> 8228.52 </td> <td align="right"> 4072.32 </td> <td align="right"> 6013.65 </td> </tr>
  <tr> <td align="right"> 1998-08-15 00:27:41 </td> <td align="right"> 5614.77 </td> <td align="right"> 7943.20 </td> <td align="right"> 3976.40 </td> <td align="right"> 5632.50 </td> <td align="right"> 5787.13 </td> <td align="right"> 8229.91 </td> <td align="right"> 4037.78 </td> <td align="right"> 6075.88 </td> </tr>
  <tr> <td align="right"> 1998-08-16 10:09:13 </td> <td align="right"> 5528.12 </td> <td align="right"> 7846.20 </td> <td align="right"> 3968.60 </td> <td align="right"> 5594.10 </td> <td align="right"> 5777.30 </td> <td align="right"> 8233.20 </td> <td align="right"> 4008.40 </td> <td align="right"> 6127.79 </td> </tr>
  <tr> <td align="right"> 1998-08-17 19:50:46 </td> <td align="right"> 5598.32 </td> <td align="right"> 7952.90 </td> <td align="right"> 4041.90 </td> <td align="right"> 5680.40 </td> <td align="right"> 5767.11 </td> <td align="right"> 8235.93 </td> <td align="right"> 3981.94 </td> <td align="right"> 6171.05 </td> </tr>
   </table>
   
```{r}
performance(pred, multivar.mase, task = test)
# multivar.mase 
#   0.02458401 
```

### Multivariate Forecasting with ML stacking

Now that we have a multivariate form of forecasting, we can use these forecasts stacked with a machine learning super learner.

```{r}
resamp.sub = makeResampleDesc("GrowingCV",
                              horizon = 5L,
                              initial.window = .97,
                              size = nrow(getTaskData(multfore.task)),
                              skip = .01
)

resamp.super = makeResampleDesc("CV", iters = 3)

base = c("mfcregr.BigVAR")
lrns = lapply(base, makeLearner)
lrns = lapply(lrns, setPredictType, "response")
stack.forecast = makeStackedLearner(base.learners = lrns,
                                    predict.type = "response",
                                    super.learner = makeLearner("regr.earth", penalty = 2),
                                    method = "growing.cv",
                                    resampling = resamp.sub)

ps = makeParamSet(
  makeDiscreteParam("mfcregr.BigVAR.p", values = 5),
  makeDiscreteParam("mfcregr.BigVAR.struct", values = "Basic"),
  makeNumericVectorParam("mfcregr.BigVAR.gran", len = 2L, lower = 25, upper = 26),
  makeDiscreteParam("mfcregr.BigVAR.h", values = 5),
  makeDiscreteParam("mfcregr.BigVAR.n.ahead", values = 5)
)

## tuning

multfore.task = makeMultiForecastRegrTask(id = "bigvar", data = stock.data.train, target = "FTSE")

multfore.tune = tuneParams(stack.forecast, multfore.task, resampling = resamp,
                   par.set = ps, control = makeTuneControlGrid(),
                   measures = multivar.mase)
multfore.tune
# Tune result:
# Op. pars: mfcregr.BigVAR.p=5; mfcregr.BigVAR.struct=Basic; mfcregr.BigVAR.gran=26,25; 
# mfcregr.BigVAR.h=5; mfcregr.BigVAR.n.ahead=5
# multivar.mase.test.mean=0.0183

stack.forecast.f  = setHyperPars2(stack.forecast,multfore.tune$x)
multfore.train = train(stack.forecast.f,multfore.task)
multfore.train
# Model for learner.id=stack; learner.class=StackedLearner
# Trained on: task.id = bigvar; obs = 1850; features = 3
# Hyperparameters: mfcregr.BigVAR.p=5,mfcregr.BigVAR.struct=Basic,mfcregr.BigVAR.gran=26,25,mfcregr.
# BigVAR.h=5,mfcregr.BigVAR.n.ahead=5

multfore.pred = predict(multfore.train, newdata = stock.data.test)
multfore.pred
# Prediction: 5 observations
# predict.type: response
# threshold: 
# time: 0.01
#                      truth response
# 1998-08-12 05:04:36 5809.7 6024.293
# 1998-08-13 14:46:09 5736.1 6022.679
# 1998-08-15 00:27:41 5632.5 6022.640
# 1998-08-16 10:09:13 5594.1 6024.083
# 1998-08-17 19:50:46 5680.4 6025.666

performance(multfore.pred, mase, task = multfore.task)
#      mase 
# 0.04215817 
```

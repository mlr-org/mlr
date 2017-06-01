context("oneclass_h2oautoencoder")

test_that("oneclass_h2oautoencoder", {
  skip_on_travis()
  requirePackages("h2o", default.method = "load")
  h2o::h2o.init()

  parset.list = list(
    list(),
    list(activation = "Tanh"),
    list(l1 = 1),
    list(hidden = c(5,2,5))
  )

 parset.list = lapply(parset.list, function(x) c(x, reproducible = TRUE, seed = 1234))
  old.probs.list = list()

  train.hex = h2o::as.h2o(oneclass.train[, -oneclass.col], destination_frame = "train.hex")
  test.hex = h2o::as.h2o(oneclass.test[, -oneclass.col], destination_frame = "test.hex")

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    parset = c(parset, list(x = colnames(train.hex), training_frame = train.hex,
      autoencoder = TRUE))
    #set.seed(getOption("mlr.debug.seed"))
    m = do.call(h2o.deeplearning, parset)
    p  = h2o.anomaly(m, test.hex, per_feature=FALSE)
    old.probs.list[[i]] = as.vector(p)
  }

  testProbParsets("oneclass.h2o.autoencoder", oneclass.df,
    oneclass.target, oneclass.positive, oneclass.negative, oneclass.train.inds, old.probs.list, parset.list)
})

test_that("class names are integers and response predicted", {
  df = data.frame(matrix(runif(100, 0, 1), 100, 9))
  classx = factor(sample(c(0, 1), 100, replace = TRUE))
  df = cbind(classx, df)

  oneclass.task = makeOneClassTask(id = "example", data = df, target = "classx", positive = 1, negative = 0)
  ae.lrn  = makeLearner("oneclass.h2o.autoencoder", predict.type = "response", activation = "Tanh")
  rdesc = makeResampleDesc("CV", iters = 2L)
  rin = makeResampleInstance(rdesc, task = oneclass.task)
  r = resample(ae.lrn, oneclass.task, rin)
  expect_false(is.null(r$pred))
})

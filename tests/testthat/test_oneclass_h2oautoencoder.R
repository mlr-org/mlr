context("oneclass_h2oautoencoder")

test_that("oneclass_h2oautoencoder", {
  skip_on_travis()
  requirePackages("h2o", default.method = "load")
  h2o::h2o.init()

  # in mlr different defaults were set for hidden
  parset.list.h2o = list(
    list(hidden = 200),
    list(hidden = 200, epochs = 50),
    list(hidden = c(20, 10))
  )

  parset.list.mlr = list(
    list(),
    list(epochs = 50),
    list(layers = 2, nodes1 = 20, nodes2 = 10)
  )


  parset.list.h2o = lapply(parset.list.h2o, function(x) c(x, activation = "Tanh", reproducible = TRUE, seed = 1234, l1 = 1e-4, sparse = TRUE))
  parset.list.mlr = lapply(parset.list.mlr, function(x) c(x, activation = "Tanh", reproducible = TRUE, seed = 1234, l1 = 1e-4, sparse = TRUE))

  old.probs.list = list()

  train.hex = h2o::as.h2o(oneclass.train[, -oneclass.col], destination_frame = "train.hex")
  test.hex = h2o::as.h2o(oneclass.test[, -oneclass.col], destination_frame = "test.hex")

  for (i in seq_along(parset.list.h2o)) {
    parset = parset.list.h2o[[i]]
    parset = c(parset, list(x = colnames(train.hex), training_frame = train.hex,
      autoencoder = TRUE))
    m = do.call(h2o.deeplearning, parset)
    p  = h2o.anomaly(m, test.hex, per_feature = FALSE)
    old.probs.list[[i]] = convertingScoresToProbability(as.matrix(p))$probability[, 1]
  }

  testProbParsets("oneclass.h2o.autoencoder", oneclass.df,
    oneclass.target, oneclass.train.inds, old.probs.list, parset.list.mlr, oneclass.positive, oneclass.negative)
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

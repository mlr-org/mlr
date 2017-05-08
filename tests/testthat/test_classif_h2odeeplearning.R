context("classif_h2odeeplearning")

test_that("classif_h2odeeplearning", {
  skip_on_travis()
  requirePackages("h2o", default.method = "load")
  h2o::h2o.init()

  parset.list = list(
    list(),
    list(hidden = 2L),
    list(hidden = 2L, rate = 0.2),
    list(hidden = 2L, rate_decay = 0.2)
  )
  #h20deeplearning needs seed in function call to be reproducible
  debug.seed = getOption("mlr.debug.seed")
  parset.list = lapply(parset.list, function(x) c(x, seed = debug.seed, reproducible = TRUE))
  old.probs.list = list()

  for (i in seq_along(parset.list)) {
    parset = parset.list[[i]]
    parset = c(parset, list(x = colnames(binaryclass.train[, -binaryclass.class.col]),
      y = binaryclass.target,
      training_frame = h2o::as.h2o(binaryclass.train)))
    m = do.call(h2o::h2o.deeplearning, parset)
    p  = predict(m, newdata = h2o::as.h2o(binaryclass.test))
    old.probs.list[[i]] = as.data.frame(p)[, 2L]
  }

  testProbParsets("classif.h2o.deeplearning", binaryclass.df, binaryclass.target, binaryclass.train.inds, old.probs.list, parset.list)
})

test_that("class names are integers and probabilities predicted (#1787)", {
  df = data.frame(matrix(runif(100, 0, 1), 100, 9))
  classx = factor(sample(c(0, 1), 100, replace = TRUE))
  df = cbind(classx, df)

  classif.task = makeClassifTask(id = "example", data = df, target = "classx")
  gb.lrn  = makeLearner("classif.h2o.deeplearning", predict.type = "prob")
  rdesc = makeResampleDesc("CV", iters = 3, stratify = TRUE)
  rin = makeResampleInstance(rdesc, task = classif.task)
  r = resample(gb.lrn, classif.task, rin)
  expect_false(is.null(r$pred))
})

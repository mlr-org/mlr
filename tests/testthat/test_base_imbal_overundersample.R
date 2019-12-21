context("overundersample")

test_that("over and undersample works", {
  y = binaryclass.df[, binaryclass.target]
  tab1 = table(y)
  task = oversample(binaryclass.task, rate = 2)
  df = getTaskData(task)
  tab2 = table(df[, binaryclass.target])
  expect_equal(tab2["M"], tab1["M"])
  expect_equal(tab2["R"], tab1["R"] * 2)

  task = undersample(binaryclass.task, rate = 0.5)
  df = getTaskData(task)
  tab2 = table(df[, binaryclass.target])
  expect_equal(tab2["M"], round(tab1["M"] / 2))
  expect_equal(tab2["R"], tab1["R"])
})

test_that("over and undersample wrapper", {
  rdesc = makeResampleDesc("CV", iters = 2)
  lrn1 = makeLearner("classif.rpart")
  lrn2 = makeUndersampleWrapper(lrn1, usw.rate = 0.5)
  r = resample(lrn2, binaryclass.task, rdesc)
  expect_true(!is.na(r$aggr))

  lrn2 = makeOversampleWrapper(lrn1, osw.rate = 1.5)
  r = resample(lrn2, binaryclass.task, rdesc)
  expect_true(!is.na(r$aggr))
})

test_that("over and undersample arg check works", {
  task = makeClassifTask(data = multiclass.df, target = multiclass.target)
  expect_error(undersample(task, rate = 0.5), "binary")
  expect_error(oversample(task, rate = 0.5), "binary")
})

test_that("over and undersample works with weights", {
  task = makeClassifTask(data = binaryclass.df, target = binaryclass.target,
    weights = seq_len(nrow(binaryclass.df)))
  task2 = undersample(task, rate = 0.5)
  expect_true(length(task2$weights) < length(task$weights))
  expect_true(all(task2$weights %in% task$weights))
})

test_that("oversampling keeps all min / max obs", {
  y = binaryclass.df[, binaryclass.target]
  z = getMinMaxClass(y)
  new.inds = sampleBinaryClass(y, 1.05, cl = z$min.name, resample.other.class = FALSE)
  expect_true(setequal(intersect(z$min.inds, new.inds), z$min.inds))
})

test_that("control which class gets over or under sampled", {
  set.seed(getOption("mlr.debug.seed"))
  # check function oversample(), undersample()
  y = binaryclass.df[, binaryclass.target]
  tab1 = table(y)
  z = getMinMaxClass(y)
  task = oversample(binaryclass.task, rate = 2, cl = z$max.name)
  df = getTaskData(task)
  tab2 = table(df[, binaryclass.target])
  expect_equal(tab2["R"], tab1["R"])
  expect_equal(tab2["M"], tab1["M"] * 2)
  task = undersample(binaryclass.task, rate = 0.5, cl = z$min.name)
  df = getTaskData(task)
  tab2 = table(df[, binaryclass.target])
  expect_equal(tab2["R"], round(tab1["R"] / 2))
  expect_equal(tab2["M"], tab1["M"])

  # check over- and undersample-wrapper
  z = getMinMaxClass(binaryclass.df[, binaryclass.target])
  rdesc = makeResampleDesc("CV", iters = 2)
  lrn1 = makeLearner("classif.rpart")
  lrn2 = makeUndersampleWrapper(lrn1, usw.rate = 0.1, usw.cl = z$min.name)
  r = resample(lrn2, binaryclass.task, rdesc)
  expect_true(!is.na(r$aggr))

  lrn2 = makeOversampleWrapper(lrn1, osw.rate = 1.5, osw.cl = z$max.name)
  r = resample(lrn2, binaryclass.task, rdesc)
  expect_true(!is.na(r$aggr))
})

test_that("training performance works as expected (#1357)", {
  num = makeMeasure(id = "num", minimize = FALSE,
    properties = c("classif", "classif.multi", "req.pred", "req.truth"),
    name = "Number",
    fun = function(task, model, pred, feats, extra.args) {
      length(pred$data$response)
    })

  y = binaryclass.df[, binaryclass.target]
  z = getMinMaxClass(y)
  rdesc = makeResampleDesc("Holdout", split = .5, predict = "both")

  lrn = makeUndersampleWrapper("classif.rpart", usw.rate = 0.1, usw.cl = z$max.name)
  r = resample(lrn, binaryclass.task, rdesc, measures = list(setAggregation(num, train.mean)))
  expect_lt(r$measures.train$num, getTaskSize(binaryclass.task) * 0.5 - 1)

  lrn = makeOversampleWrapper("classif.rpart", osw.rate = 2, osw.cl = z$max.name)
  r = resample(lrn, binaryclass.task, rdesc, measures = list(setAggregation(num, train.mean)))
  expect_gt(r$measures.train$num, getTaskSize(binaryclass.task) * 0.5 + 1)
})

test_that("Wrapper works with weights, we had issue #2047", {
  n = nrow(binaryclass.df)
  w = 1:n
  task = makeClassifTask(data = binaryclass.df, target = binaryclass.target, weights = w)
  b = table(getTaskTargets(task))

  # weights from task, use all
  lrn = makeOversampleWrapper("classif.__mlrmocklearners__6", osw.rate = 1)
  m = train(lrn, task)
  expect_set_equal(getLearnerModel(m, more.unwrap = TRUE)$weights, w)

  lrn = makeUndersampleWrapper("classif.__mlrmocklearners__6", usw.rate = 1)
  m = train(lrn, task)
  expect_set_equal(getLearnerModel(m, more.unwrap = TRUE)$weights, w)

  # weights from task, really sample
  lrn = makeOversampleWrapper("classif.__mlrmocklearners__6", osw.rate = 2)
  m = train(lrn, task)
  u = getLearnerModel(m, more.unwrap = TRUE)$weights
  expect_equal(length(u), min(b) * 2 + max(b))
  expect_subset(u, w)

  lrn = makeUndersampleWrapper("classif.__mlrmocklearners__6", usw.rate = 0.5)
  m = train(lrn, task)
  u = getLearnerModel(m, more.unwrap = TRUE)$weights
  expect_equal(length(u), round(max(b) / 2) + min(b))
  expect_subset(u, w)

  # weights from train
  subset = c(head(which(getTaskTargets(task) == names(b)[1]), 5), head(which(getTaskTargets(task) == names(b)[2]), 5))
  lrn = makeOversampleWrapper("classif.__mlrmocklearners__6", osw.rate = 2)
  m = train(lrn, task, subset = subset, weights = 1:10)
  u = getLearnerModel(m, more.unwrap = TRUE)$weights
  expect_equal(length(u), 15)
  expect_subset(u, 1:10)

  lrn = makeUndersampleWrapper("classif.__mlrmocklearners__6", usw.rate = 2 / 5)
  m = train(lrn, task, subset = subset, weights = 1:10)
  u = getLearnerModel(m, more.unwrap = TRUE)$weights
  expect_equal(length(u), 7)
  expect_subset(u, 1:10)
})

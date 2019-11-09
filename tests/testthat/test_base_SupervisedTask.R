context("SupervisedTask")

test_that("SupervisedTask", {
  ct1 = multiclass.task

  expect_equal(getTaskTargetNames(ct1), "Species")
  expect_equal(getTaskTargets(ct1), multiclass.df[, multiclass.target])

  ct = binaryclass.task
  td = getTaskDesc(ct)
  pn = c(td$positive, td$negative)
  expect_true(setequal(getTaskClassLevels(ct), sort(pn)))

  ct2 = subsetTask(ct, subset = 1:150)
  expect_equal(getTaskDesc(ct)$positive, getTaskDesc(ct2)$positive)
  ct2 = subsetTask(ct, subset = 1:150, features = colnames(binaryclass.df)[1:2])
  expect_equal(getTaskSize(ct2), 150)
  expect_equal(sum(getTaskNFeats(ct2)), 2)
  ct2 = subsetTask(ct, subset = 1:150, features = colnames(binaryclass.df)[1:2])
  expect_equal(getTaskSize(ct2), 150)
  expect_equal(sum(getTaskNFeats(ct2)), 2)

  # wrong data
  expect_error(makeClassifTask(data = 44, target = "y"), "'data.frame'")

  # wrong target type
  expect_error(makeClassifTask(data = regr.df, target = regr.target), "'factor'")
  expect_error(makeRegrTask(data = multiclass.df, target = multiclass.target), "'numeric'")

  # wrong vars
  expect_error(subsetTask(multiclass.task, features = c("Sepal.Length", "x", "y")))

  # check missing accessors
  df = multiclass.df
  df[1, 1:3] = NA
  df[2, 1:3] = NA
  ct = makeClassifTask(data = df, target = multiclass.target)
  expect_true(getTaskDesc(ct)$has.missings)

  # check that blocking is still there after subsetting
  ct1 = makeClassifTask(data = multiclass.df, target = multiclass.target,
    blocking = as.factor(seq_len(nrow(multiclass.df))))
  expect_true(getTaskDesc(ct1)$has.blocking)
  ct2 = subsetTask(ct1)
  expect_true(getTaskDesc(ct2)$has.blocking)
})

test_that("SupervisedTask dropping of levels works", {
  d = iris
  levs1 = levels(iris$Species)
  levs2 = c(levs1, "foo")
  levels(d$Species) = levs2
  task = makeRegrTask(data = d, target = colnames(iris)[1], fixup.data = "quiet")
  e = getTaskData(task)
  expect_true(setequal(levels(e$Species), levs1))
  expect_warning({
    task = makeRegrTask(data = d, target = colnames(iris)[1], fixup.data = "warn")
  },
  "Empty factor levels")
  e = getTaskData(task)
  expect_true(setequal(levels(e$Species), levs1))

  expect_warning(makeMultilabelTask("multilabel", multilabel.df[1:10, ], target = c("y1", "y2"), fixup.data = "warn"),
    "Empty factor levels")

  expect_warning(makeMultilabelTask("multilabel", multilabel.df[1:10, ], target = c("y1", "y2"), fixup.data = "quiet"), NA)
})

test_that("SupervisedTask does not drop positive class", {
  data = iris[1:100, ]
  expect_warning({
    task = makeClassifTask(data = data, target = "Species")
  }, "empty factor levels")
  td = getTaskDesc(task)
  expect_true(setequal(c(td$positive, td$negative), unique(data$Species)))
})

test_that("Task $type and $task.desc$type agree", {
  check = list(
    classif = binaryclass.task,
    multilabel = multilabel.task,
    regr = regr.task,
    surv = surv.task,
    costsens = costsens.task)
  for (type in names(check)) {
    expect_identical(type, check[[type]]$type)
    expect_identical(type, getTaskDesc(check[[type]])$type)
  }
})

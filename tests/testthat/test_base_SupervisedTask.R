context("SupervisedTask")

test_that("SupervisedTask", {
  ct1 = multiclass.task

  expect_equal(ct1$task.desc$target, "Species")
  expect_equal(getTaskTargets(ct1), multiclass.df[,multiclass.target])

  ct = binaryclass.task
  pn = c(ct$task.desc$positive, ct$task.desc$negative)
  expect_equal(sort(ct$task.desc$class.levels), sort(pn))

  ct2 = subsetTask(ct, subset = 1:150)
  expect_equal(ct$task.desc$positive, ct2$task.desc$positive)
  ct2 = subsetTask(ct, subset = 1:150, features = colnames(binaryclass.df)[1:2])
  expect_equal(ct2$task.desc$size, 150)
  expect_equal(sum(ct2$task.desc$n.feat), 2)
  ct2 = subsetTask(ct, subset = 1:150, features = colnames(binaryclass.df)[1:2])
  expect_equal(ct2$task.desc$size, 150)
  expect_equal(sum(ct2$task.desc$n.feat), 2)

  # wrong data
  expect_error(makeClassifTask(data = 44, target = "y"),
    "be a data.frame")

  # wrong target type
  expect_error(makeClassifTask(data = regr.df, target = regr.target),
    "be a factor")
  expect_error(makeRegrTask(data = multiclass.df, target = multiclass.target),
    "be numeric")

  # wrong vars
  expect_error(subsetTask(multiclass.task, vars = c("Sepal.Length", "x", "y")))

  # check missing accessors
  df = multiclass.df
  df[1,1:3] = NA
  df[2,1:3] = NA
  ct = makeClassifTask(data = df, target = multiclass.target)
  expect_true(ct$task.desc$has.missings)

  # check that blocking is still there after subsetting
  ct1 = makeClassifTask(data = multiclass.df, target = multiclass.target, blocking = as.factor(1:nrow(multiclass.df)))
  expect_true(ct1$task.desc$has.blocking)
  ct2 = subsetTask(ct1)
  expect_true(ct2$task.desc$has.blocking)
})

test_that("SupervisedTask dropping of levels works", {
  d = iris
  levs1 = levels(iris$Species)
  levs2 = c(levs1, "foo")
  levels(d$Species) = levs2
  task = makeRegrTask(data = d, target = colnames(iris)[1], fixup.data = "quiet")
  e = getTaskData(task)
  expect_true(setequal(levels(e$Species), levs1))
  expect_warning({task = makeRegrTask(data = d, target = colnames(iris)[1], fixup.data = "warn")},
    "Empty factor levels")
  e = getTaskData(task)
  expect_true(setequal(levels(e$Species), levs1))
})

test_that("SupervisedTask does not drop positive class", {
  data = iris[1:100, ]
  expect_warning({task = makeClassifTask(data = data, target = "Species")}, "Empty factor levels")
  expect_true(setequal(c(task$task.desc$positive, task$task.desc$negative), unique(data$Species)))
})

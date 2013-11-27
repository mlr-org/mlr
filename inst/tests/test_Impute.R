context("Impute")

test_that("Impute data frame", {
  data = data.frame(f = letters[c(1,1,1,1,2)], x = rep(1., 5), y = c(1, 2, 3, 3, 4), z = NA)
  target = "z"
  data[6, ] = NA

  # median
  imputed = impute(data, target, cols=list(x = imp.median(), y = imp.median()))$data
  expect_equal(imputed$x[6], 1)
  expect_equal(imputed$y[6], 3)

  # mode
  imputed = impute(data, target, cols=list(f = imp.mode(), x = imp.mode(), y = imp.mode()))$data
  expect_equal(as.character(imputed$f[6]), "a")
  expect_equal(imputed$x[6], 1)
  expect_equal(imputed$y[6], 3)

  # min / max
  imputed = impute(data, target, cols=list(x = imp.min(2), y = imp.max(2)))$data
  expect_equal(imputed$x[6], 2)
  expect_equal(imputed$y[6], 8)

  # normal
  imputed = impute(data, target, cols=list(x = imp.normal(), y = imp.normal()))$data
  expect_equal(imputed$x[6], 1)

  # hist / table
  imputed = impute(data, target, cols=list(f = imp.hist(), x = imp.hist(), y=imp.hist(breaks=1, use.mids=FALSE)))$data
  expect_true(imputed$f[6] %in% c("a", "b"))
  expect_equal(imputed$x[6], 0.5)
  expect_equal(imputed$x[6], 0.5)
  expect_true(imputed$y[6] >= 0 && imputed$y[6] <= 5)

  # constant replacements
  imputed = impute(data, target, cols=list(f = "xxx", x = 999, y = 1000))$data
  expect_equal(as.character(imputed$f[6]), "xxx")
  expect_equal(imputed$x[6], 999)
  expect_equal(imputed$y[6], 1000)

  # some reimputations
  x = impute(data, target, cols=list(f = "xxx", x = imp.mode(), y = imp.max(2)), impute.newlevels=TRUE)
  imputed = reimpute(data, x$desc)
  expect_equal(x$data, imputed)
  imputed = reimpute(data.frame(f=factor("newlvl"), x=NA), x$desc)
  expect_equal(as.character(imputed$f), "xxx")
  expect_equal(imputed$x, 1)
  expect_equal(imputed$y, 8)

  x = impute(data, target, cols=list(f = "xxx"), impute.newlevels=FALSE)
  imputed = reimpute(data.frame(f=factor("newlvl"), x=NA), x$desc)
  expect_true(is.na(imputed$f))
  expect_true("xxx" %in% levels(imputed$f))

  # dummies
  imputed = impute(data, target, dummies="x")$data
  expect_equal(imputed[["x.dummy"]], c(rep(FALSE, 5), TRUE))


})

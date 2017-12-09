context("classif_binomial_spatial")

test_that("classif_binomial_spatial", {

  parset.list1 = list(
    list(family = binomial),
    list(family = binomial(link = "logit")),
    list(family = binomial(link = "cloglog"))
  )

  parset.list2 = list(
    list(),
    list(link = "logit"),
    list(link = "cloglog")
  )

  old.predicts.list = list()
  old.probs.list = list()

  for (i in seq_along(parset.list1)) {
    parset = parset.list1[[i]]
    set.seed(getOption("mlr.debug.seed"))
    m = glm(formula = binaryclass.spatial.formula,
      data = binaryclass.spatial.train, family = parset$family)
    p  = predict(m, newdata = binaryclass.spatial.test, type = "response")
    p = 1 - p
    p.class = as.factor(binaryclass.spatial.class.levs[ifelse(p > 0.5, 1, 2)])
    old.predicts.list[[i]] = p.class
    old.probs.list[[i]] = p
  }

  testSimpleParsets("classif.binomial", binaryclass.spatial.df,
                    binaryclass.spatial.target, binaryclass.spatial.train.inds,
                    old.predicts.list, parset.list2)
  testProbParsets("classif.binomial", binaryclass.spatial.df,
                  binaryclass.spatial.target, binaryclass.spatial.train.inds,
                  old.probs.list, parset.list2)
})

# check for error if spatial = TRUE and coordinates are not named 'x' or 'y'
test_that("errors if spatial = TRUE and coordinates are not named 'x' or 'y'", {
  binaryclass.spatial.df$x = NULL
  expect_error(makeClassifTask("data", binaryclass.spatial.df,
    target = "diplo01", spatial = TRUE),
    "Please rename coordinates in data to 'x' and 'y'")
})

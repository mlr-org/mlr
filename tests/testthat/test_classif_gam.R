context("classif_gam")

test_that("classif_gam_binomial", {
  requirePackagesOrSkip("mgcv", default.method = "load")
  # train("classif.gam", sonar.task)
  fo = "Class ~ s(V57) + s(V58) + s(V59) + s(V60)"

  m = mgcv::gam(formula = BBmisc::asQuoted(fo), explicit.features = TRUE,
    data = binaryclass.train[, 57:61], control = learnerArgsToControl(mgcv::gam.control),
    family = binomial)
  p = predict(m, newdata = binaryclass.test, type = "response")
  p.prob = 1 - p
  p.class = as.factor(binaryclass.class.levs[ifelse(p > 0.5, 2, 1)])

  testSimple("classif.gam", binaryclass.df, binaryclass.target, binaryclass.train.inds, p.class, parset = list(formula = fo))

})

test_that("classif_gam_binomial", {
  requirePackagesOrSkip("mgcv", default.method = "load")

  fo = "Class ~ s(V57) + s(V58) + s(V59) + s(V60)"

  parset.list1 = list(
    list(family = binomial(link = "logit")),
    list(family = binomial(link = "cloglog")),
    list(family = binomial(link = "probit")),
    list(family = binomial(link = "cauchit")),
    list(family = quasibinomial(link = "logit")),
    list(family = quasibinomial(link = "probit")),
    list(family = quasibinomial(link = "identity")),
    #list(family = quasibinomial(link = "inverse")),
    #list(family = quasibinomial(link = "sqrt"))
    list(family = quasibinomial(link = "1/mu^2"))
    #list(family = nb(link = "log"))
    #list(family = nb(link = "identity"))
    #list(family = nb(link = "sqrt"))
  )

  # parset.list2 = list(
  #   list(family = "binomial", binomial.link = "logit", formula = fo),
  #   list(family = "binomial", binomial.link = "cloglog", formula = fo),
  #   #list(family = "binomial", binomial.link = "probit", formula = fo),
  #   list(family = "binomial", binomial.link = "cauchit", formula = fo),
  #   list(family = "quasibinomial", quasibinomial.link = "logit", formula = fo),
  #   list(family = "quasibinomial", quasibinomial.link = "identity", formula = fo),
  #   list(family = "quasibinomial.link", quasibinomial.link = "probit", formula = fo),
  #   #list(family = "quasibinomial.link", quasibinomial.link = "inverse", formula = fo),
  #   #list(family = "quasibinomial.link", quasibinomial.link = "sqrt", formula = fo),
  #   list(family = "quasibinomial", quasibinomial.link = "1/mu^2", formula = fo),
  #   list(family = "negbin", nb.link = "log", formula = fo),
  #   list(family = "negbin", nb.link = "identity", formula = fo),
  #   list(family = "negbin", nb.link = "sqrt", formula = fo)
  # )

  old.predicts.list = list()
  old.probs.list = list()

  binaryclass.formula = paste0("Class ~ ", paste(sprintf("s(V%d) + ", 57:60), collapse = ""), collapse = "")
  binaryclass.formula = BBmisc::asQuoted(substr(binaryclass.formula, 1, nchar(binaryclass.formula) - 3))

  # throws warnings due to gam behaviour + dataset.
  # important here is that parset combinations work -> they do if no error occurs
  expect_warning(for (i in seq_along(parset.list1)) {
    parset = parset.list1[[i]]
    # print(paste0(parset$family$family, " ", parset$family$link))
    set.seed(getOption("mlr.debug.seed"))
    m = mgcv::gam(data = binaryclass.train[, 57:61],
                  family = parset[[1]], formula = BBmisc::asQuoted(fo))
    p  = predict(m, newdata = binaryclass.test[, 57:61], type = "response")
    p = 1 - p
    p.class = as.factor(binaryclass.class.levs[ifelse(p > 0.5, 1, 2)])
    old.predicts.list[[i]] = p.class
    old.probs.list[[i]] = p
  })

  # not working
  # testSimpleParsets("classif.gam", binaryclass.df[, 57:61], binaryclass.target, binaryclass.train.inds,
  #                   old.predicts.list, parset.list2)
  #
  # testProbParsets("classif.gam", binaryclass.df[, 57:61], binaryclass.target, binaryclass.train.inds,
  #                 old.probs.list, parset.list2)
})

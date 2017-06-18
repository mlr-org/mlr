context("cpo properties")

test_that("attaching cpo with mismatching properties gives error", {
  # check along multiple levels
  # even after detaching from learner


  expectCpo = function(cpo) {
    if ("CPO" %in% class(cpo)) {
      retr = cpo.df1 %>>% cpo
      expect_equal(cpo.df1 %>>% retrafo(retr), cpo.df1)
      retrafo(retr) = NULL
      expect_equal(retr, cpo.df1)

      retr = cpo.df1c %>>% cpo
      expect_equal(cpo.df1 %>>% retrafo(retr), cpo.df1)
      expect_equal(getTaskData(cpo.df1c %>>% retrafo(retr)), cpo.df1)
      expect_equal(getTaskData(retr), cpo.df1)
    } else {
      expect_class(cpo, "Learner")
      expect_class(predict(train(cpo, cpo.df1c), cpo.df1c), "Prediction")
    }
  }

  cponoop = function(data, ...) { control = 0 ; data }

  properlrn = function(properties) {
    t = testlearnercpo
    t$properties = c(properties, "twoclass")
    t
  }

  getRestrictedCPOProperties = function(x) {
    lapply(getCPOProperties(x), function(y)
      intersect(y, cpo.dataproperties))
  }
  getRestrictedLearnerProperties = function(x) {
    intersect(getLearnerProperties(x), cpo.dataproperties)
  }

  for (type in c("o", "f")) {

    propercpo = function(properties, adding = character(0), needed = character(0), ...) {
      cpogen("propertytest", type, pss(), cponoop, cponoop, "task", properties, adding, needed)(...)
    }

    expect_set_equal(getRestrictedCPOProperties(propercpo(c("numerics", "factors")))$properties, c("numerics", "factors"))

    expect_set_equal(getRestrictedCPOProperties(propercpo(c("numerics", "factors")) %>>% propercpo(c("factors", "ordered")))$properties, "factors")

    expect_set_equal(getRestrictedCPOProperties(propercpo(c("numerics", "factors"), "numerics") %>>%
                                      propercpo(c("factors", "ordered")))$properties, c("factors", "numerics"))

    expect_error(propercpo("numerics", "factors"), "Must be a subset")
    expectCpo(propercpo(c("numerics", needed = "factors")))

    # cpo1 %>>% cpo2, cpo1 needs properties that cpo2 doesnt have

    expectCpo(propercpo(c("numerics", "factors"), needed = "missings") %>>% propercpo(c("numerics", "factors", "missings")))
    expect_error(propercpo(c("numerics", "factors"), needed = "missings") %>>% propercpo(c("numerics", "factors")),
      "property missings that \\w* can not handle")

    # property requirements ignored if subsetting
    expectCpo(propercpo(c("numerics", "factors"), needed = "missings") %>>% propercpo(c("numerics", "factors"), affect.type = "numeric"))

    needsmissing = propercpo(c("numerics", "factors"), needed = "missings")
    needsordered = propercpo(c("numerics", "factors"), needed = "ordered")
    hasmissing = propercpo(c("numerics", "factors", "missings"))
    hasordered = propercpo(c("numerics", "factors", "ordered"))
    hasnomissing = propercpo(c("numerics", "factors"))
    addsmissing = propercpo(c("numerics", "factors", "missings"), adding = "missings")
    addsordered = propercpo(c("numerics", "factors", "ordered"), adding = "ordered")

    # cpo1 %>>% cpo2 %>>% cpo3, cpo1 needs properties that cpo3 doesnt have
    expectCpo((needsmissing %>>% hasmissing) %>>% hasmissing)
    expectCpo(needsmissing %>>% (hasmissing %>>% hasmissing))

    expect_error((needsmissing %>>% hasmissing) %>>% hasnomissing,
      "property missings that \\w* can not handle")

    expect_error(needsmissing %>>% (hasmissing %>>% hasnomissing),
      "property missings that .* can not handle")

    # cpo1 %>>% lrn, cpo1 needs properties that lrn doesn't have
    lrn = needsordered %>>% properlrn(c("numerics", "factors", "ordered"))
    expect_set_equal(getRestrictedLearnerProperties(lrn), c("numerics", "factors"))
    expectCpo(lrn)

    expect_error(needsmissing %>>% hasordered,
      "property missings that \\w* can not handle")

    # cpo1 %>>% cpo2 %>>% lrn, cpo1 needs properties that lrn doesn't have
    lrn = (needsordered %>>% hasordered) %>>%
      properlrn(c("numerics", "factors", "ordered"))
    expect_set_equal(getRestrictedLearnerProperties(lrn), c("numerics", "factors"))
    expectCpo(lrn)

    expect_error((needsmissing %>>% hasordered) %>>% hasordered,
      "property missings that \\w* can not handle")

    lrn = needsordered %>>%
      (hasordered %>>%
      properlrn(c("numerics", "factors", "ordered")))
    expect_set_equal(getRestrictedLearnerProperties(lrn), c("numerics", "factors"))
    expectCpo(lrn)

    expect_error(needsmissing %>>% (hasordered %>>% hasordered),
      "property missings that .* can not handle")

    # cpo1 %>>% cpo2 %>>% cpo3, cpo1 needs properties that cpo3 doesn't have, but cpo2 adds
    expectCpo((needsmissing %>>% addsmissing) %>>% hasnomissing)


    expectCpo(needsmissing %>>% (addsmissing %>>% hasnomissing))

    # cpo1 %>>% cpo2 %>>% lrn, cpo1 needs properties that lrn doesn't have, but cpo2 adds
    lrn = (needsordered %>>% addsordered) %>>% properlrn(c("numerics", "factors"))
    expect_set_equal(getRestrictedLearnerProperties(lrn), c("numerics", "factors"))
    expectCpo(lrn)

    lrn = propercpo(c("numerics", "factors", "ordered"), needed = "ordered") %>>%
      (addsordered %>>% properlrn(c("numerics", "factors")))
    expect_set_equal(getRestrictedLearnerProperties(lrn), c("numerics", "factors", "ordered"))
    expectCpo(lrn)

    # cpo1 %>>% cpo2 %>>% cpo3 %>>% cpo4, cpo1 needs properties that cpo4 doesn't have, but cpo2  adds
    expectCpo((needsmissing %>>% addsmissing) %>>% hasmissing %>>% hasnomissing)
    expectCpo((needsmissing %>>% addsmissing) %>>% (hasmissing %>>% hasnomissing))
    expectCpo(needsmissing %>>% ((addsmissing %>>% hasmissing) %>>% hasnomissing))
    expectCpo(needsmissing %>>% (addsmissing %>>% (hasmissing %>>% hasnomissing)))

    # cpo1 %>>% cpo2 %>>% cpo3 %>>% cpo4, cpo1 needs properties that cpo4 doesn't have, but cpo3 adds
    expectCpo((needsmissing %>>% hasmissing) %>>% addsmissing %>>% hasnomissing)
    expectCpo((needsmissing %>>% hasmissing) %>>% (addsmissing %>>% hasnomissing))
    expectCpo(needsmissing %>>% ((hasmissing %>>% addsmissing) %>>% hasnomissing))
    expectCpo(needsmissing %>>% (hasmissing %>>% (addsmissing %>>% hasnomissing)))

    # cpo1 %>>% cpo2 %>>% cpo3 %>>% lrn, cpo1 needs properties that lrn doesn't have, but cpo2 adds
    expectCpo((needsmissing %>>% addsmissing) %>>% hasmissing %>>% properlrn(c("numerics", "factors")))
    expectCpo((needsmissing %>>% addsmissing) %>>% (hasmissing %>>% properlrn(c("numerics", "factors"))))
    expectCpo(needsmissing %>>% ((addsmissing %>>% hasmissing) %>>% properlrn(c("numerics", "factors"))))
    expectCpo(needsmissing %>>% (addsmissing %>>% (hasmissing %>>% properlrn(c("numerics", "factors")))))

    # cpo1 %>>% cpo2 %>>% cpo3 %>>% lrn, cpo1 needs properties that lrn doesn't have, but cpo3 adds
    expectCpo((needsmissing %>>% hasmissing) %>>% addsmissing %>>% properlrn(c("numerics", "factors")))
    expectCpo((needsmissing %>>% hasmissing) %>>% (addsmissing %>>% properlrn(c("numerics", "factors"))))
    expectCpo(needsmissing %>>% ((hasmissing %>>% addsmissing) %>>% properlrn(c("numerics", "factors"))))
    expectCpo(needsmissing %>>% (hasmissing %>>% (addsmissing %>>% properlrn(c("numerics", "factors")))))

    expect_error((needsmissing %>>% hasnomissing) %>>% addsmissing %>>% properlrn(c("numerics", "factors")),
      "property missings that .* can not handle")
    expect_error((needsmissing %>>% hasnomissing) %>>% (addsmissing %>>% properlrn(c("numerics", "factors"))),
      "property missings that .* can not handle")
    expect_error(needsmissing %>>% ((hasnomissing %>>% addsmissing) %>>% properlrn(c("numerics", "factors"))),
      "property missings that .* can not handle")
    expect_error(needsmissing %>>% (hasnomissing %>>% (addsmissing %>>% properlrn(c("numerics", "factors")))),
      "property missings that .* can not handle")
  }
  # TODO: check retrafos,
})

test_that("returning properties that are not allowed is detected", {
# missings, numerics, factors, ordered
# allowed is: - it is input, in 'properties' and not in adding
#             - it is in 'needed'
# not allowed is: - it is not in input and not in 'needed'
#                 - it is in input, but it is in adding
# during training
# during prediction (only missings, factors, ordered

  for (type in c("o", "f")) {

    makepcpo = function(split, properties.present, astask, generate, properties, adding = character(0), needed = character(0), inretrafo = FALSE,
                        retrafoprops = properties.present, convertinretrafo = inretrafo) {
      converter = switch(generate,
        numerics = as.numeric,
        factors = as.factor,
        ordered = as.ordered,
        missings = function(x) {
          x[1] = NA
          x
        })
      collectTaskProps = function(x) {
        td = getTaskDesc(x)
        c(if (td$n.feat["numerics"] > 0) "numerics",
          if (td$n.feat["factors"] > 0) "factors",
          if (td$n.feat["ordered"] > 0) "ordered",
          if (td$has.missings) "missings")
      }
      convert = function(data, target, split) {
        if (split %in% c("most", "all")) {
            if ("numerics" %in% properties.present) {
              data$numeric[[1]] = converter(data$numeric[[1]])
            } else {
              data$factor[[1]] = converter(data$factor[[1]])
            }
          } else if (split == "task") {
            task = data
            data = getTaskData(task)
            idx = 1 + (names(data)[1] %in% getTaskTargetNames(task))
            data[[idx]] = converter(data[[idx]])
            data = changeData(task, data)
          } else {
            idx = 1 + (split == "no" && names(data)[1] %in% target)
            data[[idx]] = converter(data[[idx]])
          }
        data
      }

      makedata = function(properties.present) {
        data = c(
          if ("numerics" %in% properties.present) list(cpo.df.numeric[1]),
          if ("factors" %in% properties.present) list(cpo.df.factorial[1]),
          if ("ordered" %in% properties.present) list(cpo.df.ordered[1]))
        data = do.call(cbind, data)
        if ("missings" %in% properties.present) {
          data[1, 1] = NA
        }
        names(data) = paste("x", seq_along(data), sep = ".")
        if (astask) {
          data = makeClassifTask("task", cbind(data, data.frame(Z = c("a", "a", "a"))), target = "Z")
        }
        data
      }

      cpo = cpogen("propertygen", type, pss(), function(data, target, ...) {
        control = 0
        if (!convertinretrafo) {
          data = convert(data, target, split)
        }
        data
      }, function(data, ...) {
        if (convertinretrafo) {
          if (split %in% c("no", "task")) {
            split = "target"
          }
          data = convert(data, NULL, split)
        }
        data
      }, split, properties, adding, needed)()

      r1 = makedata(properties.present) %>>% cpo
      if (inretrafo) r2 = makedata(retrafoprops) %>>% retrafo(r1)
      if (!astask) {
        r1 = makeClassifTask("task", cbind(r1, data.frame(Z = c("a", "a", "a"))), target = "Z")
        if (inretrafo) r2 = makeClassifTask("task", cbind(r2, data.frame(Z = c("a", "a", "a"))), target = "Z")
      }
      c(list(collectTaskProps(r1)), if (inretrafo) list(collectTaskProps(r2)))
    }

    for (split in c("task", "no", "target", "most", "all")) {

      for (astask in c(TRUE, FALSE)) {
        expect_equal(makepcpo(split, "numerics", astask, "numerics", "numerics", inretrafo = FALSE), list("numerics"))

        expect_error(makepcpo(split, "numerics", astask, "numerics", "factors", inretrafo = FALSE), "has property numerics that .* can not handle")
        expect_error(makepcpo(split, "ordered", astask, "numerics", "factors", inretrafo = FALSE), "has property ordered that .* can not handle")
        expect_error(makepcpo(split, "factors", astask, "numerics", "numerics", inretrafo = FALSE), "has property factors that .* can not handle")
        expect_error(makepcpo(split, c("numerics", "missings"), astask, "numerics", "numerics", inretrafo = FALSE), "has property missings that .* can not handle")

        expect_equal(makepcpo(split, c("factors", "numerics"), astask, "factors", c("numerics", "factors", "ordered"), inretrafo = FALSE),
          list("factors"))

        expect_equal(makepcpo(split, c("ordered", "numerics"), astask, "ordered", c("numerics", "factors", "ordered"), inretrafo = FALSE),
          list("ordered"))

        expect_equal(makepcpo(split, c("ordered", "numerics", "missings"), astask, "missings", c("numerics", "missings", "ordered"), inretrafo = FALSE),
          list(c("numerics", "ordered", "missings")))

        expect_equal(makepcpo(split, "numerics", astask, "factors", c("numerics", "missings", "ordered"), needed = "factors", inretrafo = FALSE),
          list("factors"))
        expect_equal(makepcpo(split, "numerics", astask, "ordered", c("numerics", "missings", "ordered"), needed = "ordered", inretrafo = FALSE),
          list("ordered"))
        expect_equal(makepcpo(split, "numerics", astask, "missings", c("numerics", "missings", "ordered"), needed = "missings", inretrafo = FALSE),
          list(c("numerics", "missings")))
        expect_equal(makepcpo(split, "factors", astask, "numerics", c("numerics", "factors", "ordered"), needed = "numerics", inretrafo = FALSE),
          list("numerics"))


        expect_error(makepcpo(split, "numerics", astask, "factors", c("numerics", "factors", "ordered"), inretrafo = FALSE), "factors .*did not declare")
        expect_error(makepcpo(split, "numerics", astask, "ordered", c("numerics", "factors", "ordered"), inretrafo = FALSE), "ordered .*did not declare")
        expect_error(makepcpo(split, "numerics", astask, "missings", c("numerics", "factors", "missings"), inretrafo = FALSE), "missings .*did not declare")
        expect_error(makepcpo(split, "factors", astask, "ordered", c("numerics", "factors", "ordered"), inretrafo = FALSE), "ordered.*did not declare")

        expect_equal(makepcpo(split, c("numerics", "factors"), astask, "numerics",
          c("numerics", "factors", "ordered", "missings"), inretrafo = FALSE),
          list(c("numerics", "factors")))

        expect_error(makepcpo(split, c("numerics", "factors"), astask, "numerics",
          c("numerics", "factors", "ordered", "missings"), adding = "factors", inretrafo = FALSE), "factors .*properties.adding")
        expect_error(makepcpo(split, c("numerics", "ordered"), astask, "numerics",
          c("numerics", "factors", "ordered", "missings"), adding = "ordered", inretrafo = FALSE), "ordered .*properties.adding")
        expect_error(makepcpo(split, c("numerics", "missings"), astask, "numerics",
          c("numerics", "factors", "ordered", "missings"), adding = "missings", inretrafo = FALSE), "missings .*properties.adding")
        expect_error(makepcpo(split, c("numerics", "missings"), astask, "numerics",
          c("numerics", "factors", "ordered", "missings"), adding = "numerics", inretrafo = FALSE), "numerics .*properties.adding")



        expect_equal(makepcpo(split, "numerics", astask, "numerics", "numerics", inretrafo = TRUE), list("numerics", "numerics"))


        expect_error(makepcpo(split, "factors", astask, "numerics", "factors", inretrafo = TRUE, retrafoprops = "numerics"),
          "mismatches between training and test data.")

        if (split != "all") {
          expect_error(makepcpo(split, "factors", astask, "numerics", "factors", inretrafo = TRUE, retrafoprops = "ordered"),
            "retrafo.*ordered.*can not handle")
          expect_error(makepcpo(split, "ordered", astask, "numerics", "ordered", inretrafo = TRUE, retrafoprops = "factors"),
            "retrafo.*factors.*can not handle")
        }
        expect_error(makepcpo(split, "factors", astask, "numerics", "factors", inretrafo = TRUE, retrafoprops = c("factors", "missings")),
          "retrafo.*missings.*can not handle")

        if (split != "all") {
          expect_error(makepcpo(split, "factors", astask, "ordered", c("numerics", "factors", "ordered"), inretrafo = TRUE),
            "retrafo.*ordered.*properties.needed")

          expect_equal(makepcpo(split, "factors", astask, "ordered", c("numerics", "factors", "ordered"), needed = "ordered", inretrafo = TRUE),
            list("factors", "ordered"))

          expect_equal(makepcpo(split, c("factors", "ordered"), astask, "ordered", c("numerics", "factors", "ordered"), inretrafo = TRUE),
            list(c("factors", "ordered"), "ordered"))

          expect_equal(makepcpo(split, "factors", astask, "ordered", c("numerics", "factors", "ordered"),
            needed = "ordered", inretrafo = TRUE, convertinretrafo = FALSE),
            list("ordered", "factors"))

          expect_error(makepcpo(split, "factors", astask, "ordered", c("numerics", "factors", "ordered"),
            needed = "ordered", adding = "factors", inretrafo = TRUE, convertinretrafo = FALSE),
            "retrafo.*factors.*adding")
        }
        expect_error(makepcpo(split, "factors", astask, "missings", c("numerics", "factors", "ordered", "missings"), inretrafo = TRUE),
          "retrafo.*missings.*properties.needed")

        expect_equal(makepcpo(split, "factors", astask, "missings", c("numerics", "factors", "ordered", "missings"), needed = "missings", inretrafo = TRUE),
          list("factors", c("factors", "missings")))


        expect_error(makepcpo(split, c("numerics", "factors"), astask, "numerics",
          c("numerics", "factors", "ordered", "missings"), adding = "factors", inretrafo = FALSE), "factors .*properties.adding")
        expect_error(makepcpo(split, c("numerics", "ordered"), astask, "numerics",
          c("numerics", "factors", "ordered", "missings"), adding = "ordered", inretrafo = FALSE), "ordered .*properties.adding")
        expect_error(makepcpo(split, c("numerics", "missings"), astask, "numerics",
          c("numerics", "factors", "ordered", "missings"), adding = "missings", inretrafo = FALSE), "missings .*properties.adding")
        expect_error(makepcpo(split, c("numerics", "missings"), astask, "numerics",
          c("numerics", "factors", "ordered", "missings"), adding = "numerics", inretrafo = FALSE), "numerics .*properties.adding")
      }
    }
  }
})

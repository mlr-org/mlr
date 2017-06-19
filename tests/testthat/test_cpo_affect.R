
context("cpo affect subset")

test_that("right columns are selected by affect.*", {

  expect.cpo = makeCPO("testvalue", .par.set = makeParamSet(makeUntypedLearnerParam("equand")), .datasplit = "no", cpo.trafo = {
    expect_equal(data, equand) ; control = 0 ; data }, cpo.retrafo = { expect_equal(data, equand) ; data })

  iris %>>% expect.cpo(equand = iris[c(2, 1, 3, 5)], affect.type = "factor", affect.index = c(2, 1), affect.names = c("Petal.Length", "Sepal.Width"))

  iris %>>% expect.cpo(equand = iris[c("Sepal.Width", "Petal.Width")], affect.pattern = "Width")

  expect_error(iris %>>% expect.cpo(equand = "test", affect.name = "nosuchcol"), "not found.*nosuchcol")

  expect_error(iris %>>% expect.cpo(equand = "test", affect.index = 1000), "undefined columns selected")

  iris %>>% expect.cpo(equand = iris[4], affect.type = "factor", affect.index = c(2, 1), affect.names = c("Petal.Length", "Sepal.Width"), affect.invert = TRUE)
})

test_that("nonincreasing index ordering doesn't break things", {

  for (split in c("task", "no", "target", "most", "all", "factor", "onlyfactor", "ordered", "numeric")) {

    cpo = makeCPO("testorder", .datasplit = split, cpo.trafo = { control = 0 ; data }, cpo.retrafo = { data })(affect.index = c(4, 3), affect.name = c("F3", "N1"))

    trafod = cpo.df5 %>>% cpo
    expect_equal(cpo.df5 %>>% retrafo(trafod), cpo.df5)
    expect_equal(getTaskData(cpo.df5c %>>% retrafo(trafod)), cpo.df5)
    retrafo(trafod) = NULL
    expect_equal(trafod, cpo.df5)

    trafod = cpo.df5c %>>% cpo
    expect_equal(cpo.df5 %>>% retrafo(trafod), cpo.df5)
    expect_equal(getTaskData(cpo.df5c %>>% retrafo(trafod)), cpo.df5)
    retrafo(trafod) = NULL
    expect_equal(getTaskData(trafod), cpo.df5)

    if (split %in% c("target", "factor", "onlyfactor", "ordered", "numeric")) {
      cpo = makeCPO("testorder", .datasplit = split, cpo.trafo = { control = 0 ; names(data)[1] = "XX" ; data },
        cpo.retrafo = { names(data)[1] = "XX" ; data })(affect.index = c(4, 3), affect.name = c("F3", "N1"))
    } else if (split == "no") {
      cpo = makeCPO("testorder", .datasplit = split, cpo.trafo = { control = 0 ; names(data)[1 + identical(target, names(data)[1])] = "XX" ; data },
        cpo.retrafo = { names(data)[1] = "XX" ; data })(affect.index = c(4, 3), affect.name = c("F3", "N1"))

    } else if (split == "task") {
      cpo = makeCPO("testorder", .datasplit = split, cpo.trafo = { control = 0 ; td = getTaskData(data) ;
        names(td)[1 + identical(names(td)[1], target)] = "XX" ; changeData(data, td) },
        cpo.retrafo = { names(data)[1] = "XX" ; data })(affect.index = c(4, 3), affect.name = c("F3", "N1"))
    } else {
      cpo = makeCPO("testorder", .datasplit = split, cpo.trafo = { control = 0 ; names(data$numeric)[1] = "XX" ; data },
        cpo.retrafo = { names(data$numeric)[1] = "XX" ; data })(affect.index = c(4, 3), affect.name = c("F3", "N1"))

    }

    trafod = cpo.df5 %>>% cpo

    exp.names = names(cpo.df5)
    targetidx = switch(split,
      task = 4,
      no = 4,
      target = 4,
      factor = 3,
      onlyfactor = 2,
      numeric = 4,
      ordered = 3,
      most = 4,
      all = 4)
    exp.names[targetidx] = "XX"

    expect_set_equal(names(cpo.df5 %>>% retrafo(trafod)), exp.names)
    expect_set_equal(names(getTaskData(cpo.df5c %>>% retrafo(trafod))), exp.names)
    retrafo(trafod) = NULL
    expect_set_equal(names(trafod), exp.names)

    trafod = cpo.df5c %>>% cpo
    expect_set_equal(names(cpo.df5 %>>% retrafo(trafod)), exp.names)
    expect_set_equal(names(getTaskData(cpo.df5c %>>% retrafo(trafod))), exp.names)
    retrafo(trafod) = NULL
    expect_set_equal(names(getTaskData(trafod)), exp.names)
  }

})

test_that("selected columns get through", {

  for (split in c("task", "no", "target", "most", "all", "factor", "onlyfactor", "ordered", "numeric")) {

    cpo = makeCPO("testpresent", .datasplit = split, cpo.trafo = {
      control = 0
      switch(split,
        task = expect_equal(getTaskData(data), cpo.df5[c("N1", "F1", "O1")]),
        no = expect_equal(data, cpo.df5[c("N1", "F1", "O1")]),
        target = expect_equal(data, cpo.df5[c("N1", "O1")]),
        most = expect_equal(list(data$numeric, data$factor), list(cpo.df5["N1"], cpo.df5["O1"])),
        all = expect_equal(list(data$numeric, data$ordered), list(cpo.df5["N1"], cpo.df5["O1"])),
        factor = expect_equal(data, cpo.df5["O1"]),
        onlyfactor = expect_identical(data, cpo.df5[character(0)], label = "test"),
        ordered = expect_equal(data, cpo.df5["O1"]),
        numeric = expect_equal(data, cpo.df5["N1"]),
        stop("error"))
      data
    }, cpo.retrafo = {
      switch(split,
        task = expect_equal(data, cpo.df5[c("N1", "O1")]),
        no = expect_equal(data, cpo.df5[c("N1", "O1")]),
        target = expect_equal(data, cpo.df5[c("N1", "O1")]),
        most = expect_equal(list(data$numeric, data$factor), list(cpo.df5["N1"], cpo.df5["O1"])),
        all = expect_equal(list(data$numeric, data$ordered), list(cpo.df5["N1"], cpo.df5["O1"])),
        factor = expect_equal(data, cpo.df5["O1"]),
        onlyfactor = expect_identical(data, cpo.df5[character(0)], label = "test"),
        ordered = expect_equal(data, cpo.df5["O1"]),
        numeric = expect_equal(data, cpo.df5["N1"]),
        stop("error"))
      data
    })(affect.pattern = "1$")

    cpo.df5 %>>% retrafo(cpo.df5c %>>% cpo)
  }


})

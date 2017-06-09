

context("cpo")

test_that("data actually has the form requested", {
  # split according to argument


  for (type in c("o", "f")) {
    if (type == "f") next  # TODO

    nosplit = generateCPO(type, "no")
    tasksplit = generateCPO(type, "task")
    targetsplit = generateCPO(type, "target")
    mostsplit = generateCPO(type, "most")
    allsplit = generateCPO(type, "all")

    doallsplit = function(learner, tdata, rdata, numtarget, numnumeric, numfactor, numother, numordered) {
      nr = if (is.data.frame(tdata)) nrow(tdata) else nrow(getTaskData(tdata))

      testsingle = function(cpo) {
        res = tdata %>>% cpo
        rt = retrafo(res)
        retrafo(res) = NULL
        expect_equal(res, tdata)
        expect_equal(rdata %>>% rt, rdata)

        if ("Task" %in% class(tdata)) {
          m = train(cpo %>>% learner, tdata)
          if ("Task" %in% class(rdata)) {
            predict(m, rdata)
          } else {
            predict(m, newdata = rdata)
          }
        }
      }

      testsingle(nosplit(nr, numtarget))
      testsingle(tasksplit(nr, numtarget))
      testsingle(targetsplit(nr, numtarget))
      testsingle(mostsplit(nr, numtarget, numnumeric, numfactor + numordered, numother, -1))
      testsingle(allsplit(nr, numtarget, numnumeric, numfactor, numother, numordered))
      invisible(NULL)
    }

    doallsplittask = function(l, constructor, target, tdata, rdata, numtarget, ...) {
      t1 = if (length(target)) constructor(data = tdata, target = target) else constructor(data = tdata)
      t2 = if (length(target)) constructor(data = rdata, target = target) else constructor(data = rdata)
      d1 = tdata[!names(tdata) %in% target]
      d2 = rdata[!names(rdata) %in% target]
      doallsplit(l, d1, d2, 0, ...)
      doallsplit(l, t1, d2, numtarget, ...)
      doallsplit(l, t1, rdata, numtarget, ...)
      doallsplit(l, t1, t2, numtarget, ...)
      doallsplit(l, d2, t2, 0, ...)
    }

    cl = testlearnercpo
    rl = testregrcpo
    cc = testclustercpo
    ml = testmlcpo


    doallsplit(cl, testdfcpo, testdfcpo3, 0, 1, 0, 0, 0)
    doallsplit(cl, testtaskcpo, testdfcpo3, 1, 1, 0, 0, 0)
    doallsplit(cl, testtaskcpo, testtaskcpo3, 1, 1, 0, 0, 0)
    doallsplit(cl, testdfcpo, testtaskcpo3, 0, 1, 0, 0, 0)


    doallsplittask(cl, makeClassifTask, "F1", cbind(cpo.df.numeric, cpo.df.factorial), cbind(cpo.df.numeric2, cpo.df.factorial2), 1, 3, 1, 0, 0)
    doallsplittask(cl, makeClassifTask, "F1",
      cbind(cpo.df.numeric, cpo.df.factorial, cpo.df.ordered),
      cbind(cpo.df.numeric2, cpo.df.factorial2, cpo.df.ordered), 1, 3, 1, 0, 3)
    doallsplittask(cl, makeClassifTask, "O2",
      cbind(cpo.df.numeric, cpo.df.factorial, cpo.df.ordered),
      cbind(cpo.df.numeric2, cpo.df.factorial2, cpo.df.ordered), 1, 3, 2, 0, 2)
    doallsplittask(cl, makeClassifTask, "O2",
      cbind(cpo.df.numeric, cpo.df.ordered),
      cbind(cpo.df.numeric2, cpo.df.ordered), 1, 3, 0, 0, 2)

    doallsplittask(rl, makeRegrTask, "N2", cbind(cpo.df.numeric, cpo.df.factorial), cbind(cpo.df.numeric2, cpo.df.factorial2), 1, 2, 2, 0, 0)
    doallsplittask(rl, makeRegrTask, "N2",
      cbind(cpo.df.numeric, cpo.df.factorial, cpo.df.ordered),
      cbind(cpo.df.numeric2, cpo.df.factorial2, cpo.df.ordered), 1, 2, 2, 0, 3)
    doallsplittask(rl, makeRegrTask, "N2", cpo.df.numeric, cpo.df.numeric2, 1, 2, 0, 0, 0)

    doallsplittask(cc, makeClusterTask, NULL, cbind(cpo.df.numeric, cpo.df.factorial), cbind(cpo.df.numeric2, cpo.df.factorial2), 0, 3, 2, 0, 0)
    doallsplittask(cc, makeClusterTask, NULL,
      cbind(cpo.df.numeric, cpo.df.factorial, cpo.df.ordered),
      cbind(cpo.df.numeric2, cpo.df.factorial2, cpo.df.ordered), 0, 3, 2, 0, 3)
    doallsplittask(cc, makeClusterTask, NULL, cpo.df.numeric, cpo.df.numeric2, 0, 3, 0, 0, 0)

    doallsplittask(ml, makeMultilabelTask, c("T1", "T2"), cbind(cpo.df.numeric, cpo.df.logical), cbind(cpo.df.numeric2, cpo.df.logical2), 2, 3, 0, 0, 0)
    doallsplittask(ml, makeMultilabelTask, c("T1", "T2"),
      cbind(cpo.df.numeric, cpo.df.logical, cpo.df.factorial, cpo.df.ordered),
      cbind(cpo.df.numeric2, cpo.df.logical2, cpo.df.factorial2, cpo.df.ordered), 2, 3, 2, 0, 3)
  }
})

test_that("changing some columns leaves the others in order", {
  # datasplit: most, all; also target in 'target'
  # introducing columns where there were none before

  df1 = cbind(cpo.df.numeric, cpo.df.factorial)
  df2 = cbind(cpo.df.numeric, cpo.df.other, cpo.df.factorial, cpo.df.ordered)
  df2 = df2[c(1, 4, 8, 12, 2, 5, 9, 11, 3, 6, 7, 10)]

  df3 = cbind(cpo.df.numeric, cpo.df.logical, cpo.df.factorial)
  df3 = df3[, c(1, 4, 6, 2, 3, 5, 7)]

  df4 = cbind(cpo.df.numeric, cpo.df.logical, cpo.df.factorial, cpo.df.ordered)
  df4 = df4[c(1, 4, 8, 2, 5, 9, 3, 6, 7, 10)]

  df5 = cbind(cpo.df.numeric, cpo.df.factorial3, cpo.df.factorial, cpo.df.ordered)
  df5 = df5[c(1, 4, 8, 2, 5, 9, 3, 6, 7)]

  df1c = makeClassifTask(data = df1, target = "F1")
  df1cc = makeClusterTask(data = df1)
  df3l = makeMultilabelTask(data = df3, target = c("T1", "T2"))

  df4l = makeMultilabelTask(data = df4, target = c("T1", "T2"))

  df5c = makeClassifTask(data = df5, target = "F1")
  df5cc = makeClusterTask(data = df5)


  for (type in c("o", "f")) {
    if (type == "f") next  # TODO
    for (split in c("target", "most", "all")) {

      cpo = cpogen("colchangetest", type, pss(test: discrete[none, target, other, all]), function(data, target, test) {
        control = 0
        if (test == "target") {  # just change a column name
          names(data)[2] = "X"
        } else if (test == "other") {
          names(data$other)[2] = "X"
        } else if (test == "all") {
          names(data$numeric)[2] = "X"
          names(data$factor)[1] = "Y"
        }
        data
      }, function(data, control, test) {
        if (test == "target") {  # just change a column name
          names(data)[2] = "X"
        } else if (test == "other") {
          names(data$other)[2] = "X"
        } else if (test == "all") {
          names(data$numeric)[2] = "X"
          names(data$factor)[1] = "Y"
        }
        data
      }, split)

      if (split == "target") {

        tr = df1c %>>% cpo("none")
        expect_equal(getTaskData(tr), df1)
        expect_equal(df1[c(1, 2, 3, 5)] %>>% retrafo(tr), df1[c(1, 2, 3, 5)])
        expect_equal(df1 %>>% retrafo(tr), df1)
        expect_equal(getTaskData(df1c %>>% retrafo(tr)), df1)

        tr = df1c %>>% cpo("target")
        expected = df1[c(1, 2, 3, 5, 4)]
        names(expected)[2] = "X"
        expect_equal(getTaskData(tr), expected)
        expect_equal(df1[c(1, 2, 3, 5)] %>>% retrafo(tr), expected[1:4])
        expect_equal(df1 %>>% retrafo(tr), expected)
        expect_equal(getTaskData(df1c %>>% retrafo(tr)), expected)

        tr = df1cc %>>% cpo("none")
        expect_equal(getTaskData(tr), df1)
        expect_equal(df1 %>>% retrafo(tr), df1)
        expect_equal(getTaskData(df1cc %>>% retrafo(tr)), df1)

        tr = df1cc %>>% cpo("target")
        expected = df1
        names(expected)[2] = "X"
        expect_equal(getTaskData(tr), expected)

        tr = df3l %>>% cpo("none")
        expect_equal(getTaskData(tr), df3)
        expect_equal(df3[c(1, 3, 4, 5, 7)] %>>% retrafo(tr), df3[c(1, 3, 4, 5, 7)])
        expect_equal(df3 %>>% retrafo(tr), df3)
        expect_equal(getTaskData(df3l %>>% retrafo(tr)), df3)

        tr = df3l %>>% cpo("target")
        expected = df3[c(1, 3, 4, 5, 7, 2, 6)]
        names(expected)[2] = "X"
        expect_equal(getTaskData(tr), expected)
        expect_equal(df3[c(1, 3, 4, 5, 7)] %>>% retrafo(tr), expected[1:5])
        expect_equal(df3 %>>% retrafo(tr), expected)
        expect_equal(getTaskData(df3l %>>% retrafo(tr)), expected)

      } else {
        tr = df2 %>>% cpo("other")
        expected = df2[c(1, 3, 4, 5, 7, 8, 9, 12, 2, 6, 10, 11)]
        names(expected)[10] = "X"
        ret = retrafo(tr)
        retrafo(tr) = NULL
        expect_equal(tr, expected)

        tr = df2 %>>% ret
        expect_equal(tr, expected)

        df2c = makeClassifTask(data = df2, target = "F1", check.data = FALSE)
        tr = df2c %>>% cpo("other")
        expected = df2[c(1, 3, 4, 5, 7, 8, 9, 12, 2, 6, 10, 11)]
        names(expected)[10] = "X"
        expect_equal(getTaskData(tr), expected)
        expect_equal(getTaskData(df2c %>>% retrafo(tr)), expected)
        expect_equal(df2 %>>% retrafo(tr), expected)
        expect_equal(df2[c(1:2, 4:12)] %>>% retrafo(tr), expected[c(1, 3:12)])

        if (split == "most") {
          tr = df5c %>>% cpo("all")
          expected = df5[c(5, 1, 4, 7, 2, 3, 6, 8, 9)]
          names(expected)[3] = "X"
          names(expected)[5] = "Y"
          expect_equal(getTaskData(tr), expected)
          expect_equal(df5[c(1:4, 6:9)] %>>% retrafo(tr), expected[2:9])
          expect_equal(df5 %>>% retrafo(tr), expected)
          expect_equal(getTaskData(df5c %>>% retrafo(tr)), expected)

          tr = df5cc %>>% cpo("all")
          expected = df5[c(1, 4, 7, 2, 3, 5, 6, 8, 9)]
          names(expected)[2] = "X"
          names(expected)[4] = "Y"
          expect_equal(getTaskData(tr), expected)
          expect_equal(df5 %>>% retrafo(tr), expected)
          expect_equal(getTaskData(df5cc %>>% retrafo(tr)), expected)

          tr = df4l %>>% cpo("all")
          expected = df4[c(2, 5, 1, 4, 7, 3, 6, 8:10)]
          names(expected)[4] = "X"
          names(expected)[6] = "Y"
          expect_equal(getTaskData(tr), expected)
          expect_equal(df4 %>>% retrafo(tr), expected)
          expect_equal(getTaskData(df4l %>>% retrafo(tr)), expected)
          expect_equal(df4[c(1, 3, 4, 6:10)] %>>% retrafo(tr), expected[3:10])

        } else {
          tr = df5c %>>% cpo("all")
          expected = df5[c(3, 5, 6, 9, 1, 4, 7, 2, 8)]
          names(expected)[6] = "X"
          names(expected)[8] = "Y"
          expect_equal(getTaskData(tr), expected)
          expect_equal(df5[c(1:4, 6:9)] %>>% retrafo(tr), expected[c(1, 3:9)])
          expect_equal(df5 %>>% retrafo(tr), expected)
          expect_equal(getTaskData(df5c %>>% retrafo(tr)), expected)

          tr = df5cc %>>% cpo("all")
          expected = df5[c(3, 6, 9, 1, 4, 7, 2, 5, 8)]
          names(expected)[5] = "X"
          names(expected)[7] = "Y"
          expect_equal(getTaskData(tr), expected)
          expect_equal(df5 %>>% retrafo(tr), expected)
          expect_equal(getTaskData(df5cc %>>% retrafo(tr)), expected)

          tr = df4l %>>% cpo("all")
          expected = df4[c(2, 3, 5, 6, 10, 1, 4, 7, 8, 9)]
          names(expected)[7] = "X"
          names(expected)[9] = "Y"
          expect_equal(getTaskData(tr), expected)
          expect_equal(df4 %>>% retrafo(tr), expected)
          expect_equal(getTaskData(df4l %>>% retrafo(tr)), expected)
          expect_equal(df4[c(1, 3, 4, 6:10)] %>>% retrafo(tr), expected[c(2, 4:10)])
        }
      }
    }
  }
})

test_that("changing the target gives an error", {
  # datasplit: task, no

})

test_that("introducing duplicate names gives an error", {
  # datasplit: target, most, all


})

test_that("new task is actually changed, has the expected data", {
  # all datasplits

})

test_that("training / predicting with different data types works as expected", {
  # target column is removed on prediction

})

test_that("cpo framework detects bad data", {
  # in splits: one of the returned things is not a df
  # missing / too many names in split
  # row number mismatch
})

test_that("attaching cpo with mismatching properties gives error", {
  # check along multiple levels
  # even after detaching from learner

})

test_that("returning properties that are not allowed is detected", {

})




# TEST: one target is missing (even with DFs)

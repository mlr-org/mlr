context("cpo data split")

test_that("data actually has the form requested", {
  # split according to argument
  for (type in c("o", "f")) {

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

      testsingle(nosplit(nr, numtarget, numnumeric, numfactor, numother, numordered, affect.pattern = "^OUT\\.", affect.invert = TRUE))
      testsingle(tasksplit(nr, numtarget, numnumeric, numfactor, numother, numordered, affect.pattern = "^OUT\\.", affect.invert = TRUE))
      testsingle(targetsplit(nr, numtarget, numnumeric, numfactor, numother, numordered, affect.pattern = "^OUT\\.", affect.invert = TRUE))
      testsingle(mostsplit(nr, numtarget, numnumeric, numfactor + numordered, numother, -1, affect.pattern = "^OUT\\.", affect.invert = TRUE))
      testsingle(allsplit(nr, numtarget, numnumeric, numfactor, numother, numordered, affect.pattern = "^OUT\\.", affect.invert = TRUE))
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


    doallsplittask(cl, makeClassifTask, "F1", cbind(cpo.df.numeric, OUT = cpo.df.ordered, cpo.df.factorial),
                   cbind(OUT = cpo.df.ordered, cpo.df.numeric2, cpo.df.factorial2), 1, 3, 1, 0, 0)
    doallsplittask(cl, makeClassifTask, "F1",
      cbind(cpo.df.numeric, cpo.df.factorial, cpo.df.ordered, OUT = cpo.df.ordered),
      cbind(cpo.df.numeric2, cpo.df.factorial2, OUT = cpo.df.ordered, cpo.df.ordered), 1, 3, 1, 0, 3)
    doallsplittask(cl, makeClassifTask, "O2",
      cbind(OUT = cpo.df.ordered, cpo.df.numeric, cpo.df.factorial, cpo.df.ordered),
      cbind(cpo.df.numeric2, OUT = cpo.df.ordered, cpo.df.factorial2, cpo.df.ordered), 1, 3, 2, 0, 2)
    doallsplittask(cl, makeClassifTask, "O2",
      cbind(cpo.df.numeric, cpo.df.ordered, OUT = cpo.df.ordered),
      cbind(OUT = cpo.df.numeric, cpo.df.numeric2, cpo.df.ordered, OUT = cpo.df.ordered), 1, 3, 0, 0, 2)

    doallsplittask(rl, makeRegrTask, "N2", cbind(cpo.df.numeric, cpo.df.factorial, OUT = cpo.df.ordered), cbind(cpo.df.numeric2, cpo.df.factorial2), 1, 2, 2, 0, 0)
    doallsplittask(rl, makeRegrTask, "N2",
      cbind(cpo.df.numeric, cpo.df.factorial, OUT = cpo.df.ordered, cpo.df.ordered),
      cbind(cpo.df.numeric2, cpo.df.factorial2, cpo.df.ordered), 1, 2, 2, 0, 3)
    doallsplittask(rl, makeRegrTask, "N2", cpo.df.numeric, cpo.df.numeric2, 1, 2, 0, 0, 0)

    doallsplittask(cc, makeClusterTask, NULL, cbind(cpo.df.numeric, cpo.df.factorial, OUT = cpo.df.ordered, OUT = cpo.df.numeric),
                   cbind(cpo.df.numeric2, cpo.df.factorial2), 0, 3, 2, 0, 0)
    doallsplittask(cc, makeClusterTask, NULL,
      cbind(cpo.df.numeric, cpo.df.factorial, cpo.df.ordered),
      cbind(cpo.df.numeric2, OUT = cpo.df.ordered, cpo.df.factorial2, cpo.df.ordered), 0, 3, 2, 0, 3)
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

  for (type in c("o", "f")) {
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

        tr = cpo.df1c %>>% cpo("none")
        expect_equal(getTaskData(tr), cpo.df1)
        expect_equal(cpo.df1[c(1, 2, 3, 5)] %>>% retrafo(tr), cpo.df1[c(1, 2, 3, 5)])
        expect_equal(cpo.df1 %>>% retrafo(tr), cpo.df1)
        expect_equal(getTaskData(cpo.df1c %>>% retrafo(tr)), cpo.df1)

        tr = cpo.df1c %>>% cpo("target")
        expected = cpo.df1[c(1, 2, 3, 5, 4)]
        names(expected)[2] = "X"
        expect_equal(getTaskData(tr), expected)
        expect_equal(cpo.df1[c(1, 2, 3, 5)] %>>% retrafo(tr), expected[1:4])
        expect_equal(cpo.df1 %>>% retrafo(tr), expected)
        expect_equal(getTaskData(cpo.df1c %>>% retrafo(tr)), expected)

        tr = cpo.df1cc %>>% cpo("none")
        expect_equal(getTaskData(tr), cpo.df1)
        expect_equal(cpo.df1 %>>% retrafo(tr), cpo.df1)
        expect_equal(getTaskData(cpo.df1cc %>>% retrafo(tr)), cpo.df1)

        tr = cpo.df1cc %>>% cpo("target")
        expected = cpo.df1
        names(expected)[2] = "X"
        expect_equal(getTaskData(tr), expected)

        tr = cpo.df3l %>>% cpo("none")
        expect_equal(getTaskData(tr), cpo.df3)
        expect_equal(cpo.df3[c(1, 3, 4, 5, 7)] %>>% retrafo(tr), cpo.df3[c(1, 3, 4, 5, 7)])
        expect_equal(cpo.df3 %>>% retrafo(tr), cpo.df3)
        expect_equal(getTaskData(cpo.df3l %>>% retrafo(tr)), cpo.df3)

        tr = cpo.df3l %>>% cpo("target")
        expected = cpo.df3[c(1, 3, 4, 5, 7, 2, 6)]
        names(expected)[2] = "X"
        expect_equal(getTaskData(tr), expected)
        expect_equal(cpo.df3[c(1, 3, 4, 5, 7)] %>>% retrafo(tr), expected[1:5])
        expect_equal(cpo.df3 %>>% retrafo(tr), expected)
        expect_equal(getTaskData(cpo.df3l %>>% retrafo(tr)), expected)

      } else {
        tr = cpo.df2 %>>% cpo("other")
        expected = cpo.df2[c(1, 3, 4, 5, 7, 8, 9, 12, 2, 6, 10, 11)]
        names(expected)[10] = "X"
        ret = retrafo(tr)
        retrafo(tr) = NULL
        expect_equal(tr, expected)

        tr = cpo.df2 %>>% ret
        expect_equal(tr, expected)

        cpo.df2c = makeClassifTask(data = cpo.df2, target = "F1", check.data = FALSE)
        tr = cpo.df2c %>>% cpo("other")
        expected = cpo.df2[c(1, 3, 4, 5, 7, 8, 9, 12, 2, 6, 10, 11)]
        names(expected)[10] = "X"
        expect_equal(getTaskData(tr), expected)
        expect_equal(getTaskData(cpo.df2c %>>% retrafo(tr)), expected)
        expect_equal(cpo.df2 %>>% retrafo(tr), expected)
        expect_equal(cpo.df2[c(1:2, 4:12)] %>>% retrafo(tr), expected[c(1, 3:12)])

        if (split == "most") {
          tr = cpo.df5c %>>% cpo("all")
          expected = cpo.df5[c(5, 1, 4, 7, 2, 3, 6, 8, 9)]
          names(expected)[3] = "X"
          names(expected)[5] = "Y"
          expect_equal(getTaskData(tr), expected)
          expect_equal(cpo.df5[c(1:4, 6:9)] %>>% retrafo(tr), expected[2:9])
          expect_equal(cpo.df5 %>>% retrafo(tr), expected)
          expect_equal(getTaskData(cpo.df5c %>>% retrafo(tr)), expected)

          tr = cpo.df5cc %>>% cpo("all")
          expected = cpo.df5[c(1, 4, 7, 2, 3, 5, 6, 8, 9)]
          names(expected)[2] = "X"
          names(expected)[4] = "Y"
          expect_equal(getTaskData(tr), expected)
          expect_equal(cpo.df5 %>>% retrafo(tr), expected)
          expect_equal(getTaskData(cpo.df5cc %>>% retrafo(tr)), expected)

          tr = cpo.df4l %>>% cpo("all")
          expected = cpo.df4[c(2, 5, 1, 4, 7, 3, 6, 8:10)]
          names(expected)[4] = "X"
          names(expected)[6] = "Y"
          expect_equal(getTaskData(tr), expected)
          expect_equal(cpo.df4 %>>% retrafo(tr), expected)
          expect_equal(getTaskData(cpo.df4l %>>% retrafo(tr)), expected)
          expect_equal(cpo.df4[c(1, 3, 4, 6:10)] %>>% retrafo(tr), expected[3:10])

        } else {
          tr = cpo.df5c %>>% cpo("all")
          expected = cpo.df5[c(3, 5, 6, 9, 1, 4, 7, 2, 8)]
          names(expected)[6] = "X"
          names(expected)[8] = "Y"
          expect_equal(getTaskData(tr), expected)
          expect_equal(cpo.df5[c(1:4, 6:9)] %>>% retrafo(tr), expected[c(1, 3:9)])
          expect_equal(cpo.df5 %>>% retrafo(tr), expected)
          expect_equal(getTaskData(cpo.df5c %>>% retrafo(tr)), expected)

          tr = cpo.df5cc %>>% cpo("all")
          expected = cpo.df5[c(3, 6, 9, 1, 4, 7, 2, 5, 8)]
          names(expected)[5] = "X"
          names(expected)[7] = "Y"
          expect_equal(getTaskData(tr), expected)
          expect_equal(cpo.df5 %>>% retrafo(tr), expected)
          expect_equal(getTaskData(cpo.df5cc %>>% retrafo(tr)), expected)

          tr = cpo.df4l %>>% cpo("all")
          expected = cpo.df4[c(2, 3, 5, 6, 10, 1, 4, 7, 8, 9)]
          names(expected)[7] = "X"
          names(expected)[9] = "Y"
          expect_equal(getTaskData(tr), expected)
          expect_equal(cpo.df4 %>>% retrafo(tr), expected)
          expect_equal(getTaskData(cpo.df4l %>>% retrafo(tr)), expected)
          expect_equal(cpo.df4[c(1, 3, 4, 6:10)] %>>% retrafo(tr), expected[c(2, 4:10)])
        }
      }
    }
  }
})

test_that("changing the target gives an error", {

  for (type in c("o", "f")) {
    for (split in c("no", "task")) {

      changedata = function(data, target, test, chgname) {
        if (test == "df") {
          if (chgname == "yes") {
            targetidx = which(names(data) %in% target)[1]
            names(data)[targetidx] = "X"
          } else if (chgname == "no") {
            data[[target[1]]] = data[[target[1]]][1]
          } else {
            assert(length(target) > 1)
            data[target[1]] = NULL
          }
        } else if (test == "task") {
          constructor = switch(substr(class(data)[1], 1, 3),
            Cla = makeClassifTask,
            Mul = makeMultilabelTask)
          data = getTaskData(data)
          if (chgname == "yes") {
            targetidx = which(names(data) %in% target)[1]
            names(data)[targetidx] = "X"
            target[1] = "X"
          } else if (chgname == "no") {
            data[[target[1]]] = data[[target[1]]][1]
          } else {
            assert(length(target) > 1)
            data[target[1]] = NULL
            target = target[-1]
          }
          data = constructor(data = data, target = target, fixup.data = "no", check.data = FALSE)
        }
        data
      }

      cpo = cpogen("targetchangetest", type, pss(test: discrete[none, df, task],
        chgname: discrete [no, yes, remove] [[requires = quote(test != "none")]]), function(data, target, test, chgname) {
          control = 0
          changedata(data, target, test, chgname)
        }, function(data, control, test, chgname) {
          data
        }, split)

      expect_class(cpo.df1c %>>% cpo("none"), "Task")
      expect_class(cpo.df1 %>>% cpo("none"), "data.frame")

      failmode = switch(split, no = "df", task = "task")
      if (split == "no") {
        expect_class(cpo.df1 %>>% cpo(failmode, "yes"), "data.frame")
      }
      if (split == "no") {
        expect_error(cpo.df1c %>>% cpo(failmode, "yes"), "did not contain target column")
        expect_error(cpo.df3l %>>% cpo(failmode, "yes"), "did not contain target column")
      } else {
        expect_error(cpo.df1c %>>% cpo(failmode, "yes"), "must not change target column names")
        expect_error(cpo.df3l %>>% cpo(failmode, "yes"), "must not change target column names")
      }
      expect_error(cpo.df1c %>>% cpo(failmode, "no"), "must not change target columns, but changed F1")
      expect_error(cpo.df3l %>>% cpo(failmode, "no"), "must not change target columns, but changed T1")

      if (split == "no") {
        expect_error(cpo.df3l %>>% cpo(failmode, "remove"), "did not contain target column T1")
      } else {
        expect_error(cpo.df4l2 %>>% cpo(failmode, "remove"), "must not change target column")
      }
    }
  }
})

test_that("introducing duplicate names gives an error", {
  # datasplit: target, most, all

  for (type in c("o", "f")) {
    for (split in c("target", "most", "all")) {

      cpo = cpogen("duplicatetest", type, pss(test: discrete [none, nosplit, split]), function(data, target, test) {
          control = 0
          if (test == "nosplit") {
            names(data)[1] = names(target)[1]
          } else if (test == "split") {
            names(data[[1]])[1] = names(data[[2]])[1]
          }
          data
        }, function(data, control, test) {
          data
        }, split)

      expect_class(cpo.df1c %>>% cpo("none"), "Task")
      expect_class(cpo.df1 %>>% cpo("none"), "data.frame")

      failmode = switch(split, target = "nosplit", "split")
      expect_error(cpo.df1c %>>% cpo(failmode), switch(failmode, nosplit = "column names F1 duplicated", split = "duplicate column names F2"))
      expect_error(cpo.df3l %>>% cpo(failmode), switch(failmode, nosplit = "column names T1 duplicated", split = "duplicate column names F1"))

    }
  }
})

test_that("new task is actually changed, has the expected data", {
  # all datasplits

  for (type in c("o", "f")) {
    for (split in c("no", "task", "target", "most", "all")) {

      chgfct = function(data, ...) {
        control = 0
        val = (("control" %in% names(list(...))) + 1) * 100
        if (val == 200 && split == "task") {
          split = "no"
        }
        switch(split,
          task = {
            tdata = getTaskData(data)
            numcol = which(sapply(tdata, is.numeric))[1]
            tdata[1, numcol] = val
            changeData(data, tdata)
          },
          no = {
            numcol = which(sapply(data, is.numeric))[1]
            data[1, numcol] = val
            data
          },
          target = {
            numcol = which(sapply(data, is.numeric))[1]
            data[1, numcol] = val
            data
          },
          {
            data$numeric[1, 1] = val
            data
          })
      }

      cpo = cpogen("changetest", type, pss(), chgfct, chgfct, split)

      exp.df1.t = cpo.df1
      exp.df1.r = cpo.df1
      exp.df1.t[1, 1] = 100
      exp.df1.r[1, 1] = 200

      val = cpo.df1 %>>% cpo()
      val2 = cpo.df1 %>>% retrafo(val)
      retrafo(val) = NULL
      expect_equal(val, exp.df1.t)
      expect_equal(val2, exp.df1.r)

      val = cpo.df1c %>>% cpo()
      val2 = cpo.df1c %>>% retrafo(val)
      expect_equal(getTaskData(val), exp.df1.t)
      expect_equal(getTaskData(val2), exp.df1.r)
    }
  }
})

test_that("cpo framework detects bad data", {
  # in splits: one of the returned things is not a df
  # missing / too many names in split
  # row number mismatch
split = "task"
  for (type in c("o", "f")) {
    for (split in c("no", "task", "target")) {

      chgfct = function(data, pred, ...) {
        control = 0
        ispredicting = "control" %in% names(list(...))
        if (split == "task" && !ispredicting) {
          task = data
          data = getTaskData(task)
        }
        if (pred == ispredicting) {
          data = data[-1, ]
        }
        if (split == "task" && !ispredicting) {
          data = changeData(task, data)
        }
        data
      }
      cpo = cpogen("numrowtest", type, pss(pred = FALSE: logical), chgfct, chgfct, split)

      expect_error(cpo.df1 %>>% cpo(), "must not change number of rows")
      expect_error(cpo.df1c %>>% cpo(), "must not change number of rows")

      rr = retrafo(cpo.df1 %>>% cpo(TRUE))
      expect_error(cpo.df1 %>>% rr, "must not change number of rows")

      rr = retrafo(cpo.df1c %>>% cpo(TRUE))
      expect_error(cpo.df1 %>>% rr, "must not change number of rows")
      expect_error(cpo.df1c %>>% rr, "must not change number of rows")
    }

    for (split in c("most", "all")) {
      # 'one of the returned things is not a df'
      chgfct = function(data, pred, ...) {
        control = 0
        ispredicting = "control" %in% names(list(...))
        if (pred == ispredicting) {
          data$numeric = data$numeric[[1]]
        }
        data
      }
      cpo = cpogen("baddatatest", type, pss(pred = FALSE: logical), chgfct, chgfct, split)

      expect_error(cpo.df1 %>>% cpo(), "numeric is not a data.frame")
      expect_error(cpo.df1c %>>% cpo(), "numeric is not a data.frame")

      rr = retrafo(cpo.df1 %>>% cpo(TRUE))
      expect_error(cpo.df1 %>>% rr, "numeric is not a data.frame")

      rr = retrafo(cpo.df1c %>>% cpo(TRUE))
      expect_error(cpo.df1 %>>% rr, "numeric is not a data.frame")
      expect_error(cpo.df1c %>>% rr, "numeric is not a data.frame")

      chgfct = function(data, pred, ...) {
        control = 0
        ispredicting = "control" %in% names(list(...))
        if (pred == ispredicting) {
          data$numeric = data$numeric[-1, ]
        }
        data
      }
      cpo = cpogen("rowchangetest", type, pss(pred = FALSE: logical), chgfct, chgfct, split)

      expect_error(cpo.df1 %>>% cpo(), "rows of numeric data.*must not change row number")
      expect_error(cpo.df1c %>>% cpo(), "rows of numeric data.*must not change row number")

      rr = retrafo(cpo.df1 %>>% cpo(TRUE))
      expect_error(cpo.df1 %>>% rr, "rows of numeric data.*must not change row number")

      rr = retrafo(cpo.df1c %>>% cpo(TRUE))
      expect_error(cpo.df1 %>>% rr, "rows of numeric data.*must not change row number")
      expect_error(cpo.df1c %>>% rr, "rows of numeric data.*must not change row number")


      chgfct = function(data, pred, ...) {
        control = 0
        ispredicting = "control" %in% names(list(...))
        if (pred == ispredicting || ispredicting) {
          data$ordered = data$numeric
          names(data$ordered) = paste(names(data$ordered), "x", sep = ".")
        }
        data
      }
      cpo = cpogen("badsplittest", type, pss(pred = FALSE: logical), chgfct, chgfct, split)

      if (split == "most") {
        expect_error(cpo.df1 %>>% cpo(), '"numeric", "factor", "other"')
        expect_error(cpo.df1c %>>% cpo(), '"numeric", "factor", "other"')

        rr = retrafo(cpo.df1 %>>% cpo(TRUE))
        expect_error(cpo.df1 %>>% rr, '"numeric", "factor", "other"')

        rr = retrafo(cpo.df1c %>>% cpo(TRUE))
        expect_error(cpo.df1 %>>% rr, '"numeric", "factor", "other"')
        expect_error(cpo.df1c %>>% rr, '"numeric", "factor", "other"')
      } else {
        expected = cpo.df1[c(1, 2, 3, 4, 5, 1, 2, 3)]
        names(expected)[6:8] = paste(names(expected)[1:3], "x", sep = ".")
        rr = cpo.df1 %>>% cpo()
        rt = retrafo(rr)
        retrafo(rr) = NULL
        expect_equal(rr, expected)
        expect_equal(cpo.df1 %>>% rt, expected)

        rr = cpo.df1 %>>% cpo(TRUE)
        rt = retrafo(rr)
        retrafo(rr) = NULL
        expect_equal(rr, cpo.df1)
        expect_error(cpo.df1 %>>% rt, "column name mismatch")

        rr = cpo.df1c %>>% cpo()
        expect_equal(getTaskData(rr), expected)
        rt = retrafo(rr)
        expect_equal(cpo.df1 %>>% rt, expected)
        expect_equal(getTaskData(cpo.df1c %>>% rt), expected)
      }
    }
  }
})

test_that("format change between trafo and retrafo are detected", {

  for (type in c("o", "f")) {

    for (split in c("no", "task", "target", "most", "all")) {

      cpo = cpogen("formattest", type, pss(),
        function(data, ...) { control = 0 ; data }, function(data, ...) data, split)

      rt = retrafo(cpo.df1 %>>% cpo())

      expect_equal(cpo.df1 %>>% rt, cpo.df1)

      expect_error(cpo.df1[-1] %>>% rt, "column name mismatch")

      xmp = cpo.df1
      xmp[[1]] = as.factor(xmp[[1]])
      expect_error(xmp %>>% rt, "Types? of column N1 mismatches")

      expect_error(cpo.df1c %>>% rt, "column name mismatch")

      rt = retrafo(cpo.df1c %>>% cpo())

      expect_equal(cpo.df1 %>>% rt, cpo.df1)

      expect_error(cpo.df1[-1] %>>% rt, "column name mismatch")

      xmp = cpo.df1
      xmp[[1]] = as.factor(xmp[[1]])
      expect_error(xmp %>>% rt, "Type of column N1 mismatches")

      expect_class(cpo.df1c %>>% rt, "Task")

      xmp = cpo.df1
      xmp[[5]] = as.ordered(xmp[[5]])
      if (split == "all") {
        expect_error(xmp %>>% rt, "Type of column F2 mismatches")
      } else {
        expect_class(xmp %>>% rt, "data.frame")
      }

      rt = retrafo(cpo.df1 %>>% cpo())
      expect_error(cpo.df1c %>>% rt, "column name mismatch")
      expect_class(cpo.df1cc %>>% rt, "Task")

      rt = retrafo(cpo.df1c %>>% cpo())
      expect_class(cpo.df1 %>>% rt, "data.frame")
      expect_error(cpo.df1cc %>>% rt, "column name mismatch")

    }
split = "no"
type = "o"
    for (split in c("no", "task", "target")) {

      cpo = cpogen("formattest", type, pss(),
        function(data, ...) { control = 0 ; data }, function(data, ...) { data[-1] }, split)

      rt = retrafo(cpo.df1 %>>% cpo())
      expect_error(cpo.df1 %>>% rt, "column name mismatch between training and test data")
      expect_error(cpo.df1cc %>>% rt, "column name mismatch between training and test data")

      rt = retrafo(cpo.df1c %>>% cpo())
      expect_error(cpo.df1 %>>% rt, "column name mismatch between training and test data")
      expect_error(cpo.df1c %>>% rt, "column name mismatch between training and test data")

      cpo = cpogen("formattest", type, pss(),
        function(data, ...) { control = 0 ; data }, function(data, ...) { cbind(data[c(2, 1)], data[-(1:2)]) }, split)

      rt = retrafo(cpo.df1 %>>% cpo())
      expect_error(cpo.df1 %>>% rt, "column name mismatch between training and test data")
      expect_error(cpo.df1cc %>>% rt, "column name mismatch between training and test data")

      rt = retrafo(cpo.df1c %>>% cpo())
      expect_error(cpo.df1 %>>% rt, "column name mismatch between training and test data")
      expect_error(cpo.df1c %>>% rt, "column name mismatch between training and test data")

      cpo = cpogen("formattest", type, pss(),
        function(data, ...) { control = 0 ; data }, function(data, ...) { data[[1]] = as.factor(data[[1]]) ; data },
        split, properties.needed = "factors")

      rt = retrafo(cpo.df1 %>>% cpo())
      expect_error(cpo.df1 %>>% rt, "Type of column N1 mismatches")
      expect_error(cpo.df1cc %>>% rt, "Type of column N1 mismatches")

      rt = retrafo(cpo.df1c %>>% cpo())
      expect_error(cpo.df1 %>>% rt, "Type of column N1 mismatches")
      expect_error(cpo.df1c %>>% rt, "Type of column N1 mismatches")
    }

    for (split in c("most", "all")) {
      cpo = cpogen("numrowtest", type, pss(),
        function(data, ...) { control = 0 ; data }, function(data, ...) { data$numeric = data$numeric[-1] ; data }, split)

      rt = retrafo(cpo.df1 %>>% cpo())
      expect_error(cpo.df1 %>>% rt, "column name mismatch between training and test data")
      expect_error(cpo.df1cc %>>% rt, "column name mismatch between training and test data")

      rt = retrafo(cpo.df1c %>>% cpo())
      expect_error(cpo.df1 %>>% rt, "column name mismatch between training and test data")
      expect_error(cpo.df1c %>>% rt, "column name mismatch between training and test data")

      cpo = cpogen("numrowtest", type, pss(),
        function(data, ...) { control = 0 ; data }, function(data, ...) { data$numeric = cbind(data$numeric[c(2, 1)],data$numeric[-c(1, 2)]) ; data }, split)

      rt = retrafo(cpo.df1 %>>% cpo())
      expect_error(cpo.df1 %>>% rt, "column name mismatch between training and test data")
      expect_error(cpo.df1cc %>>% rt, "column name mismatch between training and test data")

      rt = retrafo(cpo.df1c %>>% cpo())
      expect_error(cpo.df1 %>>% rt, "column name mismatch between training and test data")
      expect_error(cpo.df1c %>>% rt, "column name mismatch between training and test data")

      cpo = cpogen("numrowtest", type, pss(),
        function(data, ...) { control = 0 ; data }, function(data, ...) { data$numeric[[1]] = as.factor(data$numeric[[1]]) ; data },
        split, properties.needed = "factors")
      rt = retrafo(cpo.df1 %>>% cpo())
      expect_error(cpo.df1 %>>% rt, "Type of column N1 mismatches")
      expect_error(cpo.df1cc %>>% rt, "Type of column N1 mismatches")

      rt = retrafo(cpo.df1c %>>% cpo())
      expect_error(cpo.df1 %>>% rt, "Type of column N1 mismatches")
      expect_error(cpo.df1c %>>% rt, "Type of column N1 mismatches")


      cpo = cpogen("numrowtest", type, pss(),
        function(data, ...) { control = 0 ; data }, function(data, ...) { data$factor[[1]] = as.ordered(data$factor[[1]]) ; data },
        split, properties.needed = "ordered")

      if (split == "all") {
        rt = retrafo(cpo.df1 %>>% cpo())
        expect_error(cpo.df1 %>>% rt, "Type of column F1 mismatches")
        expect_error(cpo.df1cc %>>% rt, "Type of column F1 mismatches")

        rt = retrafo(cpo.df1c %>>% cpo())
        expect_error(cpo.df1 %>>% rt, "Type of column F2 mismatches")
        expect_error(cpo.df1c %>>% rt, "Type of column F2 mismatches")
      } else {
        rt = retrafo(cpo.df1 %>>% cpo())
        expect_class(cpo.df1 %>>% rt, "data.frame")
        expect_class(cpo.df1cc %>>% rt, "Task")

        rt = retrafo(cpo.df1c %>>% cpo())
        expect_class(cpo.df1 %>>% rt, "data.frame")
        expect_class(cpo.df1c %>>% rt, "Task")
      }


    }
  }
})

test_that("factor fixing works", {

  factormemcpo = makeCPO("dummyencode", .datasplit = "target", .fix.factors = FALSE,
    cpo.trafo = {
      control = lapply(data, levels)
      data
    }, cpo.retrafo = {
      newlevels = lapply(data, levels)
      levelsfit = mapply(identical, control, newlevels)
      data[[1]] = sum(levelsfit)
      data
    })()

  hi = head(iris)
  hi2 = hi
  hi2$Species = factor(as.character(hi2$Species), levels = c("setosa", "versicolor"))
  hi3 = hi2
  hi3$Species = factor(as.character(hi3$Species), levels = c("versicolor", "setosa"))

  expect_equal((hi %>>% retrafo(hi %>>% factormemcpo))[1, 1], 5)
  expect_equal((hi2 %>>% retrafo(hi %>>% factormemcpo))[1, 1], 4)
  expect_equal((hi3 %>>% retrafo(hi %>>% factormemcpo))[1, 1], 4)
  expect_equal((hi %>>% retrafo(hi2 %>>% factormemcpo))[1, 1], 4)
  expect_equal((hi2 %>>% retrafo(hi2 %>>% factormemcpo))[1, 1], 5)
  expect_equal((hi3 %>>% retrafo(hi2 %>>% factormemcpo))[1, 1], 4)
  expect_equal((iris %>>% retrafo(hi2 %>>% factormemcpo))[1, 1], 4)

  expect_set_equal(names(table((iris %>>% retrafo(hi2 %>>% factormemcpo))$Species)), c("setosa", "versicolor", "virginica"))

  factormemcpo$fix.factors = TRUE

  expect_equal((hi %>>% retrafo(hi %>>% factormemcpo))[1, 1], 5)
  expect_equal((hi2 %>>% retrafo(hi %>>% factormemcpo))[1, 1], 5)
  expect_equal((hi3 %>>% retrafo(hi %>>% factormemcpo))[1, 1], 5)
  expect_equal((hi %>>% retrafo(hi2 %>>% factormemcpo))[1, 1], 5)
  expect_equal((hi2 %>>% retrafo(hi2 %>>% factormemcpo))[1, 1], 5)
  expect_equal((hi3 %>>% retrafo(hi2 %>>% factormemcpo))[1, 1], 5)
  expect_equal((iris %>>% retrafo(hi2 %>>% factormemcpo))[1, 1], 5)

  expect_set_equal(names(table((iris %>>% retrafo(hi2 %>>% factormemcpo))$Species)), c("setosa", "versicolor"))
  expect_equal(length((iris %>>% retrafo(hi2 %>>% factormemcpo))$Species), nrow(iris))
  expect_equal(is.na((iris %>>% retrafo(hi2 %>>% factormemcpo))$Species), iris$Species == "virginica")

})



context("filterFeatures_praznik")
  test_that("filterFeatures_praznik", {
    a = c(1, 2, 5.3, 6, -2, 4, 8.3, 9.2, 10.1)  # numeric vector
    b = c("one", "two", "three")  # character vector
    c = c(TRUE, TRUE, TRUE, FALSE, TRUE, FALSE)  # logical vector
    d = c(1L, 3L, 5L, 7L, 9L, 17L)
    f = rep(c("c1", "c2"),9)
    df = data.frame(a = a, b = b, c = c, d = d, f = f)
    df = convertDataFrameCols(df, logicals.as.factor = TRUE)
    lapply(df,class)
    task = makeClassifTask(data = df, target = "f")
    generateFilterValuesData(task, method = "praznik", criteria = "MIM", nselect = 2L)
    generateFilterValuesData(task, method = "praznik", criteria = "MIM", nselect = 1L)
    expect_true(TRUE)
})


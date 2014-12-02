context("resample: subsampling")

test_that("subsampling instance works", {
  rin = makeResampleInstance(makeResampleDesc("Subsample", iters=2, split=0.25), size=20)
	iters <- rin$desc$iters
  expect_equal(iters, 2)
	
	for (i in 1:iters) {
    i1 = rin$train.inds[[i]]
    i2 = rin$test.inds[[i]]
    expect_equal(length(i1), 5)
    expect_equal(length(i2), 15)
    expect_true(min(i1) >= 1)
		expect_true(max(i1) <= 20)
    expect_true(min(i2) >= 1)
    expect_true(max(i2) <= 20)
		expect_equal(sort(c(i1, i2)), 1:20)
	}
})

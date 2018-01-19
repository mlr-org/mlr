For bug reports, please respect the following guidelines and check the boxes accordingly.
Please include your source code using tripple backticks ` like this:


```r
# your code
```


For questions of the type: "How can this be done?", please consider posting them on Stackoverflow.

For everything else ignore this template.

## Bug report

- [ ] Start a new R session
- [ ] Install the latest version of mlr: `update.packages(oldPkgs="mlr", ask=FALSE)` or if you use a GitHub install of mlr: `devtools::install_github(c("BBmisc", "ParamHelpers", "mlr"))`
- [ ] run `sessionInfo()`
- [ ] Give a minimal reproducible example

## Writing a good bug report

[ Slightly adapted version of (this) [https://www.r-project.org/bugs.html#writing-a-good-bug-report] ]

Bug reports should include a way of reproducing the bug. This should be as simple as possible. If the person trying to fix the bug can’t work out how to make it appear, or has to jump through a lot of unnecessary hoops to make it appear, you’re going to waste a lot of their time.

mlr is maintained by a number of people, so it’s best to make sure your bug report is clear and well-written. If it’s not, it will suck in more energy from the maintainers and take longer for the bug to get fixed - or it may end up not getting handled at all. In particular, you should:

Write a clear and unique summary for the bug. “Running a model on data set with a constant feature causes the following exception” is good; “software crashes” is not.
Include, in the description, the steps to reproduce the bug mentioned above.
Focus most on the facts of what happened, but if course helps if you can already give us (informed!) guesses where the bug comes from.

## Minimal reproducible example

A minimal reproducible example consists of the following items:

a minimal dataset, necessary to reproduce the error
the minimal runnable code necessary to reproduce the error, which can be run on the given dataset.
the necessary information on the used packages, R version and system it is run on.
in the case of random processes, a seed (set by set.seed()) for reproducibility

If you want to learn more, read
https://stackoverflow.com/questions/5963269/how-to-make-a-great-r-reproducible-example]

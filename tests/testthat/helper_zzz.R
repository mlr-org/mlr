set.seed(123)
options(mlr.debug.seed = 123L)
options(datatable.rbindlist.check = "error")
configureMlr(show.info = FALSE, show.learner.output = FALSE)

library(checkmate)

options(mlr.debug.seed=123L)
configureMlr(show.learner.output=FALSE)
# keep travis alive?
if (Sys.getenv("TRAVIS") == "true")
  cat("\n### Testing next file\n", file = stdout())

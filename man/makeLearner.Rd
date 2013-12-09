\name{makeLearner}
\alias{Learner}
\alias{makeLearner}
\title{Create learner object.}
\usage{
makeLearner(cl, id = cl, predict.type = "response", ...,
  par.vals = list())
}
\arguments{
  \item{cl}{[\code{character(1)}]\cr Class of learner to
  create. By convention, all classification learners start
  with \dQuote{classif.} and all regression learners with
  \dQuote{regr.}. A list of all learners is available on
  the \code{\link{learners}} help page.}

  \item{id}{[\code{character(1)}]\cr Id string for object.
  Used to display object.  Default is \code{cl}.}

  \item{predict.type}{[\code{character(1)}]\cr
  Classification: \dQuote{response} (= labels) or
  \dQuote{prob} (= probabilities and labels by selecting
  the ones with maximal probability).  Regression:
  \dQuote{response} (= mean response) or \dQuote{se} (=
  standard errors and mean response).  Default is
  \dQuote{response}.}

  \item{...}{[any]\cr Optional named (hyper)parameters.
  Alternatively these can be given using the
  \code{par.vals} argument.}

  \item{par.vals}{[\code{list}]\cr Optional list of named
  (hyper)parameters. The arguments in \code{...} take
  precedence over values in this list. We strongly
  encourage you to use one or the other to pass
  (hyper)parameters to the learner but not both.}
}
\value{
[\code{\link{Learner}}].
}
\description{
For a classification learner the \code{predict.type} can be
set to \dQuote{prob} to predict probabilities and the
maximum value selects the label. The threshold used to
assign the label can later be changed using the
\code{\link{setThreshold}} function.
}
\examples{
makeLearner("classif.rpart")
makeLearner("classif.lda", predict.type = "prob")

lrn <- makeLearner("classif.rpart", minsplit = 5)
print(lrn$par.vals)
lrn <- makeLearner("classif.lda", method = "mle")
print(lrn$par.vals)
lrn <- makeLearner("classif.lda", method = "t", nu = 10)
print(lrn$par.vals)
}
\seealso{
[\code{\link{resample}}],
[\code{\link{predict.WrappedModel}}]
}


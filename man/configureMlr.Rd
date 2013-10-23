\name{configureMlr}
\alias{configureMlr}
\title{Configures the behaviour of the package.}
\usage{
  configureMlr(on.learner.error = "stop",
    on.par.without.desc = "stop",
    show.learner.output = TRUE)
}
\arguments{
  \item{on.learner.error}{[\code{character(1)}]\cr What
  should happen if an error in an underlying learning
  algorithm is caught:\cr \dQuote{stop}: R exception is
  generated.\cr \dQuote{warn}: A \code{FailureModel} will
  be created, which predicts only NAs and a warning will be
  generated.\cr \dQuote{quiet}: Same as \dQuote{warn} but
  without the warning.\cr Default is \dQuote{stop}.}

  \item{on.par.without.desc}{[\code{character(1)}]\cr What
  should happen if a parameter of a learner is set to a
  value, but no parameter description object exists,
  indicating a possibly wrong name:\cr \dQuote{stop}: R
  exception is generated.\cr \dQuote{warn}: Warning, but
  parameter is still passed along to learner.\cr
  \dQuote{quiet}: Same as \dQuote{warn} but without the
  warning.\cr Default is \dQuote{stop}.}

  \item{show.learner.output}{[\code{logical(1)}]\cr Should
  the output of the learning algorithm during training and
  prediction be shown or captured and suppressed?  Default
  is \code{TRUE}.}
}
\value{
  Nothing.
}
\description{
  Configuration is done by setting custom
  \code{\link{options}}.
}


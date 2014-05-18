\name{configureMlr}
\alias{configureMlr}
\title{Configures the behavior of the package.}
\usage{
configureMlr(on.learner.error, on.learner.warning, on.par.without.desc,
  show.learner.output)
}
\arguments{
  \item{on.learner.error}{[\code{character(1)}]\cr What
  should happen if an error in an underlying learning
  algorithm is caught:\cr \dQuote{stop}: R exception is
  generated.\cr \dQuote{warn}: A \code{FailureModel} will
  be created, which predicts only NAs and a warning will be
  generated.\cr \dQuote{quiet}: Same as \dQuote{warn} but
  without the warning.\cr Default is \dQuote{stop}.}

  \item{on.learner.warning}{[\code{character(1)}]\cr What
  should happen if a warning in an underlying learning
  algorithm is generated:\cr \dQuote{warn}: The warning is
  generated as usual.\cr \dQuote{quiet}: The warning is
  suppressed.\cr Default is \dQuote{warn}.}

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
Configuration is done by setting custom \code{\link{options}}.

If you do not set an option here, its current value will be kept.

If you call this function with an empty argument list, everything is set to its defaults.
}
\seealso{
\code{\link{getMlrOptions}}
}


\name{removeConstantFactors}
\alias{removeConstantFactors}
\title{Remove constant factors from a data set.}
\usage{
removeConstantFactors(df, perc = 0)
}
\arguments{
  \item{df}{[\code{\link{data.frame}}]\cr The data set.}

  \item{df}{[\code{numeric}]\cr The percentage of a
  factor's values (in [0, 100]) that must differ from the
  mode value.  Default is 0, which means only constant
  factors with exactly one observed level are removed.}
}
\description{
Constant factors can lead to errors in some models. Given a
\code{data.frame}, this function removes all constant
factors. With the argument 'perc', there is a possibility
to also remove factors for whom less than 'perc' percent of
the observations differ from the mode value.
}


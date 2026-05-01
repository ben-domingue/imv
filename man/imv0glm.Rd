\name{imv0glm}
\alias{imv0glm}
\title{
IMV for a GLM compared to a prevalence baseline
}
\description{
Legacy function. Computes the IMV for a fitted \code{glm} model against a
null model (intercept only) via k-fold cross-validation. Both the full and
null models are refit on each training fold and evaluated on the held-out
fold.

For new code, prefer \code{imv(m0, m1)} with an explicit null model as
\code{m0}.
}
\usage{
imv0glm(m, nfold = 5)
}
\arguments{
  \item{m}{
A \code{glm} object fit with a binomial family. Must have been called with
an explicit \code{data} argument.
}
  \item{nfold}{
Number of cross-validation folds. Default 5.
}
}
\value{
A numeric vector of length \code{nfold} containing the per-fold IMV values.
}
\references{
https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0316491
}
\seealso{
\code{\link{imv}}, \code{\link{imv.binary}}
}
\examples{
set.seed(1)
x  <- rnorm(1000)
y  <- rbinom(length(x), 1, plogis(x))
df <- data.frame(x = x, y = y)
m  <- glm(y ~ x, df, family = "binomial")
imv0glm(m)
}

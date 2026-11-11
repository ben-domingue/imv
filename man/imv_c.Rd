\name{imv_c}
\alias{imv_c}
\alias{imv_t}
\title{
Polytomous IMVs for category discrimination and threshold prediction
}
\description{
Compute polytomous InterModel Vigorish (IMV) values for one item or outcome.
Each index splits categories \eqn{0, \dots, K-1} into binary prediction tasks,
scores every task with \code{\link{imv.binary}}, and returns a weighted mean.
\code{imv_c} targets discrimination between categories (\eqn{\omega_c}) and
\code{imv_t} targets ordinal threshold prediction (\eqn{\omega_t}).
}
\usage{
imv_c(x, P0, P1, K = NULL, sigma = 1e-04,
      weights = c("continuity", "legacy"))
imv_t(x, P0, P1, K = NULL, sigma = 1e-04,
      weights = c("continuity", "legacy"))
}
\arguments{
  \item{x}{
Integer responses coded \eqn{0, \dots, K-1}, without missing values. Recode
other codings to this range and pass \code{K} so that an unobserved category
keeps its column.
}
  \item{P0}{
Baseline category probabilities: an \eqn{N \times K}{N x K} matrix with columns
in category order, or a length-\code{K} vector recycled over rows.
}
  \item{P1}{
Enhanced category probabilities, in the same form as \code{P0}.
}
  \item{K}{
Number of categories. Defaults to the number of columns (or length) of
\code{P1}.
}
  \item{sigma}{
Small positive constant used to clip probabilities in each binary task. Default
\code{1e-4}.
}
  \item{weights}{
How the binary tasks are weighted: \code{"continuity"} (default), the weights of
the working note, or \code{"legacy"}, the weights used before it. See Details.
\code{"legacy"} is deprecated.
}
}
\details{
For \eqn{\omega_c}, the tasks are all category pairs \eqn{k < k'}. A task uses
responses in \eqn{\{k, k'\}}, the outcome \eqn{r = I(x = k)}, and the prediction
\eqn{q = P(k) / (P(k) + P(k'))}.

For \eqn{\omega_t}, the tasks are the cuts \eqn{k = 0, \dots, K-2}, with outcome
\eqn{r = I(x \le k)} and cumulative predictions.

Both indices are weighted means of their tasks' IMVs. All counts \eqn{N_k} are
the numbers of responses in each category of \code{x}.

With \code{weights = "continuity"} (the default), the pair \eqn{(k, k')} has
weight \eqn{N_k N_{k'}}, which grows with the evidence available from both
categories, and cut \eqn{k} has weight
\deqn{v_k = N_{\le k} (N - N_{\le k}) (N_k + N_{k+1}) / N,}{v_k = N_<=k (N - N_<=k) (N_k + N_(k+1)) / N,}
which favours balanced cuts and those next to well-populated categories. With
these weights both indices change continuously as a category empties.

With \code{weights = "legacy"}, the definitions used before the working note
are reproduced. Cut \eqn{k} has weight \eqn{N_k}, so the top category
carries no cut. \eqn{\omega_c} is a two-stage mean: the pairs that include
category \eqn{k} are averaged with weights \eqn{N_{kk'} = N_k + N_{k'}}, and
these category means are then averaged with weights \eqn{N_k}. Because the
pair IMV is symmetric in \eqn{k} and \eqn{k'}, this is one weighted mean over
pairs with
\deqn{w_{kk'} = N_{kk'} (N_k / D_k + N_{k'} / D_{k'}),}{w_kk' = N_kk' (N_k / D_k + N_k' / D_k'),}
where \eqn{D_k} is the sum of \eqn{N_{kk''}} over the other categories
\eqn{k''} that have responses. \code{components} shows these weights. Leaving
the pairs of an empty category out of \eqn{D_k} is a convention of this
package: the earlier code assumed every category \eqn{0, \dots, K-1} was
observed and did not run otherwise, so with an empty category there is no
earlier result for \code{"legacy"} to reproduce.

A task whose outcome is constant, for example because a category is empty, is
\code{NA} and has zero weight. With two categories, both indices equal
\code{\link{imv.binary}}.

A model that predicts worse than a fair coin in a task is treated as a fair coin
there. Each call gives at most one warning, which names the affected tasks.
}
\section{Deprecated}{
\code{weights = "legacy"} is deprecated and will be removed in a future release.
It is kept so that results computed with the earlier definitions can be
reproduced. Using it gives a warning of class \code{"imv_deprecated"} once per
session.
}
\value{
A scalar weighted-mean IMV. The per-task results are in
\code{attr(, "components")}, a data frame with columns \code{omega} and
\code{weight}, plus \code{k} and \code{kp} for \code{imv_c} or \code{k} for
\code{imv_t}.
}
\references{
Kanopka, K. (2026). Polytomous IMVs: \eqn{\omega_c} and \eqn{\omega_t}. Working
note.
}
\seealso{
\code{\link{imv.binary}}, \code{\link{imv.SingleGroupClass}}
}
\examples{
set.seed(1)
N <- 500
theta <- rnorm(N)
cum <- sapply(c(-1, 0, 1), function(b) plogis(1.5 * (theta - b)))
P1 <- cbind(1 - cum[, 1], cum[, 1] - cum[, 2], cum[, 2] - cum[, 3], cum[, 3])
x <- apply(P1, 1, function(p) sample(0:3, 1, prob = p))
P0 <- tabulate(x + 1, 4) / N
imv_c(x, P0, P1, K = 4)
imv_t(x, P0, P1, K = 4)
}

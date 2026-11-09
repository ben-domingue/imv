\name{imv.SingleGroupClass}
\alias{imv.SingleGroupClass}
\title{
Cross-validated IMV for mirt IRT models
}
\description{
Computes the InterModel Vigorish (IMV) for item response theory models fit
with the \code{mirt} package via response-level k-fold cross-validation.
Fold splits are at the individual response level: each held-out observation is
a single person-by-item pair, and ability is estimated from the remaining
responses for that person.

When a single model is supplied (\code{m1 = NULL}), predictions from \code{m0}
are compared to item-level prevalence rates (the null model). When two models
are supplied, \code{m0} serves as the baseline and \code{m1} as the enhanced
model.

Polytomous items are supported through the \code{type} argument. When every
item is dichotomous, \code{type} has no effect.
}
\usage{
\method{imv}{SingleGroupClass}(m0, m1 = NULL, data = NULL, nfold = 5,
     predict_fn = NULL, y = NULL, seed = NULL,
     fscores.options = list(method = "EAP"),
     whole.matrix = TRUE,
     remove.nonvarying.items = TRUE,
     remove.allNA.rows = TRUE, type = c("c", "t"), ...)
}
\arguments{
  \item{m0}{
A \code{SingleGroupClass} model object returned by \code{mirt::mirt()}.
}
  \item{m1}{
An optional second \code{SingleGroupClass} model fit to the same data as
\code{m0}. When \code{NULL}, \code{m0} is compared to item prevalence.
}
  \item{data}{
Not used for mirt models. Accepted for consistency with the generic.
}
  \item{nfold}{
Number of cross-validation folds (default 5).
}
  \item{predict_fn}{
Not used for mirt models. Accepted for consistency with the generic.
}
  \item{y}{
Not used for mirt models. Accepted for consistency with the generic.
}
  \item{seed}{
Optional integer passed to \code{set.seed} before assigning observations
to folds, for reproducible cross-validation. Default \code{NULL}.
}
  \item{fscores.options}{
Named list of additional arguments passed to \code{mirt::fscores()}.
Default is \code{list(method = "EAP")}.
}
  \item{whole.matrix}{
Logical (default \code{TRUE}). When \code{TRUE}, fold assignment is repeated
until every training partition contains all participants and all items, ensuring
that models can be re-identified on each fold. Ignored when \code{m1 = NULL}.
With polytomous items, folds must also keep every category of every item in
training, whatever the value of \code{whole.matrix}.
}
  \item{remove.nonvarying.items}{
Logical (default \code{TRUE}). Drop items with no response variance from each
training fold's response matrix.
}
  \item{remove.allNA.rows}{
Logical (default \code{TRUE}). Drop persons whose responses are entirely
missing from a training fold's response matrix.
}
  \item{type}{
Polytomous index: \code{"c"} (default) for \code{\link{imv_c}} or \code{"t"}
for \code{\link{imv_t}}. Ignored when every item is dichotomous.
}
  \item{...}{
Currently unused.
}
}
\details{
When any item has more than two categories, each fold computes the chosen index
for each item on its held-out responses and takes the unweighted mean across
items. Responses are mapped to \eqn{0, \dots, K-1} by sorted unique value within
each item, as \code{mirt} does. With \code{m1 = NULL}, the baseline for an item
is its category prevalence in the training fold. Item-level prevalence is also
the baseline for mixed tests, so results for dichotomous items in such a test
are not pooled as they are when all items are dichotomous.

Fold assignment is repeated until each category of each item appears in every
training fold; an item with a category observed only once gives an error.
}
\value{
A named list with four elements:
\describe{
  \item{folds}{Numeric vector of per-fold IMV values (length \code{nfold}).}
  \item{mean}{Mean IMV across folds.}
  \item{sd}{Standard deviation of per-fold IMVs.}
  \item{ci}{Named numeric vector of length 2: a 95\% interval computed as
    mean +/- 1.96 * (sd / sqrt(nfold)).}
}
}
\references{
Domingue, B. W., Rahal, C., Faul, J., Freese, J., Kanopka, K., Rigos, A.,
Stenhaug, B., & Tripathi, A. S. (2025). The InterModel Vigorish (IMV) as a
flexible and portable approach for quantifying predictive accuracy with binary
outcomes. \emph{PloS one}, 20(3), e0316491.
\url{https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0316491}
}
\seealso{
\code{\link{imv}}, \code{\link{imv.binary}}, \code{\link{imv_c}}
}
\examples{
\donttest{
if (requireNamespace("mirt", quietly = TRUE)) {
  set.seed(1)
  resp <- mirt::expand.table(mirt::LSAT7)

  # Single model vs prevalence baseline
  mod1 <- mirt::mirt(resp, 1, "Rasch", verbose = FALSE)
  imv(mod1)

  # Two models
  mod2 <- mirt::mirt(resp, 1, verbose = FALSE)
  imv(mod1, mod2)

  # Priors specified as a variable are handled correctly
  my_prior <- list(a1 = c(0, 1, 0.25, 3))
  mod3 <- mirt::mirt(resp, 1, prior.list = my_prior, verbose = FALSE)
  imv(mod3)

  # Polytomous items
  poly <- mirt::mirt(mirt::Science, 1, itemtype = "graded", verbose = FALSE)
  imv(poly, type = "t")
}
}
}

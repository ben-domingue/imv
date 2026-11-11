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
are compared to item-level prevalence rates (the null model; see
\code{baseline}). When two models
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
     remove.allNA.rows = TRUE, type = c("c", "t"),
     weights = c("continuity", "legacy"), aggregate = NULL,
     item.weights = c("responses", "equal"),
     baseline = c("item", "grand"), ...)
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
  \item{weights}{
Task weights passed to \code{\link{imv_c}} or \code{\link{imv_t}}:
\code{"continuity"} (default) or the deprecated \code{"legacy"}. Ignored when
every item is dichotomous.
}
  \item{aggregate}{
How each fold combines items: \code{"items"} computes one IMV per item and
takes their weighted mean (see \code{item.weights}); \code{"pooled"} stacks the
held-out responses of all items and makes a single IMV call. The default,
\code{NULL}, is resolved as follows:
\itemize{
  \item all items dichotomous: \code{"pooled"} (backward compatible with the
    existing binary behaviour);
  \item any item polytomous: \code{"items"}.
}
For dichotomous models the pooled default combines items as earlier versions
did; with \code{m1 = NULL} the baseline is now item-level (see
\code{baseline}).
\code{"pooled"} requires every item to have the same set of response
categories; otherwise it stops before fitting, naming the items and their
numbers of categories.
}
  \item{item.weights}{
Used only when \code{aggregate = "items"}. \code{"responses"} (default) weights
each item by the number of its held-out responses scored in the fold, so items
with more missing data count less; \code{"equal"} gives every scored item
weight 1. Items with an \code{NA} IMV or no scored responses are dropped.
}
  \item{baseline}{
The null model used when \code{m1 = NULL}; ignored when \code{m1} is supplied.
\code{"item"} (default) predicts each held-out response from its own item's
prevalence in the training fold: the mean training response for a dichotomous
item, or the training category proportions for a polytomous item.
\code{"grand"} uses one prevalence per fold for every item, computed from the
training responses of all items pooled. This was the baseline for dichotomous
models before version 0.5.0. For polytomous items, \code{"grand"} requires
every item to have the same set of response categories and otherwise stops
before fitting. See Details for what \code{"grand"} is for.
}
  \item{...}{
Currently unused.
}
}
\details{
Within each fold, items are combined as set by \code{aggregate}. For
dichotomous items each IMV is \code{\link{imv.binary}}; for polytomous items it
is the index chosen by \code{type}. Responses are mapped to
\eqn{0, \dots, K-1} by sorted unique value within each item, as \code{mirt}
does. With \code{m1 = NULL} and the default \code{baseline = "item"}, the
baseline for an item is its category prevalence in the training fold, for
dichotomous and polytomous items alike. An item whose training responses are
all the same gets a baseline probability of 0 or 1, which the clipping in
\code{\link{imv.binary}} handles.

The \code{"grand"} baseline knows the overall rate of each response but nothing
about which item is being answered. It has three uses:
\itemize{
  \item reproducing results computed for dichotomous models before version
    0.5.0, when it was the only baseline;
  \item measuring how much of an earlier IMV came from the baseline: the
    difference between the \code{"grand"} and \code{"item"} results is the part
    of the old value that reflected the weaker baseline, not the model;
  \item measuring the contribution of item-specific information to prediction.
    Against \code{"grand"}, the IMV credits the model both for knowing that
    items differ in difficulty (or category use) and for knowing how persons
    differ; against \code{"item"}, only for the latter. When items differ
    widely, most of the IMV against \code{"grand"} comes from item information.
}

Item-level aggregation is the default for polytomous models because category
labels are item-specific. Category 2 of one item and category 2 of another
need not mean the same thing, and items can have different numbers of
categories, so the binary tasks of a pooled index (category pairs or cuts)
would mix unrelated comparisons. Pooling across items is meaningful only when
all items share one response format, such as a common rating scale; then
\code{aggregate = "pooled"} may be used. Dichotomous items always share a
format, so pooling stays their default.

With polytomous items, fold assignment is repeated until each category of each
item appears in every training fold; an item with a category observed only once
gives an error.
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
  imv(poly, type = "t", item.weights = "equal")
  imv(poly, aggregate = "pooled")   # all Science items share one 4-point scale
}
}
}

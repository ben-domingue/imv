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

Only dichotomous response models are supported.
}
\usage{
\method{imv}{SingleGroupClass}(m0, m1 = NULL, nfold = 5,
     fscores.options = list(method = "EAP"),
     whole.matrix = TRUE,
     remove.nonvarying.items = TRUE,
     remove.allNA.rows = TRUE, ...)
}
\arguments{
  \item{m0}{
A \code{SingleGroupClass} model object returned by \code{mirt::mirt()}.
}
  \item{m1}{
An optional second \code{SingleGroupClass} model fit to the same data as
\code{m0}. When \code{NULL}, \code{m0} is compared to item prevalence.
}
  \item{nfold}{
Number of cross-validation folds (default 5).
}
  \item{fscores.options}{
Named list of additional arguments passed to \code{mirt::fscores()}.
Default is \code{list(method = "EAP")}.
}
  \item{whole.matrix}{
Logical (default \code{TRUE}). When \code{TRUE}, fold assignment is repeated
until every training partition contains all participants and all items, ensuring
that models can be re-identified on each fold. Ignored when \code{m1 = NULL}.
}
  \item{remove.nonvarying.items}{
Logical (default \code{TRUE}). Drop items with no response variance from each
training fold's response matrix.
}
  \item{remove.allNA.rows}{
Logical (default \code{TRUE}). Drop persons whose responses are entirely
missing from a training fold's response matrix.
}
  \item{...}{
Currently unused.
}
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
https://osf.io/preprints/socarxiv/gu3ap/
}
\seealso{
\code{\link{imv}}, \code{\link{imv.binary}}
}
\examples{
\dontrun{
library(mirt)
set.seed(1)
resp <- expand.table(LSAT7)

# Single model vs prevalence baseline
mod1 <- mirt(resp, 1, "Rasch", verbose = FALSE)
imv(mod1)

# Two models
mod2 <- mirt(resp, 1, verbose = FALSE)
imv(mod1, mod2)

# Priors specified as a variable are handled correctly
my_prior <- list(a1 = c(0, 1, 0.25, 3))
mod3 <- mirt(resp, 1, prior.list = my_prior, verbose = FALSE)
imv(mod3)
}
}

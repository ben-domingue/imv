\name{imv.mirt}
\alias{imv.mirt}
\title{
Cross-validated IMV for mirt IRT models
}
\description{
Computes the InterModel Vigorish (IMV) for item response theory models fit
with the \code{mirt} package. When a single model is supplied, predictions
are compared to item-level prevalence rates (the null model). When two models
are supplied, predictions from \code{mod1} serve as the baseline and those
from \code{mod2} as the enhanced model.
}
\usage{
imv.mirt(mod1, mod2 = NULL, nfold = 5,
         fscores.options = list(method = "EAP"),
         whole.matrix = TRUE, ...)
}
\arguments{
  \item{mod1}{
A model object returned by \code{mirt::mirt()}.
}
  \item{mod2}{
An optional second model returned by \code{mirt::mirt()}, fit to the same
data as \code{mod1}. When \code{NULL}, \code{mod1} is compared to item
prevalence.
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
until every training partition contains all participants and all items, to
ensure that models can be identified.
}
  \item{...}{
Additional arguments passed to \code{makeresponse()}.
}
}
\value{
A numeric vector of length \code{nfold} containing the per-fold IMV values.
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
mod1 <- mirt(resp, 1, 'Rasch')
imv.mirt(mod1)
}
}

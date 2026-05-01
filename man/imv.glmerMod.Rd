\name{imv.glmerMod}
\alias{imv.glmerMod}
\title{
Cross-validated IMV for binomial mixed-effects models
}
\description{
Computes the InterModel Vigorish (IMV) for binomial mixed-effects models fit
with \code{lme4::glmer()} via k-fold cross-validation. Both models are refit
on each training fold; predictions on the held-out fold use
\code{allow.new.levels = TRUE} to handle random-effect levels not seen during
training.

Only binomial family models are supported.
}
\usage{
\method{imv}{glmerMod}(m0, m1, data = NULL, nfold = 4, predict_fn = NULL, y = NULL, ...)
}
\arguments{
  \item{m0}{
A \code{glmerMod} object (binomial family) serving as the baseline model.
}
  \item{m1}{
A \code{glmerMod} object (binomial family) serving as the enhanced model.
Must be fit to the same data as \code{m0}.
}
  \item{data}{
Optional data frame. If \code{NULL}, extracted from \code{model.frame(m1)}.
}
  \item{nfold}{
Number of cross-validation folds. Default 4.
}
  \item{predict_fn}{
Ignored for this method.
}
  \item{y}{
Ignored for this method; the outcome is inferred from the model formula.
}
  \item{...}{
Currently unused. Accepted for consistency with the generic.
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
Domingue, B. W., Rahal, C., Faul, J., Freese, J., Kanopka, K., Rigos, A.,
Stenhaug, B., & Tripathi, A. S. (2025). The InterModel Vigorish (IMV) as a
flexible and portable approach for quantifying predictive accuracy with binary
outcomes. \emph{PloS one}, 20(3), e0316491.
\url{https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0316491}
}
\seealso{
\code{\link{imv}}, \code{\link{imv.binary}}
}
\examples{
\dontrun{
library(lme4)
data(sleepstudy)
# Binarise for illustration
sleepstudy$slow <- as.integer(sleepstudy$Reaction > 300)

m0 <- glmer(slow ~ (1 | Subject), sleepstudy, family = binomial)
m1 <- glmer(slow ~ Days + (1 | Subject), sleepstudy, family = binomial)
imv(m0, m1)
}
}

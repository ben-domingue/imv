\name{imv}
\alias{imv}
\alias{imv.default}
\alias{imv.glm}
\alias{imv.glmerMod}
\title{
Cross-validated IMV for comparing two models
}
\description{
S3 generic that computes the InterModel Vigorish (IMV) between a baseline
model \code{m0} and an enhanced model \code{m1} via k-fold cross-validation.
For each fold, both models are refit on the training partition and evaluated on
the held-out partition; the IMV is computed from those out-of-fold predictions.

\code{imv.default} provides an escape hatch for unsupported model types via
\code{predict_fn}: in that case the original fitted models are used (without
refitting) to obtain predictions on each test fold.
}
\usage{
imv(m0, m1, data = NULL, nfold = 4, predict_fn = NULL, y = NULL)

\method{imv}{glm}(m0, m1, data = NULL, nfold = 4, predict_fn = NULL, y = NULL)

\method{imv}{glmerMod}(m0, m1, data = NULL, nfold = 4, predict_fn = NULL, y = NULL)

\method{imv}{default}(m0, m1, data = NULL, nfold = 4, predict_fn = NULL, y = NULL)
}
\arguments{
  \item{m0}{
Baseline model.
}
  \item{m1}{
Enhanced model.
}
  \item{data}{
Data frame used for cross-validation. May be omitted for model classes that
store training data internally (e.g., objects from \code{glm()} or
\code{lme4::glmer()}).
}
  \item{nfold}{
Number of cross-validation folds (default 4).
}
  \item{predict_fn}{
Optional function with signature \code{function(model, newdata)} returning a
numeric vector of predicted probabilities. When supplied, \code{imv.default}
is invoked and models are not refit per fold.
}
  \item{y}{
Character string naming the binary outcome column in \code{data}. Required
when \code{predict_fn} is supplied; otherwise inferred from the model formula.
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
\code{\link{imv.binary}}
}
\examples{
set.seed(1)
x  <- rnorm(200)
y  <- rbinom(200, 1, plogis(x))
df <- data.frame(x = x, y = y)
m0 <- glm(y ~ 1,  df, family = "binomial")
m1 <- glm(y ~ x,  df, family = "binomial")
result <- imv(m0, m1)
result$mean
result$ci

## escape hatch for an unsupported model type
## (here we reuse glm just to illustrate the interface)
pfn <- function(model, newdata) predict(model, newdata, type = "response")
result2 <- imv(m0, m1, data = df, y = "y", predict_fn = pfn)
result2$mean
}

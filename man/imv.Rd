\name{imv}
\alias{imv}
\alias{imv.default}
\alias{imv.glm}
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
imv(m0, m1, ...)

\method{imv}{glm}(m0, m1, data = NULL, nfold = 4, predict_fn = NULL, y = NULL, ...)

\method{imv}{default}(m0, m1, data = NULL, nfold = 4, predict_fn = NULL, y = NULL, ...)
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
  \item{...}{
Additional arguments passed to methods.
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
\code{\link{imv.binary}}, \code{\link{imv.glmerMod}}, \code{\link{imv.SingleGroupClass}}
}
\examples{
## --- glm ------------------------------------------------------------
set.seed(1)
x  <- rnorm(100)
y  <- rbinom(100, 1, plogis(x))
df <- data.frame(x = x, y = y)
m0 <- glm(y ~ 1, df, family = "binomial")
m1 <- glm(y ~ x, df, family = "binomial")
result <- imv(m0, m1, nfold = 2)
result$mean
result$ci

## --- custom predict_fn (escape hatch for unsupported model types) ---
pfn    <- function(model, newdata) predict(model, newdata, type = "response")
result <- imv(m0, m1, data = df, y = "y", predict_fn = pfn, nfold = 2)
result$mean

\donttest{
## --- glmer (requires lme4) ------------------------------------------
if (requireNamespace("lme4", quietly = TRUE)) {
  data(sleepstudy, package = "lme4")
  sleepstudy$slow <- as.integer(sleepstudy$Reaction > 300)
  m0 <- lme4::glmer(slow ~ (1 | Subject),        sleepstudy, family = binomial)
  m1 <- lme4::glmer(slow ~ Days + (1 | Subject), sleepstudy, family = binomial)
  imv(m0, m1)
}

## --- mirt (requires mirt) -------------------------------------------
if (requireNamespace("mirt", quietly = TRUE)) {
  resp <- mirt::expand.table(mirt::LSAT7)
  mod1 <- mirt::mirt(resp, 1, "Rasch", verbose = FALSE)  # 1PL
  mod2 <- mirt::mirt(resp, 1, verbose = FALSE)            # 2PL
  imv(mod1, mod2)
}
}

## --- ranger (requires ranger) ---------------------------------------
\dontrun{
library(ranger)
set.seed(1)
x1    <- rnorm(200); x2 <- rnorm(200)
y_fac <- factor(rbinom(200, 1, plogis(2 * x1)))
df_r  <- data.frame(y = y_fac, x1 = x1, x2 = x2)
m0 <- ranger(dependent.variable.name = "y", data = df_r[, c("y", "x2")],
             num.trees = 100, probability = TRUE)
m1 <- ranger(dependent.variable.name = "y", data = df_r,
             num.trees = 100, probability = TRUE)
imv(m0, m1, data = df_r)
}

## --- tidymodels workflow (requires workflows + parsnip) -------------
\dontrun{
library(workflows)
library(parsnip)
set.seed(1)
x     <- rnorm(200)
y_fac <- factor(rbinom(200, 1, plogis(x)))
df_w  <- data.frame(x = x, y = y_fac)
spec <- logistic_reg() |> set_engine("glm")
wf0  <- workflow() |> add_model(spec) |> add_formula(y ~ 1) |> fit(df_w)
wf1  <- workflow() |> add_model(spec) |> add_formula(y ~ x) |> fit(df_w)
imv(wf0, wf1, data = df_w)
}

## --- custom predict_fn (escape hatch for unsupported model types) ---
pfn    <- function(model, newdata) predict(model, newdata, type = "response")
result <- imv(m0, m1, data = df, y = "y", predict_fn = pfn)
result$mean
}

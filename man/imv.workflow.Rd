\name{imv.workflow}
\alias{imv.workflow}
\title{
Cross-validated IMV for tidymodels workflows
}
\description{
Computes the InterModel Vigorish (IMV) for fitted \code{workflow} objects
(from the \pkg{workflows} package) via k-fold cross-validation. Both
workflows are refit on each training fold using \code{workflows::fit()} and
predictions are obtained with \code{predict(..., type = "prob")}.

Works with any \pkg{parsnip} engine that supports probability predictions
(logistic regression, random forests, boosted trees, etc.). The outcome
must be a binary factor; the positive class is the second factor level.
}
\usage{
\method{imv}{workflow}(m0, m1, data = NULL, nfold = 5, predict_fn = NULL, y = NULL, ...)
}
\arguments{
  \item{m0}{
A fitted \code{workflow} object serving as the baseline model.
}
  \item{m1}{
A fitted \code{workflow} object serving as the enhanced model.
}
  \item{data}{
Data frame used for cross-validation. Required: workflow models do not
store training data internally.
}
  \item{nfold}{
Number of cross-validation folds. Default 5.
}
  \item{predict_fn}{
Ignored for this method.
}
  \item{y}{
Character string naming the binary outcome column in \code{data}. For formula-based workflows, \code{y} is inferred automatically via
\code{workflows::extract_preprocessor()}. Required for recipe-based
workflows.
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
library(workflows)
library(parsnip)
set.seed(1)
n  <- 300
x1 <- rnorm(n); x2 <- rnorm(n)
y  <- factor(rbinom(n, 1, plogis(2 * x1)))
df <- data.frame(y = y, x1 = x1, x2 = x2)

spec <- logistic_reg() |> set_engine("glm")

wf0 <- workflow() |> add_model(spec) |> add_formula(y ~ x2)
wf0 <- fit(wf0, data = df)

wf1 <- workflow() |> add_model(spec) |> add_formula(y ~ x1 + x2)
wf1 <- fit(wf1, data = df)

imv(wf0, wf1, data = df)
}
}

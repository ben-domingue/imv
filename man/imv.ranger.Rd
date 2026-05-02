\name{imv.ranger}
\alias{imv.ranger}
\title{
Cross-validated IMV for ranger random forests
}
\description{
Computes the InterModel Vigorish (IMV) for probability random forests fit
with \code{ranger::ranger()} via k-fold cross-validation. Both models are
refit on each training fold and evaluated on the held-out fold.

Models must be fit with \code{probability = TRUE} and a binary outcome coded
as a two-level factor. The positive class is taken as the second factor level
(i.e., the higher value for 0/1-coded outcomes).
}
\usage{
\method{imv}{ranger}(m0, m1, data = NULL, nfold = 5, predict_fn = NULL, y = NULL, ...)
}
\arguments{
  \item{m0}{
A \code{ranger} object (probability forest) serving as the baseline model.
}
  \item{m1}{
A \code{ranger} object (probability forest) serving as the enhanced model.
}
  \item{data}{
Data frame containing all variables used by \code{m0} and \code{m1}.
Required: ranger models do not store training data internally.
}
  \item{nfold}{
Number of cross-validation folds. Default 5.
}
  \item{predict_fn}{
Ignored for this method.
}
  \item{y}{
Ignored for this method; the outcome is taken from
\code{m1$dependent.variable.name}.
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
library(ranger)
set.seed(1)
n  <- 300
x1 <- rnorm(n); x2 <- rnorm(n)
y  <- factor(rbinom(n, 1, plogis(2 * x1)))
df <- data.frame(y = y, x1 = x1, x2 = x2)

# m0 uses only the noise predictor; m1 uses both
m0 <- ranger(dependent.variable.name = "y", data = df[, c("y", "x2")],
             num.trees = 100, probability = TRUE)
m1 <- ranger(dependent.variable.name = "y", data = df,
             num.trees = 100, probability = TRUE)
imv(m0, m1, data = df)
}
}

\name{imvglm.rmvar}
\alias{imvglm.rmvar}
\title{
IMV for a GLM versus the same model with one variable removed
}
\description{
Legacy function. Computes the IMV comparing a full \code{glm} model to a
reduced model with \code{var.nm} dropped from the formula, via k-fold
cross-validation. Both models are refit on each training fold and evaluated
on the held-out fold.

For new code, prefer constructing both models explicitly and calling
\code{imv(m0, m1)}.
}
\usage{
imvglm.rmvar(m, nfold = 5, var.nm)
}
\arguments{
  \item{m}{
A \code{glm} object fit with a binomial family. Must have been called with
an explicit \code{data} argument.
}
  \item{nfold}{
Number of cross-validation folds. Default 5.
}
  \item{var.nm}{
Character string naming the variable to remove from the formula. Must match
exactly the term as it appears in the original \code{glm} call.
}
}
\value{
A numeric vector of length \code{nfold} containing the per-fold IMV values.
The reduced model (without \code{var.nm}) serves as the baseline; the full
model serves as the enhanced model. A positive mean indicates that
\code{var.nm} improves out-of-sample prediction.
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
set.seed(1)
x  <- rnorm(1000)
z  <- rnorm(1000)
y  <- rbinom(length(x), 1, plogis(x))
df <- data.frame(x = x, z = z, y = y)
m  <- glm(y ~ x + z, df, family = "binomial")
imvglm.rmvar(m, var.nm = "z")
}

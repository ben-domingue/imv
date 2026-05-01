\name{imv.binary}
\alias{imv.binary}
\title{
Compute IMV for binary outcomes
}
\description{
Computes the InterModel Vigorish (IMV) comparing baseline predictions
\code{p1} to enhanced predictions \code{p2} for binary outcomes \code{y}.
A positive value indicates that \code{p2} predicts better than \code{p1}
out of sample; a negative value indicates the reverse.
}
\usage{
\method{imv}{binary}(m0, m1, p2, sigma = 1e-04, ...)
}
\arguments{
  \item{m0}{
Integer or numeric vector of binary outcomes (0/1), preferably from a
held-out test set.
}
  \item{m1}{
Numeric vector of baseline predicted probabilities (same length as \code{m0}).
}
  \item{p2}{
Numeric vector of enhanced predicted probabilities (same length as \code{m0}).
}
  \item{sigma}{
Small positive constant used to clip probabilities away from 0 and 1 to
avoid numerical issues. Default \code{1e-4}.
}
  \item{...}{
Currently unused. Accepted for consistency with the \code{imv} generic.
}
}
\value{
A scalar IMV value. Positive values favour \code{p2}; negative values
favour \code{p1}.
}
\references{
Domingue, B. W., Rahal, C., Faul, J., Freese, J., Kanopka, K., Rigos, A.,
Stenhaug, B., & Tripathi, A. S. (2025). The InterModel Vigorish (IMV) as a
flexible and portable approach for quantifying predictive accuracy with binary
outcomes. \emph{PloS one}, 20(3), e0316491.
\url{https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0316491}
}
\seealso{
\code{\link{imv}}
}
\examples{
set.seed(1)
x  <- rnorm(1000)
y  <- rbinom(length(x), 1, plogis(x))
df <- data.frame(x = x, y = y)
m  <- glm(y ~ x, df, family = "binomial")
pr <- predict(m, data.frame(x = x), type = "response")
imv.binary(y, mean(y), pr)
}

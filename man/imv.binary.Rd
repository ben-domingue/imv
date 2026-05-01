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
imv.binary(y, p1, p2, sigma = 1e-4)
}
\arguments{
  \item{y}{
Integer or numeric vector of binary outcomes (0/1), preferably from a
held-out test set.
}
  \item{p1}{
Numeric vector of baseline predicted probabilities (same length as \code{y}).
}
  \item{p2}{
Numeric vector of enhanced predicted probabilities (same length as \code{y}).
}
  \item{sigma}{
Small positive constant used to clip probabilities away from 0 and 1 to
avoid numerical issues. Default \code{1e-4}.
}
}
\value{
A scalar IMV value. Positive values favour \code{p2}; negative values
favour \code{p1}.
}
\references{
https://osf.io/preprints/socarxiv/gu3ap/
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

imv.binary <- function(y,             # binary 0/1 outcome vector
                       p0,            # baseline predicted probabilities
                       p1,            # enhanced predicted probabilities
                       sigma = 1e-4,  # clips extremes to avoid log(0)
                       ...) {
    p0 <- pmax(pmin(p0, 1 - sigma), sigma)
    p1 <- pmax(pmin(p1, 1 - sigma), sigma)
    ll <- function(x, p) exp(sum(log(p) * x + log(1 - p) * (1 - x)) / length(x))
    getcoins <- function(a) {
        f <- function(p, a) abs(p * log(p) + (1 - p) * log(1 - p) - log(a))
        nlminb(.5, f, lower = 0.001, upper = .999, a = a)$par
    }
    c0 <- getcoins(ll(y, p0))
    c1 <- getcoins(ll(y, p1))
    (c1 - c0) / c0
}

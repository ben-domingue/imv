## Polytomous IMVs: omega_c (category discrimination) and omega_t (threshold /
## ordinal prediction). Each breaks an item into binary prediction tasks,
## scores each with imv.binary(), and takes a weighted mean.
##
## x       responses coded 0, ..., K-1 (one item)
## P0, P1  N x K matrices of category probabilities (baseline, enhanced),
##         columns in category order; a length-K vector is recycled over rows
## K       number of categories; pass it so an unobserved category keeps a column
##
## Both return the weighted-mean IMV with the per-task results attached as
## attr(, "components"). Tasks whose outcome is constant (empty category) are
## NA and get zero weight. At K = 2 both reduce to imv.binary() exactly.

imv_c <- function(x, P0, P1, K = NULL, sigma = 1e-4) {
    s  <- .poly_setup(x, P0, P1, K)
    K  <- s$K
    pr <- utils::combn(K, 2) - 1L
    comp <- data.frame(k = pr[1, ], kp = pr[2, ], omega = NA_real_,
                       weight = s$Nk[pr[1, ] + 1] * s$Nk[pr[2, ] + 1])
    for (j in seq_len(ncol(pr))) {
        k  <- pr[1, j]; kp <- pr[2, j]
        i  <- s$x == k | s$x == kp
        q  <- function(P) {
            v <- P[i, k + 1] / (P[i, k + 1] + P[i, kp + 1])
            v[!is.finite(v)] <- 0.5
            v
        }
        comp$omega[j] <- .imv_task(as.numeric(s$x[i] == k), q(s$P0), q(s$P1), sigma)
    }
    structure(.poly_wmean(comp$omega, comp$weight), components = comp)
}

imv_t <- function(x, P0, P1, K = NULL, sigma = 1e-4) {
    s   <- .poly_setup(x, P0, P1, K)
    K   <- s$K; N <- length(s$x); Nk <- s$Nk
    U   <- upper.tri(diag(K), diag = TRUE) * 1
    C0  <- s$P0 %*% U
    C1  <- s$P1 %*% U
    nlo <- cumsum(Nk)[seq_len(K - 1)]
    comp <- data.frame(k = 0:(K - 2), omega = NA_real_,
                       weight = nlo * (N - nlo) * (Nk[-K] + Nk[-1]) / N)
    for (k in 0:(K - 2)) {
        comp$omega[k + 1] <- .imv_task(as.numeric(s$x <= k), C0[, k + 1], C1[, k + 1], sigma)
    }
    structure(.poly_wmean(comp$omega, comp$weight), components = comp)
}

## Internal helpers -----------------------------------------------------------

.poly_setup <- function(x, P0, P1, K) {
    if (is.null(K)) K <- if (is.null(dim(P1))) length(P1) else ncol(P1)
    K <- as.integer(K)
    if (K < 2) stop("K must be at least 2")
    if (anyNA(x)) stop("x contains NA; drop missing responses first")
    if (any(x != round(x)) || any(x < 0) || any(x > K - 1)) {
        stop("x must be coded 0, ..., K-1 (K = ", K, ")")
    }
    N <- length(x)
    list(x = as.integer(x), K = K, Nk = tabulate(as.integer(x) + 1L, nbins = K),
         P0 = .as_prob_matrix(P0, N, K), P1 = .as_prob_matrix(P1, N, K))
}

.as_prob_matrix <- function(P, N, K) {
    if (is.null(dim(P))) {
        if (length(P) != K) stop("probability vector must have length K = ", K)
        return(matrix(P, N, K, byrow = TRUE))
    }
    if (nrow(P) != N || ncol(P) != K) {
        stop("probability matrix must be ", N, " x ", K)
    }
    as.matrix(P)
}

## binary IMV for one task; NA when the outcome is empty or constant
.imv_task <- function(y, p0, p1, sigma) {
    if (length(y) == 0 || all(y == y[1])) return(NA_real_)
    imv.binary(y, p0, p1, sigma = sigma)
}

## weighted mean over tasks with a defined IMV and positive weight
.poly_wmean <- function(v, w) {
    ok <- !is.na(v) & w > 0
    if (!any(ok)) return(NA_real_)
    sum(w[ok] * v[ok]) / sum(w[ok])
}

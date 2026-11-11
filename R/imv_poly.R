## Polytomous IMVs: omega_c (category discrimination) and omega_t (threshold /
## ordinal prediction). Each breaks an item into binary prediction tasks,
## scores each with imv.binary(), and takes a weighted mean.
##
## x       responses coded 0, ..., K-1 (one item)
## P0, P1  N x K matrices of category probabilities (baseline, enhanced),
##         columns in category order; a length-K vector is recycled over rows
## K       number of categories; pass it so an unobserved category keeps a column
## weights "continuity" (the working note's weights) or "legacy" (the weights
##         used before the note; deprecated)
##
## Both return the weighted-mean IMV with the per-task results attached as
## attr(, "components"). Tasks whose outcome is constant (empty category) are
## NA and get zero weight. At K = 2 both reduce to imv.binary() exactly.

imv_c <- function(x, P0, P1, K = NULL, sigma = 1e-4,
                  weights = c("continuity", "legacy")) {
    weights <- match.arg(weights)
    if (weights == "legacy") .warn_legacy_weights()
    s <- .poly_setup(x, P0, P1, K)
    .tally_out_of_domain(choose(s$K, 2), res <- .omega_c(s, sigma, weights),
                         unit = "category pairs")
    res
}

imv_t <- function(x, P0, P1, K = NULL, sigma = 1e-4,
                  weights = c("continuity", "legacy")) {
    weights <- match.arg(weights)
    if (weights == "legacy") .warn_legacy_weights()
    s <- .poly_setup(x, P0, P1, K)
    .tally_out_of_domain(s$K - 1L, res <- .omega_t(s, sigma, weights), unit = "cuts")
    res
}

## Internal helpers -----------------------------------------------------------

## The .omega_* workers leave out-of-domain warnings unsummarised, tagged with
## their task, so that the caller decides how to tally them: per call in
## imv_c()/imv_t(), per fold when cross-validating.
.omega_c <- function(s, sigma, weights = "continuity") {
    K  <- s$K
    pr <- utils::combn(K, 2) - 1L
    comp <- data.frame(k = pr[1, ], kp = pr[2, ], omega = NA_real_,
                       weight = .weights_c(s$Nk, pr, weights))
    for (j in seq_len(ncol(pr))) {
        k  <- pr[1, j]; kp <- pr[2, j]
        i  <- s$x == k | s$x == kp
        q  <- function(P) {
            v <- P[i, k + 1] / (P[i, k + 1] + P[i, kp + 1])
            v[!is.finite(v)] <- 0.5
            v
        }
        comp$omega[j] <- .imv_task(as.numeric(s$x[i] == k), q(s$P0), q(s$P1), sigma,
                                   task = paste(k, "vs", kp))
    }
    structure(.poly_wmean(comp$omega, comp$weight), components = comp)
}

.omega_t <- function(s, sigma, weights = "continuity") {
    K   <- s$K
    U   <- upper.tri(diag(K), diag = TRUE) * 1
    C0  <- s$P0 %*% U
    C1  <- s$P1 %*% U
    comp <- data.frame(k = 0:(K - 2), omega = NA_real_,
                       weight = .weights_t(s$Nk, weights))
    for (k in 0:(K - 2)) {
        comp$omega[k + 1] <- .imv_task(as.numeric(s$x <= k), C0[, k + 1], C1[, k + 1],
                                       sigma, task = paste0("<=", k))
    }
    structure(.poly_wmean(comp$omega, comp$weight), components = comp)
}

## Task weights. Nk are the category counts; pr is the 2 x choose(K, 2) matrix
## of unordered pairs (k, k'), coded from 0.
##   continuity  omega_c: N_k N_k'.  omega_t, cut k: n_lo (N - n_lo) (N_k + N_k+1) / N,
##               n_lo = N(x <= k).
##   legacy      omega_t: N_k. omega_c: the old two-stage mean (pairs within category k
##               weighted by N_kk' = N_k + N_k', then categories weighted by N_k), written
##               as one weight per pair, w = N_kk' (N_k / D_k + N_k' / D_k'), where
##               D_k sums N_kk'' over the categories k'' != k with responses (the pairs
##               whose task is defined).
.weights_c <- function(Nk, pr, weights = "continuity") {
    a <- Nk[pr[1, ] + 1]; b <- Nk[pr[2, ] + 1]
    if (weights == "continuity") return(a * b)
    Nkk <- a + b
    D   <- vapply(seq_along(Nk) - 1L, function(k) {
        on <- (pr[1, ] == k | pr[2, ] == k) & a > 0 & b > 0
        sum(Nkk[on])
    }, 0)
    share <- function(n, d) ifelse(d > 0, n / d, 0)
    Nkk * (share(a, D[pr[1, ] + 1]) + share(b, D[pr[2, ] + 1]))
}

.weights_t <- function(Nk, weights = "continuity") {
    K <- length(Nk)
    if (weights == "legacy") return(Nk[-K])
    N   <- sum(Nk)
    nlo <- cumsum(Nk)[seq_len(K - 1)]
    nlo * (N - nlo) * (Nk[-K] + Nk[-1]) / N
}

## Deprecation of weights = "legacy": one classed warning per session.
.imv_state <- new.env(parent = emptyenv())

.warn_legacy_weights <- function() {
    if (isTRUE(.imv_state$legacy_warned)) return(invisible())
    .imv_state$legacy_warned <- TRUE
    msg <- paste0('weights = "legacy" is deprecated and will be removed in a future ',
                  'release; use weights = "continuity" (the default).')
    warning(structure(list(message = msg, call = NULL),
                      class = c("imv_deprecated", "warning", "condition")))
}

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
.imv_task <- function(y, p0, p1, sigma, task) {
    if (length(y) == 0 || all(y == y[1])) return(NA_real_)
    .tag_out_of_domain(imv.binary(y, p0, p1, sigma = sigma), task = task)
}

## weighted mean over tasks with a defined IMV and positive weight
.poly_wmean <- function(v, w) {
    ok <- !is.na(v) & w > 0
    if (!any(ok)) return(NA_real_)
    sum(w[ok] * v[ok]) / sum(w[ok])
}

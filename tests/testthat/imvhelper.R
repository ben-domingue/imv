imv <- function(pr, p1, p2, eps = 1e-6) {
  pr$pv1 <- pr[[p1]]
  pr$pv2 <- pr[[p2]]
  pr$pv1 <- ifelse(pr$pv1 < eps, eps, pr$pv1)
  pr$pv2 <- ifelse(pr$pv2 < eps, eps, pr$pv2)
  pr$pv1 <- ifelse(pr$pv1 > 1 - eps, 1 - eps, pr$pv1)
  pr$pv2 <- ifelse(pr$pv2 > 1 - eps, 1 - eps, pr$pv2)
  ##
  ll <- function(x, p = 'pv') {
    z <- log(x[[p]]) * x$resp + log(1 - x[[p]]) * (1 - x$resp)
    z <- sum(z) / nrow(x)
    exp(z)
  }
  loglik1 <- ll(pr, 'pv1')
  loglik2 <- ll(pr, 'pv2')
  getcoins <- function(a) {
    f <- function(p, a) abs(p * log(p) + (1 - p) * log(1 - p) - log(a))
    nlminb(.5, f, lower = 0.001, upper = .999, a = a)$par
  }
  c1 <- getcoins(loglik1)
  c2 <- getcoins(loglik2)
  ew <- function(p1, p0) (p1 - p0) / p0
  imv <- ew(c2, c1)
  return(imv)
}

imv_c <- function(y, pctt.tab, p1, p2) {
  nn <- length(unique(y$resp))
  om <- numeric()
  iis <- 0:(nn - 1)
  for (ii in iis) {
    ns <- om.tmp <- numeric()
    jjs <- iis[-match(ii, iis)]
    for (jj in jjs) {
      y2 <- y[y$resp %in% c(ii, jj), ]
      resp <- ifelse(y2$resp == ii, 1, 0)
      ##irt p-values for being
      p1.ii <- y2[[paste(p1, ii, sep = '')]]
      p1.jj <- y2[[paste(p1, jj, sep = '')]]
      p2.ii <- y2[[paste(p2, ii, sep = '')]]
      p2.jj <- y2[[paste(p2, jj, sep = '')]]
      ##
      z <- data.frame(
        resp = resp,
        p1 = p1.ii / (p1.ii + p1.jj),
        p2 = p2.ii / (p2.ii + p2.jj)
      )
      j0 <- as.character(jj)
      om.tmp[j0] <- imv(z, p1 = "p1", p2 = "p2")
      ns[as.character(jj)] <- nrow(z)
    }
    om[ii + 1] <- sum(om.tmp * ns) / sum(ns)
  }
  print('om')
  print(om)
  print('pctt.tab')
  print(pctt.tab)
  omega_c <- sum(om * pctt.tab) / sum(pctt.tab)
  return(omega_c)
}


imv_t <- function(y, pctt.tab, p1, p2) {
  nn <- length(unique(y$resp))
  om <- numeric()
  for (ii in 0:(nn - 2)) {
    resp <- ifelse(y$resp <= ii, 1, 0)
    ##irt p-values for being below ii
    pr1 <- rowSums(y[, paste(p1, 0:ii, sep = ''), drop = FALSE])
    pr2 <- rowSums(y[, paste(p2, 0:ii, sep = ''), drop = FALSE])
    z <- data.frame(resp = resp, p1 = pr1, p2 = pr2)
    om[ii + 1] <- imv(z, p1 = "p1", p2 = "p2")
  }
  ##
  pctt.tab <- pctt.tab[1:(nn - 1)] / (1 - pctt.tab[nn])
  ##
  omega_t <- sum(om * pctt.tab) / sum(pctt.tab)
  print('om')
  print(om)
  print('pctt.tab')
  print(pctt.tab)
  return(omega_t)
}

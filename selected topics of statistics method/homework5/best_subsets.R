normalize = function(x) {
  m <- ncol(x)
  for (i in 1:m) {
    x[, i] <- (x[, i] - mean(x[, i])) / sd(x[, i])
  }
  return(x)
}

centerlize = function(x) {
  x <- x - mean(x)
  return(x)
}


best_subset = function(x, y) {
  m <- ncol(x)
  n <- nrow(x)
  p <- matrix(rep(0,m*(2^m-1)),nrow = m, ncol = 2 ^ m-1 )
  beta <- matrix(rep(0,m*(2^m-1)),nrow = m, ncol = 2 ^ m-1  )
  sigma <- rep(0, 2 ^ m -1 )
  lambda <- 0
  x <- normalize(x)
  y <- centerlize(y)
  for (k in 1:(m-1)) {
      for (i in 1:choose(m, k)) {
        lambda <- lambda + 1
        p[1:k, lambda] <- combn(m, k)[, i]
        x1 <- x[, combn(m, k)[, i]]
        beta[1:k, lambda] <- (solve(t(x1) %*% x1)) %*% (t(x1)) %*% y
        sigma[lambda] <- sum((y - x1 %*% as.matrix(beta[1:k , lambda])) ^ 2)
      }
    }
  a <- which(sigma == min(sigma[which(sigma != 0)]))
  # print(p[which(!is.na(p[, a])), a])
  return(beta[which(!is.na(beta[, a])), a])
}



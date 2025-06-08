cvm.test <- function(x,y, type=c("W2", "U2", "A2"),
                    simulate.p.value=FALSE,
                    B=2000, tol=1e-8) {

  cvm.pval.disc <- function(STAT, lambda) {

    x <- STAT

    theta <- function(u) {
      VAL <- 0
      for(i in 1:length(lambda)) {
         VAL <- VAL + 0.5*atan(lambda[i]*u)
      }
      return(VAL - 0.5*x*u)
    }

    rho <- function(u) {
      VAL <- 0
      for(i in 1:length(lambda)) {
        VAL <- VAL + log(1 + lambda[i]^2*u^2)
      }
      VAL <- exp(VAL*0.25)
      return(VAL)
    }

    fun <- function(u) return(sin(theta(u))/(u*rho(u)))

    pval <- 0
    try(pval <- 0.5 + integrate(fun, 0, Inf, subdivisions=1e6)$value/pi,
        silent=TRUE)
    if(pval > 0.001) return(pval)
    if(pval <= 0.001) {
      df <- sum(lambda != 0)
      est1 <- dchisq(STAT/max(lambda),df)
      logf <- function(t) {
        ans <- -t*STAT
        ans <- ans - 0.5*sum( log(1-2*t*lambda) )
        return(ans)
      }
      est2 <- 1
      try( est2 <- exp(nlm(logf, 1/(4*max(lambda)))$minimum), silent=TRUE)
      return(min(est1,est2))
    }
  } # End cvm.pval.disc()

  cvm.stat.disc <- function(x,y, type=c("W2", "U2", "A2")) {
    type <- match.arg(type)
    I <- knots(y)
    N <- length(x)
    e <- diff(c(0,N*y(I)))
    obs <- rep(0, length(I))
    for(j in 1:length(I)) {
      obs[j] <- length(which(x == I[j]))
    }
    S <- cumsum(obs)
    T <- cumsum(e)
    H <- T/N
    p <- e/N
    t <- (p + p[c(2:length(p), 1)])/2
    Z <- S - T
    Zbar <- sum(Z*t)

    S0 <- diag(p) - p %*% t(p)
    A <- matrix(1, length(p), length(p))
    A <- apply(row(A) >= col(A),2, as.numeric)
    E <- diag(t)
    One <- rep(1, nrow(E))
    K <- diag(0, length(H))
    diag(K)[-length(H)] <- 1/(H[-length(H)]*(1-H[-length(H)]))
    Sy <- A %*% S0 %*% t(A)
    M <- switch(type, W2 = E,
                U2 = (diag(1, nrow(E)) -
                      E%*%One%*%t(One))%*%E%*%(diag(1, nrow(E)) -
                      One%*%t(One)%*%E),
                      A2 = E%*%K)
    lambda <- eigen(M%*%Sy)$values

    STAT <- switch(type, W2 = sum(Z^2*t)/N, U2 = sum((Z-Zbar)^2*t)/N,
                   A2 = sum((Z^2*t/(H*(1-H) ))[-length(I)])/N)

    return(c(STAT, lambda))
  } # End cvm.stat.disc()


  cvm.pval.disc.sim <-  function(STATISTIC, lambda, y, type, tol, B) {
    # Simulate B samples from given stepfun y
    knots.y <- knots(y)
    fknots.y <- y(knots.y)
    u <- runif(B*length(x))
    u <- sapply(u, function(a) return(knots.y[sum(a>fknots.y)+1]))
    dim(u) <- c(B, length(x))

    # Calculate B values of the test statistic
    s <- apply(u, 1, cvm.stat.disc, y, type)
    s <- s[1,]
    # Produce the estimated p-value
    return(sum(s >= STATISTIC-tol) / B)
  }

  type <- match.arg(type)
  DNAME <- deparse(substitute(x))
  if(is.stepfun(y)) {
    if(length(setdiff(x, knots(y))) != 0) {
      stop("Data are incompatable with null distribution; ",
           "Note: This function is meant only for discrete distributions ",
           "you may be receiving this error because y is continuous.")
    }
    tempout <- cvm.stat.disc(x,y,type=type)
    STAT <- tempout[1]
    lambda <- tempout[2:length(tempout)]
    if(!simulate.p.value) {
      PVAL <- cvm.pval.disc(STAT, lambda)
    } else {
      PVAL <- cvm.pval.disc.sim(STAT, lambda, y, type, tol, B)
    }
    METHOD <- paste("Cramer-von Mises -", type)
    names(STAT) <- as.character(type)
    RVAL <- list(statistic = STAT, p.value = PVAL, alternative = "Two.sided",
                 method = METHOD, data.name=DNAME)
  } else {
    stop('Null distribution must be a discrete.')
  }


  class(RVAL) <- "htest"
  return(RVAL)
}

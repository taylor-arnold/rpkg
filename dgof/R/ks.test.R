#  Original file .../src/library/stats/R/ks.test.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/
#
#  Modification by Taylor Arnold and Jay Emerson, 2010:
#
#  Any changes to the original R Core source is noted by
#  the following comment convension to aid code review:
#
#   ####################
#   ## TBA, JWE: comment
#
#   ## END TBA, JWE
#   ###############



ks.test <- function(x, y, ...,
                    alternative = c("two.sided", "less", "greater"),
                    exact = NULL,

    #########################################################################
    ## TBA, JWE: an additional argument, tol, is used as an upper bound for
    ## possible rounding error in values (say, a and b) when needing to check
    ## for equality (a==b).  We also use it in establishing a small epsilon
    ## for some other calculations involving the step functions.

    ## Additional arguments, simulate.p.value and B, follow the usage of
    ## fisher.test, but are only used for the discrete distribution case.

                    tol=1e-8,
                    simulate.p.value=FALSE, B=2000)

    ## END TBA, JWE
    ################

{

    pkolmogorov1x <- function(x, n) {
        ## Probability function for the one-sided one-sample Kolmogorov
        ## statistics, based on the formula of Birnbaum & Tingey (1951).
        if(x <= 0) return(0)
        if(x >= 1) return(1)
        j <- seq.int(from = 0, to = floor(n * (1 - x)))
        1 - x * sum(exp(lchoose(n, j)
                        + (n - j) * log(1 - x - j / n)
                        + (j - 1) * log(x + j / n)))
    }

    #############################################################
    ## TBA, JWE: exact.pval is a new function implementing the
    ## p-value calculations presented in Conover (1972) and
    ## Gleser (1985), which are cited in our revision of
    ## ks.test.Rd.

    exact.pval <-  function(alternative, STATISTIC, x, n, y, knots.y, tol) {

        ts.pval <- function(S, x, n, y, knots.y, tol) {
            # The exact two-sided p-value from Gleser (1985)
            # and Niederhausen (1981)
            f_n <- ecdf(x)
            eps <- min(tol, min(diff(knots.y)) * tol)
            eps2 <- min(tol, min(diff(y(knots.y))) * tol)
            a <- rep(0, n); b <- a; f_a <- a
            for (i in 1:n) {
                a[i] <- min(c(knots.y[which(y(knots.y) + S >= i/n + eps2)[1]],
                              Inf), na.rm=TRUE)
                b[i] <- min(c(knots.y[which(y(knots.y) - S > (i-1)/n - eps2)[1]],
                              Inf), na.rm=TRUE)
                # Calculate F(a_i-)
                # If a_i is not a knot of F_0,
                # then simply return the value of F_0(a_i).
                # Otherwise, evaluate F_0(a_i - eps)
                f_a[i] <- ifelse(!(a[i] %in% knots.y),
                                 y(a[i]), y(a[i]-eps))
            }
            f_b <- y(b)

            # NOW HAVE f_a and f_b which are Niederhausen u, v, and this
            # uses the Noe result.

            p <- rep(1, n+1)
            for (i in 1:n) {
                tmp <- 0
                for (k in 0:(i-1)) {
                    tmp <- tmp + choose(i, k) * (-1)^(i-k-1) *
                                 max(f_b[k+1] - f_a[i], 0)^(i-k) * p[k+1]
                }
                p[i+1] <- tmp
            }
            p <- max(0, 1 - p[n+1])          # Niederhausen result
            if (p>1) {
                warning("numerical instability in p-value calculation.")
                p <- 1
            }
            return(p)
        }

        # Conover
        less.pval <- function(S, n, H, z, tol) {
            m <- ceiling(n*(1-S))
            c <- S+(1:m-1)/n
            CDFVAL <- H(sort(z))
            for(j in 1:length(c)) {
                ifelse( (min(abs(c[j] - CDFVAL)) < tol),
                        c[j] <- 1-c[j],
                        c[j] <- 1-CDFVAL[which(order(c(c[j], CDFVAL)) == 1 )] )
            }
            b <- rep(0, m)
            b[1] <- 1
            for(k in 1:(m-1))
                b[k+1] <- 1 - sum(choose(k,1:k-1)*c[1:k]^(k-1:k+1)*b[1:k])
            p <- sum(choose(n, 0:(m-1))*c^(n-0:(m-1))*b )
            return(p)
        }

        # Conover
        greater.pval <- function(S, n, H, z, tol) {
            m <- ceiling(n*(1-S))
            c <- 1-(S+(1:m-1)/n)
            CDFVAL <- c(0,H(sort(z)))
            for(j in 1:length(c)) {
                if( !(min(abs(c[j] - CDFVAL)) < tol) )
                   c[j] <- CDFVAL[which(order(c(c[j], CDFVAL)) == 1 ) - 1]
            }
            b <- rep(0, m)
            b[1] <- 1
            for(k in 1:(m-1))
                b[k+1] <- 1 - sum(choose(k,1:k-1)*c[1:k]^(k-1:k+1)*b[1:k])
            p <- sum(choose(n, 0:(m-1))*c^(n-0:(m-1))*b )
            return(p)
        }

        p <- switch(alternative,
                    "two.sided" = ts.pval(STATISTIC, x, n, y, knots.y, tol),
                    "less" = less.pval(STATISTIC, n, y, knots.y, tol),
                    "greater" = greater.pval(STATISTIC, n, y, knots.y, tol))
        return(p)
    }

    sim.pval <-  function(alternative, STATISTIC, x, n, y, knots.y, tol, B) {
        # Simulate B samples from given stepfun y
        fknots.y <- y(knots.y)
        u <- runif(B*length(x))
        u <- sapply(u, function(a) return(knots.y[sum(a>fknots.y)+1]))
        dim(u) <- c(B, length(x))

        # Calculate B values of the test statistic
        getks <- function(a, knots.y, fknots.y) {
            dev <- c(0, ecdf(a)(knots.y) - fknots.y)
            STATISTIC <- switch(alternative,
                              "two.sided" = max(abs(dev)),
                              "greater" = max(dev),
                              "less" = max(-dev))
            return(STATISTIC)
        }
        s <- apply(u, 1, getks, knots.y, fknots.y)

        # Produce the estimated p-value
        return(sum(s >= STATISTIC-tol) / B)
    }

    ## END TBA, JWE
    ###############

    alternative <- match.arg(alternative)
    DNAME <- deparse(substitute(x))
    x <- x[!is.na(x)]
    n <- length(x)
    if(n < 1L)
        stop("not enough 'x' data")
    PVAL <- NULL

    if(is.numeric(y)) {
        DNAME <- paste(DNAME, "and", deparse(substitute(y)))
        y <- y[!is.na(y)]
        n.x <- as.double(n)             # to avoid integer overflow
        n.y <- length(y)
        if(n.y < 1L)
            stop("not enough 'y' data")
        if(is.null(exact))
            exact <- (n.x * n.y < 10000)
        METHOD <- "Two-sample Kolmogorov-Smirnov test"
        TIES <- FALSE
        n <- n.x * n.y / (n.x + n.y)
        w <- c(x, y)
        z <- cumsum(ifelse(order(w) <= n.x, 1 / n.x, - 1 / n.y))
        if(length(unique(w)) < (n.x + n.y)) {
            warning("cannot compute correct p-values with ties")
            z <- z[c(which(diff(sort(w)) != 0), n.x + n.y)]
            TIES <- TRUE
        }
        STATISTIC <- switch(alternative,
                            "two.sided" = max(abs(z)),
                            "greater" = max(z),
                            "less" = - min(z))
        nm_alternative <- switch(alternative,
                                 "two.sided" = "two-sided",
                                 "less" = "the CDF of x lies below that of y",
                                 "greater" = "the CDF of x lies above that of y")
        if(exact && (alternative == "two.sided") && !TIES)
            PVAL <- 1 - .C(C_psmirnov2x,
                           p = as.double(STATISTIC),
                           as.integer(n.x),
                           as.integer(n.y),
                           PACKAGE="dgof")$p

    #################################################################
    ## TBA, JWE: the following else implements behavior relating to
    ## a new possibility for y: an object of class 'stepfun', of
    ## which 'ecdf' is a subclass we expect most users to have.

    ## Don't recalculate knots(z) more than necessary.

    } else if (is.stepfun(y)) {
        z <- knots(y)

    ## JE NOTE: could allow larger n for two-sided case???
    ## revisit.

        if(is.null(exact)) exact <- (n <= 30)
        if (exact && n>30) {
            warning("numerical instability may affect p-value")
        }
        METHOD <- "One-sample Kolmogorov-Smirnov test"
        dev <- c(0, ecdf(x)(z) - y(z))
        STATISTIC <- switch(alternative,
                            "two.sided" = max(abs(dev)),
                            "greater" = max(dev),
                            "less" = max(-dev))
        if (simulate.p.value) {
            PVAL <- sim.pval(alternative, STATISTIC,
                             x, n, y, z, tol, B)
        } else {
            PVAL <- switch(exact,
                           "TRUE" = exact.pval(alternative, STATISTIC,
                                               x, n, y, z, tol),
                           "FALSE" = NULL)
        }
        nm_alternative <- switch(alternative,
            "two.sided" = "two-sided",
            "less" = "the CDF of x lies below the null hypothesis",
            "greater" = "the CDF of x lies above the null hypothesis")

        ## END TBA, JWE
        ###############

    } else {
        if(is.character(y))
            y <- get(y, mode="function")
        if(mode(y) != "function")
            stop("'y' must be numeric or a string naming a valid function")
        if(is.null(exact))
            exact <- (n < 100)
        METHOD <- "One-sample Kolmogorov-Smirnov test"
        TIES <- FALSE
        if(length(unique(x)) < n) {

            ##########################################################
            ## TBA, JWE: the following error message has been changed.

            warning(paste("default ks.test() cannot compute correct p-values with ties;\n",
                "see help page for one-sample Kolmogorov test for discrete distributions."))

            ## END TBA, JWE
            ###############

            TIES <- TRUE
        }
        x <- y(sort(x), ...) - (0 : (n-1)) / n
        STATISTIC <- switch(alternative,
                            "two.sided" = max(c(x, 1/n - x)),
                            "greater" = max(1/n - x),
                            "less" = max(x))
        if(exact && !TIES) {
            PVAL <- if(alternative == "two.sided")
                1 - .C(C_pkolmogorov2x,
                           p = as.double(STATISTIC),
                           as.integer(n),
                           PACKAGE="dgof")$p
            else
                1 - pkolmogorov1x(STATISTIC, n)
        }
        nm_alternative <- switch(alternative,
                                 "two.sided" = "two-sided",
                                 "less" = "the CDF of x lies below the null hypothesis",
                                 "greater" = "the CDF of x lies above the null hypothesis")

    }

    names(STATISTIC) <- switch(alternative,
                               "two.sided" = "D",
                               "greater" = "D^+",
                               "less" = "D^-")

    pkstwo <- function(x, tol = 1e-6) {
        ## Compute \sum_{-\infty}^\infty (-1)^k e^{-2k^2x^2}
        ## Not really needed at this generality for computing a single
        ## asymptotic p-value as below.
        if(is.numeric(x))
            x <- as.vector(x)
        else
            stop("argument 'x' must be numeric")
        p <- rep(0, length(x))
        p[is.na(x)] <- NA
        IND <- which(!is.na(x) & (x > 0))
        if(length(IND)) {
            p[IND] <- .C(C_pkstwo,
                         as.integer(length(x[IND])),
                         p = as.double(x[IND]),
                         as.double(tol),
                         PACKAGE="dgof")$p
        }
        return(p)
    }

    if(is.null(PVAL)) {
        ## <FIXME>
        ## Currently, p-values for the two-sided two-sample case are
        ## exact if n.x * n.y < 10000 (unless controlled explicitly).
        ## In all other cases, the asymptotic distribution is used
        ## directly.  But: let m and n be the min and max of the sample
        ## sizes, respectively.  Then, according to Kim and Jennrich
        ## (1973), if m < n / 10, we should use the
        ## * Kolmogorov approximation with c.c. -1/(2*n) if 1 < m < 80;
        ## * Smirnov approximation with c.c. 1/(2*sqrt(n)) if m >= 80.
        PVAL <- ifelse(alternative == "two.sided",
                       1 - pkstwo(sqrt(n) * STATISTIC),
                       exp(- 2 * n * STATISTIC^2))
        ## </FIXME>
    }

    RVAL <- list(statistic = STATISTIC,
                 p.value = PVAL,
                 alternative = nm_alternative,
                 method = METHOD,
                 data.name = DNAME)
    class(RVAL) <- "htest"
    return(RVAL)
}

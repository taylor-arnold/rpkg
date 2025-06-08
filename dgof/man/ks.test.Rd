% Original file src/library/stats/man/ks.test.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2008 R Core Development Team
% Distributed under GPL 2 or later
%
% Modification by Taylor Arnold and Jay Emerson, 2010:
%
% Any changes to the original R Core source is noted by
% the following comment convension to aid review:
%
%   ####################
%   ## TBA, JWE: comment
%
%   ## END TBA, JWE
%   ###############

\name{ks.test}
\alias{ks.test}
\title{Kolmogorov-Smirnov Tests}
\description{
  Performs one or two sample Kolmogorov-Smirnov tests.
}
\usage{

%   #########################################################################
%   ## TBA, JWE: an additional argument, tol, is used as an upper bound for
%   ## possible rounding error in values (say, a and b) when needing to check
%   ## for equality (a==b).  We also add simulate.p.value and B, following
%   ## the usage in fisher.test(), for example

ks.test(x, y, \dots,
        alternative = c("two.sided", "less", "greater"),
        exact = NULL, tol=1e-8, simulate.p.value=FALSE, B=2000)

%   ## END TBA, JWE
%   ###############

}
\arguments{
  \item{x}{a numeric vector of data values.}

  % ########################################
  % TBA, JWE: modified argument description.

  \item{y}{a numeric vector of data values, or a character string
    naming a cumulative distribution function or an actual cumulative
    distribution function such as \code{pnorm}. Alternatively, \code{y}
    can be an \code{\link{ecdf}} function (or an object of class
    \code{\link{stepfun}}) for specifying a discrete distribution.}

  % END TBA, JWE
  % ############

  \item{\dots}{parameters of the distribution specified (as a character
    string) by \code{y}.}
  \item{alternative}{indicates the alternative hypothesis and must be
    one of \code{"two.sided"} (default), \code{"less"}, or
    \code{"greater"}.  You can specify just the initial letter of the
    value, but the argument name must be give in full.
    See \sQuote{Details} for the meanings of the possible values.}
  \item{exact}{\code{NULL} or a logical indicating whether an exact
    p-value should be computed.  See \sQuote{Details} for the meaning of
    \code{NULL}.  Not used for the one-sided two-sample case.}

  % ######################
  % TBA, JWE: new argument

  \item{tol}{used as an upper bound for possible rounding error in values
    (say, \code{a} and \code{b}) when needing to check for equality
    (\code{a==b}); value of \code{NA} or \code{0} does exact comparisons
    but risks making errors due to numerical imprecisions.}
  \item{simulate.p.value}{a logical indicating whether to compute
    p-values by Monte Carlo simulation, for discrete goodness-of-fit
    tests only.}
  \item{B}{an integer specifying the number of replicates used in the
    Monte Carlo test (for discrete goodness-of-fit tests only).}

  % END TBA, JWE
  % ############

}
\details{
  If \code{y} is numeric, a two-sample test of the null hypothesis
  that \code{x} and \code{y} were drawn from the same \emph{continuous}
  distribution is performed.

  % #####################
  % TBA, JWE: new details
  Alternatively, \code{y} can be a character string naming a continuous
  (cumulative) distribution function (or such a function),
  or an \code{\link{ecdf}} function
  (or object of class \code{stepfun}) giving a discrete distribution.  In
  these cases, a one-sample test is carried  out of the null that the
  distribution function which generated \code{x} is distribution \code{y}
  with parameters specified by \code{\dots}.

  The presence of ties generates a warning unless \code{y} describes a discrete
  distribution (see above), since continuous distributions do not generate them.
  % END TBA, JWE
  % ############

  The possible values \code{"two.sided"}, \code{"less"} and
  \code{"greater"} of \code{alternative} specify the null hypothesis
  that the true distribution function of \code{x} is equal to, not less
  than or not greater than the hypothesized distribution function
  (one-sample case) or the distribution function of \code{y} (two-sample
  case), respectively.  This is a comparison of cumulative distribution
  functions, and the test statistic is the maximum difference in value,
  with the statistic in the \code{"greater"} alternative being
  \eqn{D^+ = \max_u [ F_x(u) - F_y(u) ]}{D^+ = max[F_x(u) - F_y(u)]}.
  Thus in the two-sample case
  \code{alternative="greater"} includes distributions for which \code{x}
  is stochastically \emph{smaller} than \code{y} (the CDF of \code{x} lies
  above and hence to the left of that for \code{y}), in contrast to
  \code{\link{t.test}} or \code{\link{wilcox.test}}.

  %% ##########################
  %% TBA, JWE added and edited:
  Exact p-values are not available for the one-sided two-sample case,
  or in the case of ties if \code{y} is continuous.  If \code{exact = NULL}
  (the default), an exact p-value is computed if the sample size is less
  than 100 in the one-sample case with \code{y} continuous or if the sample
  size is less than or equal to 30 with \code{y}
  discrete; or if the product of the
  sample sizes is less than 10000 in the two-sample case for continuous
  \code{y}.  Otherwise,
  asymptotic distributions are used whose approximations may be inaccurate
  in small samples.  With \code{y} continuous,
  the one-sample two-sided case, exact p-values are
  obtained as described in Marsaglia, Tsang & Wang (2003); the formula of
  Birnbaum & Tingey (1951) is used for the one-sample one-sided case.

  In the one-sample case with \code{y} discrete, the methods presented in
  Conover (1972) and Gleser (1985) are used when \code{exact=TRUE} (or when
  \code{exact=NULL}) and \code{length(x)<=30} as described above.
  When \code{exact=FALSE} or \code{exact=NULL} with
  \code{length(x)>30}, the test is not exact and the resulting p-values
  are known to be conservative.  Usage of \code{exact=TRUE} with
  sample sizes greater than 30 is not advised due to numerical instabilities;
  in such cases, simulated p-values may be desirable.

  If a single-sample test is used with \code{y} continuous,
  the parameters specified in
  \code{\dots} must be pre-specified and not estimated from the data.
  There is some more refined distribution theory for the KS test with
  estimated parameters (see Durbin, 1973), but that is not implemented
  in \code{ks.test}.
  %% END TBA, JWE
  %% ############
}
\value{
  A list with class \code{"htest"} containing the following components:
  \item{statistic}{the value of the test statistic.}
  \item{p.value}{the p-value of the test.}
  \item{alternative}{a character string describing the alternative
    hypothesis.}
  \item{method}{a character string indicating what type of test was
    performed.}
  \item{data.name}{a character string giving the name(s) of the data.}
}
\author{
Modified by Taylor B. Arnold and John W. Emerson to include
one-sample testing with a discrete distribution (as presented
in Conover's 1972 paper -- see references).
}
\references{
  Z. W. Birnbaum and Fred H. Tingey (1951),
  One-sided confidence contours for probability distribution functions.
  \emph{The Annals of Mathematical Statistics}, \bold{22}/4, 592--596.

  William J. Conover (1971),
  \emph{Practical Nonparametric Statistics}.
  New York: John Wiley & Sons.
  Pages 295--301 (one-sample Kolmogorov test),
  309--314 (two-sample Smirnov test).

  %% ###############
  %% TBA, JWE added:
  William J. Conover (1972),
  A Kolmogorov Goodness-of-Fit Test for Discontinuous Distributions.
  \emph{Journal of American Statistical Association}, Vol. 67, No. 339, 591--596.

  Leon Jay Gleser (1985),
  Exact Power of Goodness-of-Fit Tests of Kolmogorov Type for Discontinuous
  Distributions.
  \emph{Journal of American Statistical Association}, Vol. 80, No. 392,  954--958.
  %% END TBA, JWE
  %% ############

  Durbin, J. (1973)
  \emph{Distribution theory for tests based on the sample distribution
    function}.  SIAM.

  George Marsaglia, Wai Wan Tsang and Jingbo Wang (2003),
  Evaluating Kolmogorov's distribution.
  \emph{Journal of Statistical Software}, \bold{8}/18.
  \url{https://www.jstatsoft.org/v08/i18/}.

}
%% ########################
%% TBA, JWE added cvm.test:
\seealso{
  \code{\link{shapiro.test}} which performs the Shapiro-Wilk test for
  normality; \code{\link{cvm.test}} for Cramer-von Mises type tests.
}
%% END TBA, JWE
%% ############
\examples{
require(graphics)
require(dgof)

set.seed(1)

x <- rnorm(50)
y <- runif(30)
# Do x and y come from the same distribution?
ks.test(x, y)
# Does x come from a shifted gamma distribution with shape 3 and rate 2?
ks.test(x+2, "pgamma", 3, 2) # two-sided, exact
ks.test(x+2, "pgamma", 3, 2, exact = FALSE)
ks.test(x+2, "pgamma", 3, 2, alternative = "gr")

# test if x is stochastically larger than x2
x2 <- rnorm(50, -1)
plot(ecdf(x), xlim=range(c(x, x2)))
plot(ecdf(x2), add=TRUE, lty="dashed")
t.test(x, x2, alternative="g")
wilcox.test(x, x2, alternative="g")
ks.test(x, x2, alternative="l")

#########################################################
# TBA, JWE new examples added for discrete distributions:

x3 <- sample(1:10, 25, replace=TRUE)

# Using ecdf() to specify a discrete distribution:
ks.test(x3, ecdf(1:10))

# Using step() to specify the same discrete distribution:
myfun <- stepfun(1:10, cumsum(c(0, rep(0.1, 10))))
ks.test(x3, myfun)

# The previous R ks.test() does not correctly calculate the
# test statistic for discrete distributions (gives warning):
# stats::ks.test(c(0, 1), ecdf(c(0, 1)))
# ks.test(c(0, 1), ecdf(c(0, 1)))

# Even when the correct test statistic is given, the
# previous R ks.test() gives conservative p-values:
stats::ks.test(rep(1, 3), ecdf(1:3))
ks.test(rep(1, 3), ecdf(1:3))
ks.test(rep(1, 3), ecdf(1:3), simulate=TRUE, B=10000)

}
\keyword{htest}

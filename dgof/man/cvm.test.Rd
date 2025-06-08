\name{cvm.test}
\alias{cvm.test}
\title{Discrete Cramer-von Mises Goodness-of-Fit Tests}
\description{
Computes the test statistics for doing one-sample Cramer-von Mises goodness-of-fit tests
and calculates asymptotic p-values.
}
\usage{
cvm.test(x, y, type = c("W2", "U2", "A2"),
         simulate.p.value=FALSE, B=2000, tol=1e-8)
}
\arguments{
  \item{x}{a numerical vector of data values.}
  \item{y}{an \code{\link{ecdf}} or step-function (\code{\link{stepfun}}) for specifying the hypothesized model.}
  \item{type}{the variant of the Cramer-von Mises test; \code{"W2"} is the default and most common method, \code{"U2"} is for cyclical data, and \code{"A2"} is the Anderson-Darling alternative. For details see references.}
  \item{simulate.p.value}{a logical indicating whether to compute
    p-values by Monte Carlo simulation.}
  \item{B}{an integer specifying the number of replicates used in the
    Monte Carlo test (for discrete goodness-of-fit tests only).}
  \item{tol}{used as an upper bound for possible rounding error in values
    (say, \code{a} and \code{b}) when needing to check for equality
    (\code{a==b}) (for discrete goodness-of-fit tests only).}
}
\details{
While the Kolmogorov-Smirnov test may be the most
popular of the nonparametric goodness-of-fit tests,
Cramer-von Mises tests have been shown to be more
powerful against a large class of alternatives hypotheses. 
The original test was developed by Harald
Cramer and Richard von Mises (Cramer, 1928; von
Mises, 1928) and further adapted by Anderson and
Darling (1952), and Watson (1961).
}
\value{
An object of class \code{htest}.
%%  ~Describe the value returned
%%  If it is a LIST, use
%% \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
T. W. Anderson and D. A. Darling (1952). \emph{Asymptotic
theory of certain "goodness of fit" criteria based on stochastic
processes.} Annals of Mathematical Statistics, 23:193-212.

V. Choulakian, R. A. Lockhart, and M. A. Stephens (1994).
\emph{Cramer-von Mises statistics for discrete distributions}.
The Canadian Journal of Statistics, 22(1): 125-137.

H. Cramer (1928).
\emph{On the composition of elementary errors.}
Skand. Akt., 11:141-180.

M. A. Stephens (1974).
\emph{Edf statistics for goodness of fit and some comparisons}.
Journal of the American Statistical Association, 69(347): 730-737.

R. E. von Mises (1928).
\emph{Wahrscheinlichkeit, Statistik und Wahrheit.}
Julius Springer, Vienna, Austria.

G. S. Watson (1961).  \emph{Goodness of fit tests on the circle.}
Biometrika, 48:109-114.
}
\author{
Taylor B. Arnold and John W. Emerson

Maintainer: Taylor B. Arnold <taylor.arnold@yale.edu>
}
\note{
Additional notes?
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{ks.test}}, \code{\link{ecdf}}, \code{\link{stepfun}}
}
\examples{
require(dgof)

x3 <- sample(1:10, 25, replace=TRUE)

# Using ecdf() to specify a discrete distribution:
ks.test(x3, ecdf(1:10))
cvm.test(x3, ecdf(1:10))

# Using step() to specify the same discrete distribution:
myfun <- stepfun(1:10, cumsum(c(0, rep(0.1, 10))))
ks.test(x3, myfun)
cvm.test(x3, myfun)

# Usage of U2 for cyclical distributions (note U2 unchanged, but W2 not)

set.seed(1)
y <- sample(1:4, 20, replace=TRUE)
cvm.test(y, ecdf(1:4), type='W2')
cvm.test(y, ecdf(1:4), type='U2')
z <- y%%4 + 1
cvm.test(z, ecdf(1:4), type='W2')
cvm.test(z, ecdf(1:4), type = 'U2')

# Compare analytic results to simulation results
set.seed(1)
y <- sample(1:3, 10, replace=TRUE)
cvm.test(y, ecdf(1:6), simulate.p.value=FALSE)
cvm.test(y, ecdf(1:6), simulate.p.value=TRUE)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{htest}

\name{EM.Fuzzy-package}
\alias{EM.Fuzzy-package}
\alias{EM.Fuzzy}
\docType{package}
\title{
\packageTitle{EM.Fuzzy}
}
\description{
The main goal of this package is easy estimation of the unknown parameter of a continues distribution by EM algorithm where the observed data are fuzzy rather than crisp. This package contains two major functions: (1) the function \code{EM.Triangular} works by Triangular Fuzzy Numbers (TFNs), and (2) the function \code{EM.Trapezoidal} works by Trapezoidal Fuzzy Numbers (TrFNs).
}

\author{
Abbas Parchami
}

\references{
Denoeux, T. (2011) Maximum likelihood estimation from fuzzy data using the EM algorithm, Fuzzy Sets and Systems 183, 72-91.

Gagolewski, M., Caha, J. (2015) FuzzyNumbers Package: Tools to deal with fuzzy numbers in R. R package version 0.4-1, https://cran.r-project.org/web/packages=FuzzyNumbers

Gagolewski, M., Caha, J. (2015) A guide to the FuzzyNumbers package for R (FuzzyNumbers version 0.4-1) http://FuzzyNumbers.rexamine.com
}
\examples{
    library(FuzzyNumbers)
    library(DISTRIB, warn.conflicts = FALSE)

    # Let us we are going to estimation the unknown mean of Normal population with known variance 
    # (e.g, sd(X) = 0.5) on the basis of 11 trapezoidal fuzzy numbers (which we simulate them in 
    # bellow for simplification).
    n = 11
    set.seed(1000)
    c1 = rnorm(n, 10,.5)
    c2 = rnorm(n, 10,.5)
    for(i in 1:n) {if (c1[i] > c2[i]) { zarf <- c1[i]; c1[i] <- c2[i]; c2[i] <- zarf }}
    round(c1,3); round(c2,3)
    c1 <= c2
    l = runif(n, 0,1); round(l,3)
    u = runif(n, 0,1); round(u,3)

    \donttest{
        EM.Trapezoidal(T.dist="norm", T.dist.par=c(NA,0.5), par.space=c(-5,30), c1, c2, l, u,
          start=4, ebs=.0001, fig=2)
    }
}

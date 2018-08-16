\name{EM.Trapezoidal}
\alias{EM.Trapezoidal}
\title{
MLE by EM algorithm based on Trapezoidal Fuzzy Data
}
\description{
This function can easily obtain Maximum Likelihood Estimation (MLE) for the unknown one-dimensional parameter on the basis of Trapezoidal Fuzzy observation.
}
\usage{
EM.Trapezoidal(T.dist, T.dist.par, par.space, c1, c2, l, u, start, ebs=0.001,
 fig = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{T.dist}{
the distribution name of the random variable is determined by characteristic element \code{T.dist}. The names of distributions is similar to \code{stats} package.
}
  \item{T.dist.par}{
a vector of distribution parameters with considered ordering in \code{stats} package. If \code{T.dist} has only one parameter (which obviously is unknown) the user must be considered \code{T.dist.par=NA}. Also, it may be \code{T.dist} has two parameters which one of them is unknown and another known. In such cases, the user must be considered \code{T.dist.par = c(NA, known parameter} where the first parameter is unknown, and \code{T.dist.par = c(known parameter, NA} where the second parameter is unknown. See bellow examples.
}
  \item{par.space}{
an interval which is a subset / subinterval of the parameter space and it must be contain the true value of unknown parameter.
}
  \item{c1}{
a vector with \code{length(c) = n} from the first point of the core-values of TrFNs.  
}
  \item{c2}{
a vector with \code{length(c) = n} from the last point of the core-values of TrFNs. Therefore, it is obvious that \eqn{c1 <= c2}.
}
  \item{l}{
a vector with \code{length(c) = n} from the left spreads of TrFNs.  
}
  \item{u}{
a vector with \code{length(c) = n} from the right spreads of TrFNs.  
}
  \item{start}{
a real number from \code{par.space} which EM algorithm must be started / worked with this start point.
}
  \item{ebs}{
a real positive small number (e.g., \eqn{0.01}, \eqn{0.001} or \eqn{0.1^6}) which determine the accuracy of EM algorithm in estimation of unknown parameter.
}
  \item{fig}{
a numeric argument which can tack only values 0, 1 or 2.

If \code{fig = 0}, the result of EM algorithm will not contains any figure.

If \code{fig = 1}, then the membership functions of TrFNs will be shown in a figure with different colors.

If \code{fig = 2}, then the membership functions of TrFNs will be shown in a figure with the curve of estimated probability density function (p.d.f.) on the basis of maximum likelihood estimation. 
}
}
\value{
The parameter computed / estimated in each iteration separately and also the computation of the following values can be asked directly. 
\item{ MLE }{the value of maximum likelihood estimated for unknown parameter by EM algorithm based on TrFNs.}
\item{ parameter.vector }{a vector of the ML estimated parameter for unknown parameter in algorithm which its first elements \code{start} and the last element is \code{MLE}.}
\item{ Iter.Num }{the number of EM algorithm iterations.}
}
\note{
In using this package it must be noted that:

(1)  The sample size of TrFNs must be less than 16. This package is able to work with small sample sizes (\eqn{n \leq 15}) and can be extended by the user if needs.

(2)  Considering a suitable interval for \code{par.space} is very important to obtain a true result for EM algorithm. It must be mentioned that this interval must be a sub-interval of the parameter space and the user must check the result of algorithm (MLE). It means that if the obtained MLE (by \code{EM.Trapezoidal}) overlay on the boundary of \code{par.space}, then the result is not acceptable and the EM algorithm must be repeated once again with a wider \code{par.space}. 

(3)  This package is able to work for continuous distributions with one or two parameter which only one of them is unknown and the user wants to estimate it based on TrFNs.
}
\examples{
    library(FuzzyNumbers)
    library(DISTRIB, warn.conflicts = FALSE)

    # Example 1: Estimation the unknown mean of Normal population with known variance (e.g, 
    # var=0.5^2) based of Trapezoidal FNs.
    n = 2
    set.seed(1000)
    c1 = rnorm(n, 10,.5)
    c2 = rnorm(n, 10,.5)
    for(i in 1:n) {if (c1[i] > c2[i]) { zarf <- c1[i]; c1[i] <- c2[i]; c2[i] <- zarf }}
    round(c1,3); round(c2,3)
    c1 <= c2
    l = runif(n, 0,1); round(l,3)
    u = runif(n, 0,1); round(u,3)
    
    EM.Trapezoidal(T.dist="norm", T.dist.par=c(NA,0.5), par.space=c(-5,30), c1, c2, l, u, start=4,
    ebs=.1, fig=2)


    \donttest{
        # Example 2: 
        n = 4	
        set.seed(10)
        c1 = rexp(n, 2)
        c2 = rexp(n, 2)
        for(i in 1:n) {if (c1[i] > c2[i]) { zarf <- c1[i]; c1[i] <- c2[i]; c2[i] <- zarf }}
        round(c1,3); round(c2,3)
        c1 <= c2
        l = runif(n, 0,1); round(l,3)
        u = runif(n, 0,2); round(u,3)

        EM.Trapezoidal(T.dist="exp", T.dist.par=NA, par.space=c(.1,20), c1, c2, l, u, start=7, 
          ebs=.001)


        # Example 3: Estimation the unknown standard deviation of Normal population with known 
        #  mean (e.g, mean=7) based of Trapezoidal FNs.
        n = 10	
        set.seed(123)
        c1 = rnorm(n, 4,1)
        c2 = rnorm(n, 4,1)
        for(i in 1:n) {if (c1[i] > c2[i]) { zarf <- c1[i]; c1[i] <- c2[i]; c2[i] <- zarf }}
        round(c1,3); round(c2,3)
        c1 <= c2
        l = runif(n, 0,.5); round(l,3)
        u = runif(n, 0,.75); round(u,3)

        EM.Trapezoidal(T.dist="norm", T.dist.par=c(4,NA), par.space=c(0,40), c1, c2, l, u, start=1,
         ebs=.0001, fig=2)


        # Example 4: Estimation alpha parameter in Beta distribution.
        n = 4	
        set.seed(12)
        c1 = rbeta(n, 2,1)
        c2 = rbeta(n, 2,1)
        for(i in 1:n) {if (c1[i] > c2[i]) { zarf <- c1[i]; c1[i] <- c2[i]; c2[i] <- zarf }}
        round(c1,3); round(c2,3)
        c1 <= c2
        l = rbeta(n, 1,1); round(l,3)
        u = rbeta(n, 1,1); round(u,3)

        EM.Trapezoidal(T.dist="beta", T.dist.par=c(NA,1), par.space=c(0,10), c1, c2, l, u, start=1,
         ebs=.01, fig=2)
    }
}

\keyword{ TrapezoidalFuzzyNumber }
\keyword{ EM algorithm }
\keyword{ Fuzzy Data }
\keyword{ Maximum Likelihood Estimation }

\seealso{
DISTRIB
FuzzyNumbers
}

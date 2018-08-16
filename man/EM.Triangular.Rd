\name{EM.Triangular}
\alias{EM.Triangular}
\title{
MLE by EM algorithm based on Triangular Fuzzy Data
}
\description{
This function can easily obtain Maximum Likelihood Estimation (MLE) for the unknown one-dimensional parameter on the basis of Triangular Fuzzy observation.
}
\usage{
EM.Triangular(T.dist, T.dist.par, par.space, c, l, u, start, ebs = 0.001,
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
  \item{c}{
a vector with \code{length(c) = n} from the core-values of TFNs.  
}
  \item{l}{
a vector with \code{length(c) = n} from the left spreads of TFNs.  
}
  \item{u}{
a vector with \code{length(c) = n} from the right spreads of TFNs.  
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

If \code{fig = 1}, then the membership functions of TFNs will be shown in a figure.

If \code{fig = 2}, then the membership functions of TFNs will be shown in a figure with the curve of estimated probability density function (p.d.f.) on the basis of maximum likelihood estimation. 
}
}
\value{
The parameter computed / estimated in each iteration separately and also the computation of the following values can be asked directly. 
\item{ MLE }{the value of maximum likelihood estimated for unknown parameter by EM algorithm based on TFNs.}
\item{ parameter.vector }{a vector of the ML estimated parameter for unknown parameter in algorithm which its first elements \code{start} and the last element is \code{MLE}.}
\item{ Iter.Num }{the number of EM algorithm iterations.}
}
\note{
In using this package it must be noted that:

(1)  The sample size of TFNs must be less than 16. This package is able to work with small sample sizes (\eqn{n \leq 15}) and can be extended by the user if needs.

(2)  Considering a suitable interval for \code{par.space} is very important to obtain a true result for EM algorithm. It must be mentioned that this interval must be a sub-interval of the parameter space and the user must check the result of algorithm (MLE). It means that if the obtained MLE (by \code{EM.Triangular}) overlay on the boundary of \code{par.space}, then the result is not acceptable and the EM algorithm must be repeated once again with a wider \code{par.space}. 

(3)  This package is able to work for continuous distributions with one or two parameter which only one of them is unknown and the user wants to estimate it based on TFNs.
}

\examples{
    library(FuzzyNumbers)
    library(DISTRIB, warn.conflicts = FALSE)

    # Example 1: 
    n = 2	
    set.seed(131)
    c = rexp(n, 2);    round(c,3)
    l = runif(n, 0,1); round(l,3)
    u = runif(n, 0,2); round(u,3)
 
    EM.Triangular(T.dist="exp", T.dist.par=NA, par.space=c(0,30), c, l, u, start=5, ebs=.1,
     fig=0)

    \donttest{
        EM.Triangular(T.dist="exp", T.dist.par=NA, par.space=c(0,30), c, l, u, start=50, ebs=.001,
         fig=1) #Fast Convergence

        EM.Triangular(T.dist="exp", T.dist.par=NA, par.space=c(0,30), c, l, u, start=50, ebs=.1^6,
         fig=2)


        #Example 2:  Computing the mean and the standard deviation of 20 EM estimations:
        n = 15
        MLEs=c()
        for(j in 100:120){
                         print(j)
                         set.seed(j)
                         c = rexp(n, 2)
                         l = runif(n, 0,1)
                         u = runif(n, 0,2)
                         MLEs = c(MLEs, EM.Triangular(T.dist="exp", T.dist.par=NA, 
                                                      par.space=c(0,30), c, l, u, start=5, 
                                                      ebs=.01, fig=0)$MLE )
                         }

        MLEs       #  3.283703 2.475541 3.171026 ...
        mean(MLEs) # 2.263996
        sd(MLEs)   # 0.4952257
        hist(MLEs)


        # Example 3: Estimation the unknown mean of Normal population with known variance
        #  (e.g, var=1) based of TFNs.
        n = 5	
        set.seed(100)
        c = rnorm(n, 10,1);    round(c,3)
        l = runif(n, 0,1); round(l,3)
        u = runif(n, 0,1); round(u,3)

        EM.Triangular(T.dist="norm", T.dist.par=c(NA,1), par.space=c(-10,30), c, l, u, start=20,
          ebs=.001, fig=2)


        # Example 4: Estimation the unknown standard deviation of Normal population with known
        #  mean (e.g, mean=7) based of TFNs.
        n = 10	
        set.seed(123)
        c = rnorm(n, 7,2);    round(c,3)
        l = runif(n, 0,2.5); round(l,3)
        u = runif(n, 0,1); round(u,3)

        EM.Triangular(T.dist="norm", T.dist.par=c(7,NA), par.space=c(0,10), c, l, u, start=5,
          ebs=.0001, fig=2)


        # Example 5: Estimation the unknown parameter b where  X ~ U(a=0,b).
        n = 15	
        set.seed(101)
        c = runif(n, 0,5); round(c,3)
        l = runif(n, 0,1); round(l,3)
        u = runif(n, 0,1); round(u,3)

        b <- EM.Triangular(T.dist="unif", T.dist.par=c(0,NA), par.space=c(0,10), c, l, u,
                           start=5, ebs=.001, fig=2)$MLE
        print(b)
    }
}

\keyword{ TriangularFuzzyNumber }
\keyword{ EM algorithm }
\keyword{ Fuzzy Data }
\keyword{ Maximum Likelihood Estimation }

\seealso{
DISTRIB
FuzzyNumbers
}

EM.Trapezoidal <-
function(T.dist, T.dist.par, par.space, c1, c2, l, u, start, ebs=.001, fig=2){
n <- length(c1)
par <- start

j = 1
No.stop = TRUE

for (i in (n+1):15) { c1[i]=c1[n]; c2[i]=c2[n]; l[i]=l[n]; u[i]=u[n] }

cat("==== EM estimation for n=", n, "Trapezoidal Fuzzy Numbers:", fill=T ) 

while(No.stop){
x.j = c()
x = NULL

if (length(T.dist.par)==1 & is.na(T.dist.par[1])) { 
T.dist.old.par <- par[j]
S = function(parameter) integrate(function(x) DISTRIB::pdf(T.dist, parameter, x) * evaluate(TrapezoidalFuzzyNumber(c1[1]-l[1],c1[1],c2[1],c2[1]+u[1]), x) * DISTRIB::pdf(T.dist, T.dist.old.par, x), max(par.space[1],c1[1]-l[1]), min(par.space[2],c2[1]+u[1]))$value^(n>=1)  * 
integrate(function(x) DISTRIB::pdf(T.dist, parameter, x) * evaluate(TrapezoidalFuzzyNumber(c1[2]-l[2],c1[2],c2[2],c2[2]+u[2]), x) * DISTRIB::pdf(T.dist, T.dist.old.par, x), max(par.space[1],c1[2]-l[2]), min(par.space[2],c2[2]+u[2]))$value^(n>=2)  * 
integrate(function(x) DISTRIB::pdf(T.dist, parameter, x) * evaluate(TrapezoidalFuzzyNumber(c1[3]-l[3],c1[3],c2[3],c2[3]+u[3]), x) * DISTRIB::pdf(T.dist, T.dist.old.par, x), max(par.space[1],c1[3]-l[3]), min(par.space[2],c2[3]+u[3]))$value^(n>=3)  * 
integrate(function(x) DISTRIB::pdf(T.dist, parameter, x) * evaluate(TrapezoidalFuzzyNumber(c1[4]-l[4],c1[4],c2[4],c2[4]+u[4]), x) * DISTRIB::pdf(T.dist, T.dist.old.par, x), max(par.space[1],c1[4]-l[4]), min(par.space[2],c2[4]+u[4]))$value^(n>=4)  * 
integrate(function(x) DISTRIB::pdf(T.dist, parameter, x) * evaluate(TrapezoidalFuzzyNumber(c1[5]-l[5],c1[5],c2[5],c2[5]+u[5]), x) * DISTRIB::pdf(T.dist, T.dist.old.par, x), max(par.space[1],c1[5]-l[5]), min(par.space[2],c2[5]+u[5]))$value^(n>=5)  * 
integrate(function(x) DISTRIB::pdf(T.dist, parameter, x) * evaluate(TrapezoidalFuzzyNumber(c1[6]-l[6],c1[6],c2[6],c2[6]+u[6]), x) * DISTRIB::pdf(T.dist, T.dist.old.par, x), max(par.space[1],c1[6]-l[6]), min(par.space[2],c2[6]+u[6]))$value^(n>=6)  * 
integrate(function(x) DISTRIB::pdf(T.dist, parameter, x) * evaluate(TrapezoidalFuzzyNumber(c1[7]-l[7],c1[7],c2[7],c2[7]+u[7]), x) * DISTRIB::pdf(T.dist, T.dist.old.par, x), max(par.space[1],c1[7]-l[7]), min(par.space[2],c2[7]+u[7]))$value^(n>=7)  * 
integrate(function(x) DISTRIB::pdf(T.dist, parameter, x) * evaluate(TrapezoidalFuzzyNumber(c1[8]-l[8],c1[8],c2[8],c2[8]+u[8]), x) * DISTRIB::pdf(T.dist, T.dist.old.par, x), max(par.space[1],c1[8]-l[8]), min(par.space[2],c2[8]+u[8]))$value^(n>=8)  * 
integrate(function(x) DISTRIB::pdf(T.dist, parameter, x) * evaluate(TrapezoidalFuzzyNumber(c1[9]-l[9],c1[9],c2[9],c2[9]+u[9]), x) * DISTRIB::pdf(T.dist, T.dist.old.par, x), max(par.space[1],c1[9]-l[9]), min(par.space[2],c2[9]+u[9]))$value^(n>=9)  * 
integrate(function(x) DISTRIB::pdf(T.dist, parameter, x) * evaluate(TrapezoidalFuzzyNumber(c1[10]-l[10],c1[10],c2[10],c2[10]+u[10]), x) * DISTRIB::pdf(T.dist, T.dist.old.par, x), max(par.space[1],c1[10]-l[10]), min(par.space[2],c2[10]+u[10]))$value^(n>=10)  * 
integrate(function(x) DISTRIB::pdf(T.dist, parameter, x) * evaluate(TrapezoidalFuzzyNumber(c1[11]-l[11],c1[11],c2[11],c2[11]+u[11]), x) * DISTRIB::pdf(T.dist, T.dist.old.par, x), max(par.space[1],c1[11]-l[11]), min(par.space[2],c2[11]+u[11]))$value^(n>=11)  * 
integrate(function(x) DISTRIB::pdf(T.dist, parameter, x) * evaluate(TrapezoidalFuzzyNumber(c1[12]-l[12],c1[12],c2[12],c2[12]+u[12]), x) * DISTRIB::pdf(T.dist, T.dist.old.par, x), max(par.space[1],c1[12]-l[12]), min(par.space[2],c2[12]+u[12]))$value^(n>=12)  * 
integrate(function(x) DISTRIB::pdf(T.dist, parameter, x) * evaluate(TrapezoidalFuzzyNumber(c1[13]-l[13],c1[13],c2[13],c2[13]+u[13]), x) * DISTRIB::pdf(T.dist, T.dist.old.par, x), max(par.space[1],c1[13]-l[13]), min(par.space[2],c2[13]+u[13]))$value^(n>=13)  * 
integrate(function(x) DISTRIB::pdf(T.dist, parameter, x) * evaluate(TrapezoidalFuzzyNumber(c1[14]-l[14],c1[14],c2[14],c2[14]+u[14]), x) * DISTRIB::pdf(T.dist, T.dist.old.par, x), max(par.space[1],c1[14]-l[14]), min(par.space[2],c2[14]+u[14]))$value^(n>=14)  * 
integrate(function(x) DISTRIB::pdf(T.dist, parameter, x) * evaluate(TrapezoidalFuzzyNumber(c1[15]-l[15],c1[15],c2[15],c2[15]+u[15]), x) * DISTRIB::pdf(T.dist, T.dist.old.par, x), max(par.space[1],c1[15]-l[15]), min(par.space[2],c2[15]+u[15]))$value^(n>=15)  
 }

if (length(T.dist.par)==2 & is.na(T.dist.par[1])) { 
T.dist.old.par <- c(par[j], T.dist.par[2])
S = function(parameter) integrate(function(x) DISTRIB::pdf(T.dist, c(parameter, T.dist.par[2]), x) * evaluate(TrapezoidalFuzzyNumber(c1[1]-l[1],c1[1],c2[1],c2[1]+u[1]), x) * DISTRIB::pdf(T.dist, T.dist.old.par, x), max(par.space[1],c1[1]-l[1]), min(par.space[2],c2[1]+u[1]))$value^(n>=1)  * 
integrate(function(x) DISTRIB::pdf(T.dist, c(parameter, T.dist.par[2]), x) * evaluate(TrapezoidalFuzzyNumber(c1[2]-l[2],c1[2],c2[2],c2[2]+u[2]), x) * DISTRIB::pdf(T.dist, T.dist.old.par, x), max(par.space[1],c1[2]-l[2]), min(par.space[2],c2[2]+u[2]))$value^(n>=2)  * 
integrate(function(x) DISTRIB::pdf(T.dist, c(parameter, T.dist.par[2]), x) * evaluate(TrapezoidalFuzzyNumber(c1[3]-l[3],c1[3],c2[3],c2[3]+u[3]), x) * DISTRIB::pdf(T.dist, T.dist.old.par, x), max(par.space[1],c1[3]-l[3]), min(par.space[2],c2[3]+u[3]))$value^(n>=3)  * 
integrate(function(x) DISTRIB::pdf(T.dist, c(parameter, T.dist.par[2]), x) * evaluate(TrapezoidalFuzzyNumber(c1[4]-l[4],c1[4],c2[4],c2[4]+u[4]), x) * DISTRIB::pdf(T.dist, T.dist.old.par, x), max(par.space[1],c1[4]-l[4]), min(par.space[2],c2[4]+u[4]))$value^(n>=4)  * 
integrate(function(x) DISTRIB::pdf(T.dist, c(parameter, T.dist.par[2]), x) * evaluate(TrapezoidalFuzzyNumber(c1[5]-l[5],c1[5],c2[5],c2[5]+u[5]), x) * DISTRIB::pdf(T.dist, T.dist.old.par, x), max(par.space[1],c1[5]-l[5]), min(par.space[2],c2[5]+u[5]))$value^(n>=5)  * 
integrate(function(x) DISTRIB::pdf(T.dist, c(parameter, T.dist.par[2]), x) * evaluate(TrapezoidalFuzzyNumber(c1[6]-l[6],c1[6],c2[6],c2[6]+u[6]), x) * DISTRIB::pdf(T.dist, T.dist.old.par, x), max(par.space[1],c1[6]-l[6]), min(par.space[2],c2[6]+u[6]))$value^(n>=6)  * 
integrate(function(x) DISTRIB::pdf(T.dist, c(parameter, T.dist.par[2]), x) * evaluate(TrapezoidalFuzzyNumber(c1[7]-l[7],c1[7],c2[7],c2[7]+u[7]), x) * DISTRIB::pdf(T.dist, T.dist.old.par, x), max(par.space[1],c1[7]-l[7]), min(par.space[2],c2[7]+u[7]))$value^(n>=7)  * 
integrate(function(x) DISTRIB::pdf(T.dist, c(parameter, T.dist.par[2]), x) * evaluate(TrapezoidalFuzzyNumber(c1[8]-l[8],c1[8],c2[8],c2[8]+u[8]), x) * DISTRIB::pdf(T.dist, T.dist.old.par, x), max(par.space[1],c1[8]-l[8]), min(par.space[2],c2[8]+u[8]))$value^(n>=8)  * 
integrate(function(x) DISTRIB::pdf(T.dist, c(parameter, T.dist.par[2]), x) * evaluate(TrapezoidalFuzzyNumber(c1[9]-l[9],c1[9],c2[9],c2[9]+u[9]), x) * DISTRIB::pdf(T.dist, T.dist.old.par, x), max(par.space[1],c1[9]-l[9]), min(par.space[2],c2[9]+u[9]))$value^(n>=9)  * 
integrate(function(x) DISTRIB::pdf(T.dist, c(parameter, T.dist.par[2]), x) * evaluate(TrapezoidalFuzzyNumber(c1[10]-l[10],c1[10],c2[10],c2[10]+u[10]), x) * DISTRIB::pdf(T.dist, T.dist.old.par, x), max(par.space[1],c1[10]-l[10]), min(par.space[2],c2[10]+u[10]))$value^(n>=10)  * 
integrate(function(x) DISTRIB::pdf(T.dist, c(parameter, T.dist.par[2]), x) * evaluate(TrapezoidalFuzzyNumber(c1[11]-l[11],c1[11],c2[11],c2[11]+u[11]), x) * DISTRIB::pdf(T.dist, T.dist.old.par, x), max(par.space[1],c1[11]-l[11]), min(par.space[2],c2[11]+u[11]))$value^(n>=11)  * 
integrate(function(x) DISTRIB::pdf(T.dist, c(parameter, T.dist.par[2]), x) * evaluate(TrapezoidalFuzzyNumber(c1[12]-l[12],c1[12],c2[12],c2[12]+u[12]), x) * DISTRIB::pdf(T.dist, T.dist.old.par, x), max(par.space[1],c1[12]-l[12]), min(par.space[2],c2[12]+u[12]))$value^(n>=12)  * 
integrate(function(x) DISTRIB::pdf(T.dist, c(parameter, T.dist.par[2]), x) * evaluate(TrapezoidalFuzzyNumber(c1[13]-l[13],c1[13],c2[13],c2[13]+u[13]), x) * DISTRIB::pdf(T.dist, T.dist.old.par, x), max(par.space[1],c1[13]-l[13]), min(par.space[2],c2[13]+u[13]))$value^(n>=13)  * 
integrate(function(x) DISTRIB::pdf(T.dist, c(parameter, T.dist.par[2]), x) * evaluate(TrapezoidalFuzzyNumber(c1[14]-l[14],c1[14],c2[14],c2[14]+u[14]), x) * DISTRIB::pdf(T.dist, T.dist.old.par, x), max(par.space[1],c1[14]-l[14]), min(par.space[2],c2[14]+u[14]))$value^(n>=14)  * 
integrate(function(x) DISTRIB::pdf(T.dist, c(parameter, T.dist.par[2]), x) * evaluate(TrapezoidalFuzzyNumber(c1[15]-l[15],c1[15],c2[15],c2[15]+u[15]), x) * DISTRIB::pdf(T.dist, T.dist.old.par, x), max(par.space[1],c1[15]-l[15]), min(par.space[2],c2[15]+u[15]))$value^(n>=15)  
 }

if (length(T.dist.par)==2 & is.na(T.dist.par[2])) { 
T.dist.old.par <- c(T.dist.par[1], par[j])
S = function(parameter) integrate(function(x) DISTRIB::pdf(T.dist, c(T.dist.par[1], parameter), x) * evaluate(TrapezoidalFuzzyNumber(c1[1]-l[1],c1[1],c2[1],c2[1]+u[1]), x) * DISTRIB::pdf(T.dist, T.dist.old.par, x), max(par.space[1],c1[1]-l[1]), min(par.space[2],c2[1]+u[1]))$value^(n>=1)  * 
integrate(function(x) DISTRIB::pdf(T.dist, c(T.dist.par[1], parameter), x) * evaluate(TrapezoidalFuzzyNumber(c1[2]-l[2],c1[2],c2[2],c2[2]+u[2]), x) * DISTRIB::pdf(T.dist, T.dist.old.par, x), max(par.space[1],c1[2]-l[2]), min(par.space[2],c2[2]+u[2]))$value^(n>=2)  * 
integrate(function(x) DISTRIB::pdf(T.dist, c(T.dist.par[1], parameter), x) * evaluate(TrapezoidalFuzzyNumber(c1[3]-l[3],c1[3],c2[3],c2[3]+u[3]), x) * DISTRIB::pdf(T.dist, T.dist.old.par, x), max(par.space[1],c1[3]-l[3]), min(par.space[2],c2[3]+u[3]))$value^(n>=3)  * 
integrate(function(x) DISTRIB::pdf(T.dist, c(T.dist.par[1], parameter), x) * evaluate(TrapezoidalFuzzyNumber(c1[4]-l[4],c1[4],c2[4],c2[4]+u[4]), x) * DISTRIB::pdf(T.dist, T.dist.old.par, x), max(par.space[1],c1[4]-l[4]), min(par.space[2],c2[4]+u[4]))$value^(n>=4)  * 
integrate(function(x) DISTRIB::pdf(T.dist, c(T.dist.par[1], parameter), x) * evaluate(TrapezoidalFuzzyNumber(c1[5]-l[5],c1[5],c2[5],c2[5]+u[5]), x) * DISTRIB::pdf(T.dist, T.dist.old.par, x), max(par.space[1],c1[5]-l[5]), min(par.space[2],c2[5]+u[5]))$value^(n>=5)  * 
integrate(function(x) DISTRIB::pdf(T.dist, c(T.dist.par[1], parameter), x) * evaluate(TrapezoidalFuzzyNumber(c1[6]-l[6],c1[6],c2[6],c2[6]+u[6]), x) * DISTRIB::pdf(T.dist, T.dist.old.par, x), max(par.space[1],c1[6]-l[6]), min(par.space[2],c2[6]+u[6]))$value^(n>=6)  * 
integrate(function(x) DISTRIB::pdf(T.dist, c(T.dist.par[1], parameter), x) * evaluate(TrapezoidalFuzzyNumber(c1[7]-l[7],c1[7],c2[7],c2[7]+u[7]), x) * DISTRIB::pdf(T.dist, T.dist.old.par, x), max(par.space[1],c1[7]-l[7]), min(par.space[2],c2[7]+u[7]))$value^(n>=7)  * 
integrate(function(x) DISTRIB::pdf(T.dist, c(T.dist.par[1], parameter), x) * evaluate(TrapezoidalFuzzyNumber(c1[8]-l[8],c1[8],c2[8],c2[8]+u[8]), x) * DISTRIB::pdf(T.dist, T.dist.old.par, x), max(par.space[1],c1[8]-l[8]), min(par.space[2],c2[8]+u[8]))$value^(n>=8)  * 
integrate(function(x) DISTRIB::pdf(T.dist, c(T.dist.par[1], parameter), x) * evaluate(TrapezoidalFuzzyNumber(c1[9]-l[9],c1[9],c2[9],c2[9]+u[9]), x) * DISTRIB::pdf(T.dist, T.dist.old.par, x), max(par.space[1],c1[9]-l[9]), min(par.space[2],c2[9]+u[9]))$value^(n>=9)  * 
integrate(function(x) DISTRIB::pdf(T.dist, c(T.dist.par[1], parameter), x) * evaluate(TrapezoidalFuzzyNumber(c1[10]-l[10],c1[10],c2[10],c2[10]+u[10]), x) * DISTRIB::pdf(T.dist, T.dist.old.par, x), max(par.space[1],c1[10]-l[10]), min(par.space[2],c2[10]+u[10]))$value^(n>=10)  * 
integrate(function(x) DISTRIB::pdf(T.dist, c(T.dist.par[1], parameter), x) * evaluate(TrapezoidalFuzzyNumber(c1[11]-l[11],c1[11],c2[11],c2[11]+u[11]), x) * DISTRIB::pdf(T.dist, T.dist.old.par, x), max(par.space[1],c1[11]-l[11]), min(par.space[2],c2[11]+u[11]))$value^(n>=11)  * 
integrate(function(x) DISTRIB::pdf(T.dist, c(T.dist.par[1], parameter), x) * evaluate(TrapezoidalFuzzyNumber(c1[12]-l[12],c1[12],c2[12],c2[12]+u[12]), x) * DISTRIB::pdf(T.dist, T.dist.old.par, x), max(par.space[1],c1[12]-l[12]), min(par.space[2],c2[12]+u[12]))$value^(n>=12)  * 
integrate(function(x) DISTRIB::pdf(T.dist, c(T.dist.par[1], parameter), x) * evaluate(TrapezoidalFuzzyNumber(c1[13]-l[13],c1[13],c2[13],c2[13]+u[13]), x) * DISTRIB::pdf(T.dist, T.dist.old.par, x), max(par.space[1],c1[13]-l[13]), min(par.space[2],c2[13]+u[13]))$value^(n>=13)  * 
integrate(function(x) DISTRIB::pdf(T.dist, c(T.dist.par[1], parameter), x) * evaluate(TrapezoidalFuzzyNumber(c1[14]-l[14],c1[14],c2[14],c2[14]+u[14]), x) * DISTRIB::pdf(T.dist, T.dist.old.par, x), max(par.space[1],c1[14]-l[14]), min(par.space[2],c2[14]+u[14]))$value^(n>=14)  * 
integrate(function(x) DISTRIB::pdf(T.dist, c(T.dist.par[1], parameter), x) * evaluate(TrapezoidalFuzzyNumber(c1[15]-l[15],c1[15],c2[15],c2[15]+u[15]), x) * DISTRIB::pdf(T.dist, T.dist.old.par, x), max(par.space[1],c1[15]-l[15]), min(par.space[2],c2[15]+u[15]))$value^(n>=15)  
 }


par <- c(par, optimize(S, interval=par.space, maximum=T)$maximum )

cat(" *** par(", j-1,")=", par[j], fill=T ) 
No.stop = ( abs(par[j]-par[j+1]) > ebs )
j <- j+1
 }

for (i in 1:n)
 {
  Xi <- TrapezoidalFuzzyNumber(c1[i]-l[i],c1[i],c2[i],c2[i]+u[i])
  if (fig==1 | fig==2) plot( Xi, xlim=c(min(c1-l), max(c2+u)), add = (i != 1), col = i )
  if (fig==2 & length(T.dist.par)==1 & is.na(T.dist.par[1])) curve(DISTRIB::pdf(T.dist, par[j-1], x), col=2, add=TRUE, lty=4, lwd=2)
  if (fig==2 & length(T.dist.par)==2 & is.na(T.dist.par[1])) curve(DISTRIB::pdf(T.dist, c(par[j-1], T.dist.par[2]), x), col=2, add=TRUE, lty=4, lwd=3)
  if (fig==2 & length(T.dist.par)==2 & is.na(T.dist.par[2])) curve(DISTRIB::pdf(T.dist, c(T.dist.par[1], par[j-1]), x), col=2, add=TRUE, lty=4, lwd=3)
 }

return( list( 
 MLE = par[j-1],
 parameter.vector = par[1:(j-1)],
 Iter.Num = j-2
            ) 
 )

}

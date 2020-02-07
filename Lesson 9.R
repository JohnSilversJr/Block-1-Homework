##### Problem 1 #####

library(ma391silvers)
library(ma391silvers2)
library(MASS)
library(NlcOptim)
library(lpSolve)
#objective function
obj = function(x){600-3*x[1]+x[2]*(800-x[2]+x[1])}

#plot
Z=Outer(obj,X)
contour(X$x,X$y,Z)
abline(a=5/2,b=-1/2,col="red")

#constraints
Aeq = matrix(c(12,5),nrow=1,byrow=TRUE)
Beq = matrix(2500)

x0=c(2,1)

#optimize
ans = solnl(x0,objfun = obj,Aeq=Aeq,Beq=Beq)
print(ans)


##### Problem 2 #####

#constraints
A = matrix(c(),nrow=2,byrow=TRUE)
B = matrix(c(0,0),nrow=2)

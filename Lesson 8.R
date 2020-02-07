install.packages("lpSolve")
install.packages("NlcOptim")
library(MASS)
library(NlcOptim)
library(lpSolve)

##### Problem 9 #####
#a.
profit = function(x){
  ((1.5*(80000-1600*x[1])+250*(350-3.5*x[1]+52.5*x[2])) - ((80000-8000*x[1])+(30000+6000*x[2]+30000+60000)))*(-1)
}
x0 = c(2,3)
A = matrix(c(-1,1,0,0,0,0,1,-1),nrow=4)
B = matrix(c(5,0,5,0),nrow=4)
solnl(x0,objfun=profit,A=A,B=B)

#ANSWER:
#[1,]    0
#[2,]    5
#Do not decrease editorial costs and increase sales to 50,000

Outer = function(f,x){
  n1 = length(x[[1]])
  n2 = length(x[[2]])
  res = matrix(0,nrow=n1,ncol=n2)
  rownames(res) = x[[1]]
  colnames(res) = x[[2]]
  for (i in 1:n1){
    for (j in 1:n2){
      res[i,j]=f(c(x[[1]][[i]],x[[2]][[j]]))
    }
  }
  return(res)
}

X = list(x=seq(0,5,.1),y=seq(0,5,.1))
Z = Outer(profit,X)
print(Z)
contour(x=X$x,y=X$y,z=-Z,lwd=2)
abline(v=5,col="red",lwd=3)
abline(h=5,col="red",lwd=3)

#d.
profit = function(a){
  f = function(x){
    return((1.5*(80000-1600*x[1]*a)+250*(350-3.5*x[1]*a+52.5*x[2])) - ((80000-8000*x[1])+(30000+6000*x[2]+30000+60000)))*(-1)
  }
  x0 = c(2,3)
  A = matrix(c(-1,1,0,0,0,0,1,-1),nrow=4)
  B = matrix(c(5,0,5,0),nrow=4)
  ans = solnl(x0,objfun=f,A=A,B=B)
}

a = seq(1,2,.1)
ans.x1=0
ans.x2=0
ans.profit=0
for(i in 1:length(a)){
  ans = profit(a[i])
  ans.x1[i]=ans$par[1][1]
  ans.x2[i]=ans$par[2][1]
  ans.profit[i] = -ans$fn
}
result = data.frame(a=a,x1=ans.x1,x2=ans.x2,profit=format(ans.profit,big.mark=","))
print(result)

##### Problem 10a done in excel #####

##### Problem 1 #####

#a. Maximize Profit
# Decision Variables: R = x[1] = rebate ($)
# Objective Function  P = Profit (base of #1500)
# Other Variables:    S = sales = R/100 * .15 ()

library(ma391silvers)
P = function(x){(1500-100*x)*(1.15*x)

}
x=seq(0,15,1)
plot(x,P(x),type="o")

dP = function(x){fprime(P,x)}
bisection(dP,0,15)

#answer = $750 rebate

#does it make sense?
(1500-750)*1.15*7.5
(1500-700)*1.15*7
(1500-800)*1.15*8

#b. Sensitivity analysis
S = seq(1.1,1.2,0.01)
ans = 0
ans.profit = 0
for(i in 1: length(S)){
  P = function(x){(1500-100*x)*(S*x)

  }
  dP = function(x){fprime(P,x)}
  ans[i] = bisection(dP,-10,20)
  ans.profit[i]=P(ans[i])
}
result = data.frame(salesIncrease=S,optimalRebate=ans,profit=ans.profit)
print(result)

#the answer I am getting is that it doesn't change even though that doesn't make sense

#c.
P2 = function(x){(1500-100*x)*(1.1*x)

}
x=seq(0,20,1)
plot(x,P2(x),type="o")

dP = function(x){fprime(P2,x)}
bisection(dP,0,15)

#the answer I am getting is that it doesn't change from an increase of 15%.  It should be lower if not a negative impact by giving a rebate at that price

##### Problem 2 #####

feed = seq(0.3,0.6,0.05)
pr = array(0,length(feed))
ans = array(0,length(feed))
for (i in 1:length(feed)){
  profit = function (x){
    (0.65-0.01*x)*(200+5*x)-feed[i]*x
  }
  dProfit = function(x){fprime(profit,x,)}
  ans[i] = bisection(dProfit,-100,50,0.0001)
  pr[i] = profit(round(ans[i]))
}
print(ans)
print(pr)
plot(feed,ans,"o",xlab="feed cost",ylab="x(Days to Sell)")

profit = function (x){
  return((0.65-0.01*x)*(200+7*x)-0.6*x)
}
x = seq(0,20,1)
plot(x,profit(x),type="o",xlab="x(days)")
abline(h=132.8) #represents the max profits for previous feed
dProfit = function(x){fprime(profit,x)}
ans=bisection(dProfit,0,20)
print(ans)

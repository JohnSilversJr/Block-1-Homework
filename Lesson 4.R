##### Problem 8 #####
p=function(x){(0.65-0.01*t)*(200+5*t-t^2/60)-0.45*t}
dP=function(t){fprime(p,t)}
t=seq(0,20)
ans = Bisection(dP,10)
print(ans)
print(p(ans))


##### We did this in class on lesson 4 and I could not figure out how to make the function "newton" and get it to work #####
months = seq(1,10)
for (i in 1:length(months)){
  m = months[i]
  p = function(t){(0.65-0.01*t)*((5/m)*(m*t-t^2/60)+200-0.45*t)}
  dP = function(t){fprime(p,t)}
  ans.time[i] = newton(dP,10)
  ans.profit[i] = p(ans.time[i])
}
result = data.frame(months = months,time=ans.time,profit = ans.profit)
print(results)

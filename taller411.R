n=10000000
x=sample(1:6,2*n, replace = TRUE)
xy=matrix(x, ncol = 2)
t=apply(xy, 1, sum)
#data.frame(xy,t)
y = as.numeric(t>=7 & t<=11)
sum(y)/n


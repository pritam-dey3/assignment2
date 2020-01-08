library(lattice)
library(microbenchmark)

avg <- function(n, r) {
  y = numeric(0)
  for (i in 1:r){
    s = runif(n)
    w = floor(runif(1) * n) + 1
    t = summary(microbenchmark(order_statistic(s, w), times = 1L))[["mean"]]
    y = c(y, t)
  }
  mean(y)
}

test_hyp <- function(N = 20000, r=20) {
  n = seq(1000, N, by = 1000)
  nlogn = sapply(n, function (k) k*log(k))
  t = rep(0, length(n))
  j = 1
  for (i in n){
    t[j] = avg(i, r)
    print(j) #to track progress
    j=j+1
  }
  xyplot(t ~ n,
         type=c('p', 'l', 'smooth'),
         auto.key = TRUE)
}

test_hyp() #the loes curve is straight line hence average case run time is O(n)
# from the curve we see the variance is also increasing with increase in n
# since it is not increasing very reapidly, we guess order of variance is also
# O(n)

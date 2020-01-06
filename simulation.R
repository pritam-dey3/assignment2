library(lattice)
library(microbenchmark)

test_hyp <- function(N = 50000) {
  n = seq(1000, N, by = 1000)
  nlogn = sapply(n, function (k) k*log(k))
  t = rep(0, length(n))
  j = 1
  for (i in n){
    x = runif(i)
    w = floor(runif(1) * i) + 1
    t[j] = summary(microbenchmark(order_statistic(x, w), unit="ms"))[["mean"]]
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

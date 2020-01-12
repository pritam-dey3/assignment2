library(microbenchmark)

avg <- function(n, r) {
  y = numeric(0)
  for (i in 1:r){
    s = sample(1:n)
    w = floor(runif(1) * n) + 1
    t = summary(microbenchmark(order_statistic(s, w), times = 1L, unit = "us"))[["mean"]]
    y = c(y, t)
  }
  mean(y)
}

test_hyp <- function(N = 20000, r=20) {
  n = seq(1000, N, by = 1000)
  t = rep(0, length(n))
  j = 1
  for (i in n){
    t[j] = avg(i, r)
    print(j) #to track progress
    j=j+1
  }
  df = data.frame(cbind(n, t))
  write.csv(df, "data.csv")
  df
}

df = test_hyp()



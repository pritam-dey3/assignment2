library(lattice)

time_data <- function(n, r) {
  y = numeric(0)
  for (i in 1:r){
    s = sample(1:n)
    #w = floor(runif(1) * n) + 1
    w = floor(runif(1)*n) + 1
    t = summary(microbenchmark(order_statistic(s, w), times = 1L, unit = "us"))[["mean"]]
    y = c(y, t)
  }
  y
}

t = time_data(4000, 200)
hist(t)
qqplot(rnorm(200), t)


n = 1:500
y = rnorm(n, mean = n, sd=10)
plot(y)

y1 = rnorm(n, mean = n*log(n), sd = 10)
plot(y1)

y = y/n
modelt = lm(y ~ log(n))
summary(modelt)
hist(y, breaks = 100)

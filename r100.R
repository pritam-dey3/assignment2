
rcoin <- function() {
  rbinom(n = 1, size = 1, prob = 0.5)
}

r100 <- function(n=100) {
  v = n + 1
  while(v >= n) {
    v = sum(sapply(0:6, function(i) rcoin() * 2 ^ i))
  }
  v
}

val_plot <- function() {
  N = 10000
  rand_sample <- rep(0, N)
  for (i in 1:N){
    rand_sample[i] = r100()
  }
  hist(rand_sample, breaks = 0:100, right = F)
}

val_plot()

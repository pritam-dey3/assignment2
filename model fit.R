library(lattice)
df <- read.csv("data/data.csv")

df$logn = log(df$n)
df$t_n = df$t / df$n

model_logn <- lm(t_n ~ logn, data = df)
summary(model_logn)

fitted_logn = predict(model_logn)

xyplot(t_n + fitted_logn ~ n, data = df,
       type=c('p', 'l'),
       auto.key = TRUE)

library(trafo)
assumptions(model_logn)

resd = model_logn$residuals
plot(model_logn)

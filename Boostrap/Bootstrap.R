set.seed(1995)
indexes <- createResample(mnist_27$train$y, 10)

set.seed(1)

q75 <- replicate(10000,
quantile(rnorm(100, 0, 1),0.75)

)

set.seed(1)
y <- rnorm(100, 0, 1)
indexes <- createResample(y,10000)
ses <- sapply(indexes, function(ind){
  y_star <- y[ind]
  quantile <- quantile(y_star,0.75)
} )

mean(ses)
sd(ses)
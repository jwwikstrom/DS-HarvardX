library(rpart)
n <- 1000
sigma <- 0.25
x <- rnorm(n, 0, 1)
y <- 0.75 * x + rnorm(n, 0, sigma)
dat <- data.frame(x = x, y = y)
fit <- rpart(y ~ ., data = dat)

dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col=2)

library(randomForest)
fit <- randomForest(y ~ x, data = dat) 
  dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = 2)
  
  library(randomForest)
  fit <- randomForest(y ~ x, data = dat, nodesize = 50, maxnodes = 25)
    dat %>% 
    mutate(y_hat = predict(fit)) %>% 
    ggplot() +
    geom_point(aes(x, y)) +
    geom_step(aes(x, y_hat), col = 2)
    
  set.seed(1)
  fit <- train(y~x, method ="Rborist", data =dat, tuneGrid = data.frame(predFixed=1,minNode = seq(25,100,25)))
  library(caret)
  dat %>% 
    mutate(y_hat = predict(fit)) %>% 
    ggplot() +
    geom_point(aes(x, y)) +
    geom_step(aes(x,y_hat), col=2)
  
  
 dat <- data("tissue_gene_expression")
 y <- tissue_gene_expression$y
 x <- tissue_gene_expression$x
  fit_rpart <- train(x,y,method="rpart" , tuneGrid=data.frame(cp=seq(0,0.1,0.01)))
  control <- rpart.control(minsplit=0)
  fit <- train(x,y,method="rpart" , tuneGrid=data.frame(cp=seq(0,0.1,0.01)), control=control)
  
  control <- rpart.control(nodesize=1)
set.seed(1991)
fit_rf <- train(x,y,data=dat,method = "rf", tuneGrid = data.frame(mtry=seq(50,200,25)), nodesize=1)

tree_terms <- as.character(unique(fit_rf$finalModel$frame$var[!(fit_rpart$finalModel$frame$var == "<leaf>")]))
tree_terms
  
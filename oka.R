library(purrr)
library(caret)
library(tidyverse)
library(dslabs)


ks = seq(1,101,3)

set.seed(1)

ind <- createDataPartition(heights$sex,times = 1, p = 0.5, list = FALSE)

train_set <- heights %>% slice(ind)

test_set <- heights %>% slice(-ind)


F_1 <- map_df(ks, function(k){
  fit <- knn3(sex ~ height, data = train_set, k=k)
  y_hat <- predict(fit, newdata = test_set, type = "class")
  F_1 <- F_meas(data = y_hat, reference = factor(test_set$sex))
  list(k=k, F_1 = F_1)
})
F_1 %>% ggplot(aes(k,F_1)) + geom_line()
F_1[which.max(F_1$F_1),]

data("tissue_gene_expression")

ks = seq(1,11,2)

x <- tissue_gene_expression$x
y <- tissue_gene_expression$y
set.seed(1)
ind <- createDataPartition(y, times=1, p=0.5, list= FALSE)

train_x <- x[ind,]
train_y <- y[ind]
test_x <- x[-ind,]
test_y <- y[-ind]

accuracy <- map_df(ks, function(k){
  fit <- knn3(train_x,train_y, k=k)
  y_hat <- predict(fit, newdata = test_x, type = "class")
  accuracy <- confusionMatrix(data = y_hat, reference = test_y)$overall["Accuracy"]
  list(k=k, accuracy = accuracy)
})

fit <- knn3(x,y, k=k)
y_hat <- predict(fit, newdata = test_x, type = "class")
accuracy <- confusionMatrix(data = y_hat, reference = test_y)

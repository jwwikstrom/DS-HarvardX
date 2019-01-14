set.seed(1993)
data("tissue_gene_expression")
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
x <- x[, sample(ncol(x), 10)]
ind <- createDataPartition(y,times=1,p=0.5,list=FALSE)
train_set <- data.frame(y=y[ind],x[ind,])
test_set <- data.frame(y=y[-ind],x[-ind,])
mod <- train(x,y ,method="lda")


y_hat <- predict(mod, test_set)
confusionMatrix(data = y_hat, reference = y[-ind])$overall[["Accuracy"]]

plot(mod$finalModel$means[1,],mod$finalModel$means[2,])

set.seed(1993)
data("tissue_gene_expression")

x <- tissue_gene_expression$x
x <- x[, sample(ncol(x), 10)]
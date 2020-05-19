set.seed(1996)
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()

x_subset <- x[ ,sample(p, 100)]

fit <- train(x_subset, y, method = "glm")
fit$results

library(devtools)
devtools::install_bioc("genefilter")
library(genefilter)
tt <- colttests(x, y)
pvals <- tt$p.value
ind <- pvals<=0.01
mean(ind)

x <- x[,ind]
x_subset <- x[ ,sample(108, 100)]
fit <- train(x_subset, y, method = "glm")
fit$results


fit <- train(x_subset, y, method = "knn", tuneGrid = data.frame(k = seq(101, 301, 25)))
ggplot(fit)


data("tissue_gene_expression")

fit <- train(tissue_gene_expression$x, tissue_gene_expression$y, method = "knn", tuneGrid = data.frame(k = seq(1, 20, 1)))
ggplot(fit)

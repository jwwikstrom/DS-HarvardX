
#############################################################
# Create edx set, validation set, and submission file
#############################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate")
if(!require(recommenderlab)) install.packages("recommenderlab")
if(!require(reshape2)) installed.packages("reshape2")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                      col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data

set.seed(1)
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set

validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)


rm(dl, ratings, movies, test_index, temp, movielens, removed)

edx <- edx %>% 
  mutate(date = as_datetime(timestamp)) %>% 
  select(-timestamp)


RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2,na.rm=TRUE))
}
save(edx, validation,RSME, file = "edx.RData")

# Plot movie bias

# Plot user bias

# Calculate estimation of mu
mu_hat <- mean(edx$rating)

# Create vectors lambdas_m and lambdas_u containing the values of lambda_m and lambda_u to test
lambdas_i <- seq(3,7,1)
lambdas_j <- seq(4,7,1)

#K fold cross-validation, add the fold number to edx
n_folds <- 5
edx <- edx %>%
  mutate(K_fold = createFolds(y = edx$rating, k = n_folds, list = FALSE))

# For each combination of lambda_m and lambda_u, perform cross-validation with k = n_folds and calculate mean RMSE for those folds
Folds <- seq(1,n_folds)
rmses <- sapply( lambdas_i,
                 function(l_i){sapply(lambdas_j,
                                      function(l_u){mean(sapply(Folds,
                                                           function(k){

                                                             B_i_hat <- edx %>%
                                                               filter(K_fold != k) %>%
                                                               group_by(movieId) %>%
                                                               summarize(b_i_hat = sum(rating-mu_hat)/(n()+l_i))

                                                             B_j_hat <- edx %>%
                                                               filter(K_fold != k) %>%
                                                               left_join(B_i_hat, by = 'movieId') %>%
                                                               group_by(userId) %>%
                                                               summarize(b_j_hat = sum(rating - mu_hat - b_i_hat)/(n()+l_j))


                                                             predicted_ratings <- edx %>%
                                                               filter(K_fold == k) %>%
                                                               left_join(B_i_hat, by = 'movieId') %>%
                                                               left_join(B_j_hat, by = 'userId') %>%
                                                               mutate(pred_rating = mu_hat + b_i_hat + b_j_hat) %>%
                                                               .$pred_rating

                                                             # predicted_ratings[predicted_ratings>5] <- 5
                                                             # predicted_ratings[predicted_ratings<0.5] <- 0.5
                                                             RMSE(predicted_ratings,edx %>% filter(K_fold == k) %>% .$rating)

                                                           }))
                                      })
                 })

# Assign best values found for each lambda
lambda_i <- lambdas_m[which(rmses==min(rmses), arr.ind = TRUE)[2]]
lambda_j <- lambdas_u[which(rmses==min(rmses), arr.ind = TRUE)[1]]

# Estimate using entire edx set with the best lambdas
# Movie bias
B_i_hat <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_i_hat = sum(rating-mu_hat)/(n()+lambda_i))

# User bias
B_j_hat <- edx %>%
  left_join(B_i_hat, by = 'movieId') %>%
  group_by(userId) %>%
  summarize(b_j_hat = sum(rating - mu_hat - b_i_hat)/(n()+lambda_j))

# First predict on edx set to find residuals
first_prediction <- edx %>%
  left_join(B_i_hat, by = 'movieId') %>%
  left_join(B_j_hat, by = 'userId') %>%
  mutate(pred_rating = mu_hat + b_i_hat + b_j_hat) %>%
  .$pred_rating

# Calculate residuals, res, using the first prediction and spread into matrix R, with users as rows and movies as coloumns
R <- edx %>%
  mutate(res = rating - first_prediction) %>%
  group_by(movieId) %>%
  filter(n()>30) %>%
  ungroup() %>%
  group_by(userId) %>%
  filter(n()>30) %>%
  ungroup() %>%
  select(userId,movieId,res) %>%
  spread(movieId,res) %>%
  as.matrix()

# Convert first coloumn to rownames (first coloumn is userId)
rownames(R) <- R[,1]
R <- R[,-1]

# Perform matrix factorization to find U and V that estimates filled y
R_svd <- funkSVD(R, verbose = TRUE, k=5, max_epochs = 300)

# Create R_hat from U and V
R_hat <- R_svd$U %*% t(R_svd$V)

# Apply rownames and colnames from R to R_hat
rownames(R_hat) <- rownames(R)
colnames(R_hat) <- colnames(R)

# Remove stuff to free memory
rm(R,edx,R_svd, first_prediction)

# Melt to structured format
R_hat <- melt(R_hat, value.name = "r", varnames = c("userId","movieId"))

# Create svd predictions using mu_hat, B_m_hat, B_u_hat
svd_prediction <- validation %>%
  select(userId,movieId) %>%
  left_join(B_i_hat, by = 'movieId') %>%
  left_join(B_j_hat, by = 'userId') %>%
  mutate(pred_rating = mu_hat + b_i_hat + b_j_hat ) %>%
  select(movieId,userId, pred_rating)

# Create fold for R_hat since left_join on entire R_hat used to much memory
R_hat <- R_hat %>%
    mutate(K_fold = createFolds(y = R_hat$r, k = 100, list = FALSE))

# For each fold, 1 to 100, join the R_hat values to finalize the svd predictions
for (i in 1:100) {
  svd_prediction <- svd_prediction %>%
    left_join(R_hat %>% filter(K_fold == i) , by = c("userId", "movieId")) %>%
    replace_na(list(r = 0)) %>%
    mutate(pred_rating = pred_rating + r) %>%
    select(movieId,userId, pred_rating)
}

# Fit out of interval ratings into the interval [0.5, 5]
svd_prediction$pred_rating[svd_prediction$pred_rating>5] <- 5
svd_prediction$pred_rating[svd_prediction$pred_rating<0.5] <- 0.5

# Calculate RMSE
RMSE(validation$rating,svd_prediction$pred_rating)

# Save predictions
write_csv(svd_prediction, path = "predictions.CSV")

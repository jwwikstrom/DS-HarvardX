
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
# 
# ROUND_TO_RATING <- function(rating){
#   0.5*round(rating/0.5)
# }
# Plot movie bias

# Plot user bias

# Plot day bias
edx %>% 
  mutate(week=wday(round_date(date, unit = "day"))) %>% 
  group_by(week) %>% 
  summarise(mean_rating = mean(rating)) %>% 
  ggplot(aes(x=week, y = mean_rating)) + geom_line()


# Calculate estimation of mu
mu_hat <- mean(edx$rating)

# Create test and train set
# test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
# edx_test <- edx[test_index,]
# edx_train <- edx[-test_index,]

# Create vectors lambdas_m and lambdas_u containing the values of lambda_m and lambda_u to test 
lambdas_m <- seq(3,7,1)
lambdas_u <- seq(4,7,1)

#K fold cross-validation, add the fold number to edx
n_folds <- 5
edx <- edx %>%
  mutate(K_fold = createFolds(y = edx$rating, k = n_folds, list = FALSE))

# For each combination of lambda_m and lambda_u, perform cross-validation with k = n_folds and calculate mean RMSE for those folds
Folds <- seq(1,n_folds)
rmses <- sapply( lambdas_m, 
                 function(l_m){sapply(lambdas_u, 
                                      function(l_u){mean(sapply(Folds,
                                                           function(k){
                                                             
                                                             B_m_hat <- edx %>% 
                                                               filter(K_fold != k) %>%
                                                               group_by(movieId) %>% 
                                                               summarize(b_i_hat = sum(rating-mu_hat)/(n()+l_m))
                                                             
                                                             B_u_hat <- edx %>%
                                                               filter(K_fold != k) %>%
                                                               left_join(B_m_hat, by = 'movieId') %>%
                                                               group_by(userId) %>%
                                                               summarize(b_j_hat = sum(rating - mu_hat - b_i_hat)/(n()+l_u))
                                                             
                                                             
                                                             predicted_ratings <- edx %>%
                                                               filter(K_fold == k) %>%
                                                               left_join(B_m_hat, by = 'movieId') %>%
                                                               left_join(B_u_hat, by = 'userId') %>%
                                                               mutate(pred_rating = mu_hat + b_i_hat + b_j_hat) %>%
                                                               .$pred_rating
                                                             
                                                             # predicted_ratings[predicted_ratings>5] <- 5
                                                             # predicted_ratings[predicted_ratings<0.5] <- 0.5
                                                             RMSE(predicted_ratings,edx %>% filter(K_fold == k) %>% .$rating)
                                                             
                                                           }))
                                      })
                 })

lambda_m <- lambdas_m[which(rmses==min(rmses), arr.ind = TRUE)[2]]
lambda_u <- lambdas_u[which(rmses==min(rmses), arr.ind = TRUE)[1]] 

# Estimate using entire edx set
# Movie bias
B_m_hat <- edx %>% 
  group_by(movieId) %>% 
  summarize(b_i_hat = sum(rating-mu_hat)/(n()+lambda_m))

# User bias
B_u_hat <- edx %>%
  left_join(B_m_hat, by = 'movieId') %>%
  group_by(userId) %>%
  summarize(b_j_hat = sum(rating - mu_hat - b_i_hat)/(n()+lambda_u))

# Predict on edx set
predicted_ratings <- edx %>%
  left_join(B_m_hat, by = 'movieId') %>%
  left_join(B_u_hat, by = 'userId') %>%
  mutate(pred_rating = mu_hat + b_i_hat + b_j_hat) %>%
  .$pred_rating

# predicted_ratings[predicted_ratings>5] <- 5
# predicted_ratings[predicted_ratings<0.5] <- 0.5

# predicted_validation <- validation %>%
#   left_join(B_m_hat, by = 'movieId') %>%
#   left_join(B_u_hat, by = 'userId') %>%
#   mutate(pred_rating = mu_hat + b_i_hat + b_j_hat) %>%
#   .$pred_rating
# 
# predicted_validation[predicted_validation>5] <- 5
# predicted_validation[predicted_validation<0.5] <- 0.5
# 
# RMSE(validation$rating , predicted_validation)
# 
# RMSE(edx$rating,predicted_ratings)

# Spread edx into matrix y, of residuals res, with users as rows and movies as coloumns
y <- edx %>%
  mutate(res = rating - predicted_ratings) %>%
  group_by(movieId) %>%
  filter(n()>200) %>%
  ungroup() %>%
  group_by(userId) %>%
  filter(n()>400) %>%
  ungroup() %>%
  select(userId,movieId,res) %>%
  spread(movieId,res) %>%
  as.matrix()

# Convert first coloumn to rownames (first coloumn is userId)
rownames(y) <- y[,1]
y <- y[,-1]

# Perform matrix factorization to find U and V that estimates filled y
y_svd <- funkSVD(y, verbose = TRUE, k=5, max_epochs = 300)

# 
r <- y_svd$U %*% t(y_svd$V)
hist(abs(y-r))

rownames(r) <- rownames(y)
colnames(r) <- colnames(y)
test_svd <- melt(r, value.name = "r_svd", varnames = c("userId","movieId"))

svd_prediction <- validation %>%
  left_join(B_m_hat, by = 'movieId') %>%
  left_join(B_u_hat, by = 'userId') %>%
  left_join(test_svd, by = c("userId", "movieId")) %>%
  replace_na(list(r_svd = 0)) %>%
  mutate(pred_rating = mu_hat + b_i_hat + b_j_hat + r_svd) %>%
  .$pred_rating

RMSE(validation$rating,svd_prediction)

test <- edx %>%
  filter(userId == 7) %>%
  left_join(B_m_hat, by = 'movieId') %>%
  left_join(B_u_hat, by = 'userId') %>%
  left_join(test_svd, by = c("movieId", "userId")) %>%
  replace_na(list(r_svd = 0)) %>%
  mutate(pred_rating = mu_hat + b_i_hat + b_j_hat + r_svd) %>%
  select(rating,movieId,b_i_hat, b_j_hat, r_svd, pred_rating)

# errors <- edx %>%
#   mutate(error = rating - ROUND_TO_RATING(predicted_ratings)) %>%
#   filter(error == -0.5)
#   
# hist(errors$error)

# test
# B_M_T <- edx %>%
#   mutate(year = year(date)) %>%
#   group_by(movieId, year) %>%
#   summarize(b_i_t = mean(rating - mu_hat))
# 
# n_05 <- edx %>%
#   group_by(userId) %>%
#   filter(n()>0) %>% 
#   summarize(n_05 = sum(rating%%1==0.5))
# 
# 
# 
# # Calculate estimation of user bias
# lambda_u <- 3
# B_u_hat <- edx %>%
#   left_join(B_m_hat, by = 'movieId') %>%
#   group_by(userId) %>%
#   summarize(b_u_hat = sum(rating - mu_hat - b_m_hat)/(n()+lambda_u))
# 


# # Matrix with user as row and movie as col 
# ratings <- edx %>% 
#   group_by(movieId) %>%
#   filter(n() > 100) %>% 
#   ungroup() %>%
#   group_by(userId) %>%
#   filter(n() > 50) %>%
#   select(userId,movieId,rating) %>% 
#   spread(movieId,rating) %>% 
#   as.matrix()
# 
# 
# X_svd <- svd(X)
# 
# model_knn <- train(X,Y,method = "knn")

# # Estimates
# Y_hat <- edx %>%
#   left_join(B_m_hat, by = 'movieId') %>%
#   left_join(B_u_hat, by = 'userId') %>%
#   mutate(y_hat = mu_hat + b_m_hat + b_u_hat ) %>%
#   .$y_hat
# 
# rmse <- RMSE(edx$rating,Y_hat)
# 
# # Validation estimates
# Y_hat <- validation %>%
#   left_join(B_m_hat, by = 'movieId') %>%
#   left_join(B_u_hat, by = 'userId') %>%
#   mutate(y_hat = mu_hat + b_m_hat + b_u_hat ) %>%
#   .$y_hat
# 
# # Ratings will go into the CSV submission file below:
# 
# write.csv(validation %>% select(userId, movieId) %>% mutate(rating = Y_hat ),
#           "submission.csv", na = "", row.names=FALSE)
# rm(dl, ratings, movies, test_index, temp, movielens, removed)



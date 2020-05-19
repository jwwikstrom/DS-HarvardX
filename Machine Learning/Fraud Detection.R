
library(readr)
library(dplyr)
library(ggplot2)
library(caret)
library(MLmetrics)
library(gbm)

# Read and throw away non-used attribute
paysim <- read_csv("R data/PS_20174392719_1491204439457_log.csv"
                              ,col_types = cols(type = col_factor(NULL), isFraud = "l", isFlaggedFraud = "l"))
paysim <- paysim %>% select(-isFlaggedFraud)

# Check summary of data
summary(paysim)

# Summary of fraud transactions
paysim %>% filter(isFraud == TRUE) %>% summary()

# Summary of non-fraud transactions
paysim %>% filter(isFraud == FALSE) %>% summary()

# Number of frauds 0.12%
mean(paysim$isFraud)*100

#which steps do we transactions ?
paysim %>% group_by(step) %>% summarize(n = n()) %>% ggplot(aes(x = step, y = n)) + geom_step()

# Check steps where frauds are made
paysim %>% filter(isFraud == TRUE) %>% ggplot(aes(step)) + geom_histogram( bins = max(paysim$step))


# Number of frauds per type
# type     n_fraud
# <fct>      <int>
#   1 TRANSFER    4097
# 2 CASH_OUT    4116
paysim %>% filter(isFraud == TRUE) %>% group_by(type) %>% summarize(n_fraud = n())


# Only one fraud per orig
paysim %>% filter(isFraud == TRUE) %>% group_by(nameOrig) %>% summarise(nFraudTransactions = n()) %>% select(nFraudTransactions) %>% table()

# 44 dest is used more than once
paysim %>% filter(isFraud == TRUE) %>% group_by(nameDest) %>% summarise(nFraudTransactions = n()) %>% select(nFraudTransactions) %>% table()


# Some examples of Frauds
paysim %>% filter(isFraud == TRUE) %>% head(20) 


### CREATE ATTRIBUTES ###
# convert into factor for training and remove names
paysim$isFraud <- as.factor(paysim$isFraud)
paysim$isSusp <- as.factor(paysim$isSusp)
paysim <- paysim %>% select(-c(nameOrig, nameDest))
paysim <- paysim %>% filter(type %in% c("TRANSFER", "CASH_OUT"))


# Create suspect cash out
susp_co <- paysim %>% group_by(step, amount) %>% summarize(isSusp = n()>1)

# Join susp_co into paysim
paysim <- paysim %>%
  left_join(susp_co, by = c('step', 'amount'))


# Split into validation at 80%
validation_cutoff <- paysim$step[NROW(paysim)*0.8]
validation <- paysim %>% filter(step>validation_cutoff) %>% as.data.frame()
transactions <- paysim %>% filter(step<=validation_cutoff)  %>% as.data.frame()

#see sizes
NROW(validation)/NROW(paysim)
NROW(transactions)/NROW(paysim)

# Create folds
folds <- createFolds(1:NROW(transactions), k = 5)

F1 <- function (data, lev = NULL, model = NULL){
  out <- F1_Score(y_pred = data$pred, y_true = data$obs, positive = "TRUE")  
  names(out) <- "F1"
  out
}

gbmGrid <- expand.grid(interaction.depth = (1:5) * 2,
                       n.trees = (1:10)*25,
                       shrinkage = .1,
                       n.minobsinnode = 10)

gbm_fit <- train(x = (transactions %>% select(-isFraud)), 
             y = (transactions$isFraud),
             method = "gbm",
             tuneGrid = gbmGrid,
             metric = "F1",
             trControl = trainControl(method = "cv",
                                      number = 5,
                                      summaryFunction = F1,
                                      sampling = "up")
             )

plot(gbm_fit)

plot(varImp(gbm_fit))


y_hat <- predict(gbm_fit, newdata = validation %>% select(-isFraud))
F1_Score(validation$isFraud, y_hat,positive = "TRUE")

library(xgboost)
set.seed(121)
targets <- train$OutcomeType
targets_train <- as.numeric(targets)-1
train$OutcomeType<-NULL
full_train_matrix = matrix(as.numeric(data.matrix(train)),ncol=length(train))
View(full_train_matrix)
test$ID<-NULL
test_matrix <- matrix(as.numeric(data.matrix(test)),ncol=length(test))
model = xgboost(data=full_train_matrix, 
                label=targets_train, 
                nrounds=200, 
                verbose=1, 
                eta=0.3, 
                max_depth=8, 
                subsample=1, 
                colsample_bytree=0.85,
                objective="multi:softprob", 
                eval_metric="mlogloss",
                num_class=5)

test_preds <- predict(model, test_matrix)
test_preds_frame <- data.frame(matrix(test_preds, ncol = 5, byrow=TRUE))
write.csv(test_preds_frame, file="finalsubmission1.csv")
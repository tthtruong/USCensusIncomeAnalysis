### Random forest on simple learn dataset
# Number of tree was chosen after plotting error depending on number of trees (OOB error)
rf.model.basic <- randomForest(income~., data=classif.learn, importance=TRUE, ntree=20)
# Variables influencing the most the income
# Most important variables seem to be : education, capital gains, sex, and major ind code
varImpPlot(rf.model.basic)

# Application of model on learning data
set.seed(100)
rf.predict.learn.basic <- predict(rf.model.basic, classif.learn, positive='1')
confusionMatrix(classif.learn$income, rf.predict.learn.basic, positive='1')
# Error rate on learning data
errors.basics.learn <- sum(rf.predict.learn.basic != classif.learn$income)
errors.basics.learn/length(classif.learn$income) * 100
# Error rate 5.85%

# Application of model on test data
set.seed(100)
rf.predict.test.basic <- predict(rf.model.basic, classif.test, positive='1')
confusionMatrix(classif.test$income, rf.predict.test.basic, positive='1')
# Error rate on test data
errors.basics.test <- sum(rf.predict.test.basic != classif.test$income)
errors.basics.test/length(classif.test$income) * 100
# Error rate 5.90%

### Random forest on IMPROVED dataset
rf.model.impr <- randomForest(income~., data=classif.improved.learn, importance=TRUE, ntree=20)
# Variables influencing the most the income
# Most important variables seem to be : sex, major indus code, age, capital gains and education
# Interesting how age was less important on basic dataset
varImpPlot(rf.model.impr)

# Application of model on learning data
set.seed(100)
rf.predict.learn.impr <- predict(rf.model.impr, classif.improved.learn, positive='1')
confusionMatrix(classif.improved.learn$income, rf.predict.learn.impr, positive='1')
# Error rate on learning data
errors.impr.learn <- sum(rf.predict.learn.impr != classif.improved.learn$income)
errors.impr.learn/length(classif.improved.learn$income) * 100
# Error rate 1.49%, way better than on unimproved dataset

# Application of model on test data
set.seed(100)
rf.predict.test.impr <- predict(rf.model.impr, classif.improved.test, positive='1')
confusionMatrix(classif.improved.test$income, rf.predict.test.impr, positive='1')
# Error rate on test data
errors.impr.test <- sum(rf.predict.test.impr != classif.improved.test$income)
errors.impr.test/length(classif.improved.test$income) * 100
# Error rate 4.68%, also better than on unimproved dataset

### Logistic regression on improved learn dataset 
# On the var the most possibly important : sex, major indus code, age, capital gains and education
glm.model <- glm(income ~ sex + major.ind.code + age + capital.gains + education, family=binomial, 
                     data=classif.improved.learn)
summary(glm.model)

# Application on learning dataset
set.seed(100)
glm.predict.odds.learn <- predict.glm(glm.model.learn, classif.improved.learn, type="response")
# Error rate on learning data
glm.predict.learn <- ifelse(glm.predict.odds.learn > 0.5, 1, 0)
errors.learn <- sum(glm.predict.learn != classif.improved.learn$income)
errors.learn/length(classif.improved.learn$income) * 100
# Error rate 5.3%

# Application on test dataset
set.seed(100)
glm.predict.odds.test <- predict.glm(glm.model, classif.improved.test, type="response")
# Error rate on test data
glm.predict.test <- ifelse(glm.predict.odds.test > 0.5, 1, 0)
errors.test <- sum(glm.predict.test != classif.improved.test$income)
errors.test/length(classif.improved.test$income) * 100
# Error rate 5.28% (weird ?)



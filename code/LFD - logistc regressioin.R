require(foreign)
require(ggplot2)
require(MASS)
require(Hmisc)
require(reshape2)
install.packages("caret")
install.packages("Metrics")
stationary_ca_test <- read.csv("/Users/wang.c/Desktop/112-1 NCCU/lfd/ca_test.csv")
stationary_ca_train <- read.csv("/Users/wang.c/Desktop/112-1 NCCU/lfd/ca_train.csv")

#ca severity1
stationary_ca_train$Severity <- factor(stationary_ca_train$Severity, ordered = TRUE)
stationary_ca_test$Severity <- factor(stationary_ca_test$Severity, ordered = TRUE)

# Perform ordinal logistic regression
# Provide custom starting values
stationary_ca_train$Lasting_time_log <- log(stationary_ca_train$Lasting_time + 1)
stationary_ca_test$Lasting_time_log <- log(stationary_ca_test$Lasting_time + 1)
ordinal_logistic <- polr(Severity ~  Distance.mi.+ Temperature.F.+Humidity...+Pressure.in. + Visibility.mi. + Lasting_time_log , data = stationary_ca_train, Hess = TRUE, method = "logistic")
summary(ordinal_logistic)

# Assuming 'ordinal_logistic' is your fitted model and 'stationary_ca_test' is your test dataset

# Predict on the test dataset
predictions <- predict(ordinal_logistic, newdata = stationary_ca_test, type = "class")
conf_matrix <- caret::confusionMatrix(predictions, stationary_ca_test$Severity)
print(conf_matrix)
predictions <- as.numeric(predictions)


recall = recall(actual = stationary_ca_test$Severity, predicted = predictions)
print(recall)

#ca severity2
stationary_ca_train$Severity2 <- factor(stationary_ca_train$Severity2, ordered = TRUE)
stationary_ca_test$Severity2 <- factor(stationary_ca_test$Severity2, ordered = TRUE)
ordinal_logistic_ca2 <- glm(Severity2 ~  Distance.mi.+ Temperature.F.+Humidity...+Pressure.in. + Visibility.mi. + Lasting_time_log , data = stationary_ca_train,family = binomial(link = "logit"))
predictions_ca2 <- predict(ordinal_logistic_ca2, newdata = stationary_ca_test, type = "class")
conf_matrix_ca2 <- caret::confusionMatrix(predictions, stationary_ca_test$Severity2)
print(conf_matrix_ca2)

#tehama severity1
tehama_test <- read.csv("/Users/wang.c/Desktop/112-1 NCCU/lfd/Tehama_test.csv")
tehama_train <- read.csv("/Users/wang.c/Desktop/112-1 NCCU/lfd/Tehama_train.csv")
tehama_train$Severity <- factor(tehama_train$Severity, ordered = TRUE)
tehama_test$Severity <- factor(tehama_test$Severity, ordered = TRUE)
tehama_train$Lasting_time_log <- log(tehama_train$Lasting_time + 1)
tehama_test$Lasting_time_log <- log(tehama_test$Lasting_time + 1)
library(polr)
ordinal_logistic_tehama <- polr(Severity ~  Distance.mi.+ Temperature.F.+Humidity...+Pressure.in. + Visibility.mi. + Lasting_time_log , data = tehama_train, Hess = TRUE, method = "logistic")
summary(ordinal_logistic_tehama)
predictions_tehama <- predict(ordinal_logistic_tehama, newdata = tehama_test, type = "class")
conf_matrix_tehama <- caret::confusionMatrix(predictions_tehama, tehama_test$Severity)
print(conf_matrix_tehama)
#tehama severity2
tehama_train$Severity2 <- factor(tehama_train$Severity2, ordered = TRUE,levels = c(0,1))
tehama_test$Severity2 <- factor(tehama_test$Severity2, ordered = TRUE,levels = c(0,1))
ordinal_logistic_tehama2 <- glm(Severity2 ~  Distance.mi.+ Temperature.F.+Humidity...+Pressure.in. + Visibility.mi. + Lasting_time_log , data = tehama_train,family = binomial(link = "logit"))
summary(ordinal_logistic_tehama2)
predictions_tehama2 <- predict(ordinal_logistic_tehama2, newdata = tehama_test, , type = "response")
threshold <- 0.5 
# Convert probabilities to binary predictions using the threshold
predictions_tehama2 <- ifelse(predictions_tehama2  > threshold, 1, 0)
predictions_tehama2 <- factor(predictions_tehama2, ordered = TRUE,levels = c(0,1))
conf_matrix_tehama2 <- caret::confusionMatrix(predictions_tehama2, tehama_test$Severity2)
print(conf_matrix_tehama2)
install.packages("DescTools")
library(DescTools)
PseudoR2(ordinal_logistic_tehama)
PseudoR2(ordinal_logistic_tehama2)

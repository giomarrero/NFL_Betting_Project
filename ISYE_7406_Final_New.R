### Read Training Data
## Assume you save the training data in the folder "C:/temp" in your local laptop
traindata <- read.table(file = "/Users/gmarrero/Desktop/ISYE 7406/7406train.csv", sep=",");
testdata <- read.table(file = "/Users/gmarrero/Desktop/ISYE 7406/7406test.csv", sep=",");
dim(traindata);
#dim=10000*202
dim(testdata);
#dim=2500*2
#testdata

## The first two columns are X1 and X2 values, and the last 200 columns are the Y values

### Some example plots for exploratory data analysis
### please feel free to add more exploratory analysis
X1train <- traindata[,1];
X2train <- traindata[,2];
X1test <- testdata[,1];
X2test <- testdata[,2];


## compute the empirical estimation of muhat = E(Y) and Vhat = Var(Y)
muhat <- apply(traindata[,3:202], 1, mean);
Vhat <- apply(traindata[,3:202], 1, var);


## You can construct a dataframe in R that includes all crucial
## information for our exam
train_data = data.frame(X1=X1train, X2=X2train, muhat = muhat, Vhat = Vhat);
test_data = data.frame(X1 = X1test, X2=X2test);
#testdata
#test_data

traindata
train_data

testdata
test_data

## we can plot 4 graphs in a single plot
par(mfrow = c(2, 2));
plot(X1train, muhat);
plot(X2train, muhat);
plot(X1train, Vhat);
plot(X2train, Vhat);

# Assuming your dataframe is named 'df'
# Make sure to replace 'df' with the actual name of your dataframe

##### Model 1: Linear Regression Model
# Fit linear regression model
lm_model_Vhat <- lm(Vhat ~ X1 + X2, data = train_data)
lm_model_muhat <- lm(muhat ~ X1 + X2, data = train_data)

# Make predictions on the training set
#lm_predictions <- predict(lm_model, newdata = train_data)

lm_predictions_Vhat <- predict(lm_model_Vhat, newdata = test_data)
lm_predictions_muhat <- predict(lm_model_muhat, newdata = test_data)


# Calculate training error for Linear Regression
lm_train_error <- mean((lm_predictions - train_data$Vhat)^2)
print(paste("Linear Regression Training Error: ", round(lm_train_error, 2)))




##### Model 2: Decision Trees

# Fit decision tree model
library(rpart)
tree_model <- rpart(Vhat ~ X1 + X2, data = train_data)

# Make predictions on the training set
tree_predictions <- predict(tree_model, newdata = train_data)

# Calculate training error for Decision Trees
tree_train_error <- mean((tree_predictions - train_data$Vhat)^2)
print(paste("Decision Trees Training Error: ", round(tree_train_error, 2)))



##### Model 3: Random Forest Model

# Fit random forest model
library(randomForest)
rf_model_muhat <- randomForest(muhat ~ X1 + X2, data = train_data)
rf_model_Vhat <- randomForest(Vhat ~ X1 + X2, data = train_data)

# Make predictions on the training set
#rf_predictions <- predict(rf_model, newdata = train_data)
rf_predictions_muhat <- predict(rf_model_muhat, newdata = test_data)
rf_predictions_Vhat <- predict(rf_model_Vhat, newdata = test_data)


FinalData <- data.frame()
FinalData = data.frame(X1 = X1test, X2=X2test, muhat = rf_predictions_muhat, Vhat = rf_predictions_Vhat);

write.csv(FinalData, "/Users/gmarrero/Desktop/ISYE 7406/1.Marrero.Giovanni.csv", row.names=FALSE)


# Calculate training error for Random Forest
rf_train_error <- mean((rf_predictions - train_data$muhat)^2)
print(paste("Random Forest Training Error: ", round(rf_train_error, 2)))


##### Model 4: Boosting

# Load the required package
#install.packages("xgboost")
library(xgboost)

# Convert data to DMatrix format
dmatrix <- xgb.DMatrix(data = as.matrix(train_data[, c("X1", "X2")]), label = train_data$Vhat)

# Fit XGBoost model
xgb_model <- xgboost(data = dmatrix, max.depth = 3, nrounds = 100, objective = "reg:squarederror")

# Make predictions on the training set
xgb_predictions <- predict(xgb_model, as.matrix(train_data[, c("X1", "X2")]))

# Calculate training error for XGBoost
xgb_train_error <- mean((xgb_predictions - train_data$Vhat)^2)
print(paste("XGBoost Training Error: ", round(xgb_train_error, 2)))



##### Model 5: Support Vector Model
# Load the required package
#install.packages("e1071")
library(e1071)

# Fit SVR model
svr_model <- svm(Vhat ~ X1 + X2, data = train_data)

# Make predictions on the training set
svr_predictions <- predict(svr_model, newdata = train_data)

# Calculate training error for SVR
svr_train_error <- mean((svr_predictions - train_data$Vhat)^2)
print(paste("SVR Training Error: ", round(svr_train_error, 2)))


##### Model 6: Ridge Regression Model

# Load the required package
#install.packages("glmnet")
library(glmnet)

# Fit Ridge Regression model
ridge_model <- glmnet(as.matrix(train_data[, c("X1", "X2")]), train_data$Vhat, alpha = 0)

# Make predictions on the training set
ridge_predictions <- predict(ridge_model, newx = as.matrix(train_data[, c("X1", "X2")]), s = 0)

# Calculate training error for Ridge Regression
ridge_train_error <- mean((ridge_predictions - train_data$Vhat)^2)
print(paste("Ridge Regression Training Error: ", round(ridge_train_error, 2)))




##### Model 7: LASSO Regression Model

# Load the required package
#install.packages("glmnet")
library(glmnet)

# Fit Lasso Regression model
lasso_model <- glmnet(as.matrix(train_data[, c("X1", "X2")]), train_data$Vhat, alpha = 1)

# Make predictions on the training set
lasso_predictions <- predict(lasso_model, newx = as.matrix(train_data[, c("X1", "X2")]), s = 0)

# Calculate training error for Lasso Regression
lasso_train_error <- mean((lasso_predictions - train_data$Vhat)^2)
print(paste("Lasso Regression Training Error: ", round(lasso_train_error, 2)))




##### Model 8: Elastic Net Model

# Load the required package
#install.packages("glmnet")
library(glmnet)

# Fit Elastic Net Regression model
elastic_net_model <- glmnet(as.matrix(train_data[, c("X1", "X2")]), train_data$Vhat, alpha = 0.5)

# Make predictions on the training set
elastic_net_predictions <- predict(elastic_net_model, newx = as.matrix(train_data[, c("X1", "X2")]), s = 0)

# Calculate training error for Elastic Net Regression
elastic_net_train_error <- mean((elastic_net_predictions - train_data$Vhat)^2)
print(paste("Elastic Net Regression Training Error: ", round(elastic_net_train_error, 2)))





##### Model 9: KNN Regression Model

# Load the required package
install.packages("rlang")
install.packages("ggplot2")
install.packages("caret")
library(rlang)
library(ggplot2)
library(caret)


# Fit KNN Regression model
knn_model <- train(Vhat ~ X1 + X2, data = train_data, method = "knn", preProcess = c("center", "scale"))

# Make predictions on the training set
knn_predictions <- predict(knn_model, newdata = train_data)

# Calculate training error for KNN Regression
knn_train_error <- mean((knn_predictions - train_data$Vhat)^2)
print(paste("KNN Regression Training Error: ", round(knn_train_error, 2)))



##### Model 10: Polynomial Regression Model

# Fit Polynomial Regression model (degree = 2 as an example)
poly_model <- lm(Vhat ~ poly(X1, 5) + poly(X2, 5), data = train_data)

# Make predictions on the training set
poly_predictions <- predict(poly_model, newdata = train_data)

# Calculate training error for Polynomial Regression
poly_train_error <- mean((poly_predictions - train_data$Vhat)^2)
print(paste("Polynomial Regression Training Error: ", round(poly_train_error, 2)))



##### Model 11: LOESS Regression Model

# Fit LOESS model
loess_model <- loess(Vhat ~ X1 + X2, data = train_data)

# Make predictions on the training set
loess_predictions <- predict(loess_model, newdata = train_data)

# Calculate training error for LOESS
loess_train_error <- mean((loess_predictions - train_data$Vhat)^2)
print(paste("LOESS Training Error: ", round(loess_train_error, 2)))



##### Model 12: Generalize Additive  Model

# Load the required package
#install.packages("mgcv")
library(mgcv)

# Fit GAM model
gam_model <- gam(Vhat ~ s(X1) + s(X2), data = train_data)

# Make predictions on the training set
gam_predictions <- predict(gam_model, newdata = train_data)

# Calculate training error for GAM
gam_train_error <- mean((gam_predictions - train_data$Vhat)^2)
print(paste("GAM Training Error: ", round(gam_train_error, 2)))



##### Model 13: Neural Net Model

# Load the required package
#install.packages("neuralnet")
library(neuralnet)

# Combine the predictors X1 and X2 into a data frame
nn_data <- data.frame(X1 = train_data$X1, X2 = train_data$X2)

# Combine predictors and response variable into a single data frame
nn_data <- cbind(nn_data, Vhat = train_data$Vhat)

# Split the data into training and testing sets
set.seed(123)
train_indices <- sample(1:nrow(nn_data), 0.8 * nrow(nn_data))
train_data <- nn_data[train_indices, ]
test_data <- nn_data[-train_indices, ]

# Define the neural network model
nn_model <- neuralnet(
  Vhat ~ X1 + X2,
  data = train_data,
  hidden = c(2, 3),  # Number of nodes in hidden layers (you can adjust)
  linear.output = TRUE
)

# Make predictions on the training set
nn_predictions <- predict(nn_model, newdata = train_data)

# Calculate training error for Neural Network
nn_train_error <- mean((nn_predictions - train_data$Vhat)^2)
print(paste("Neural Network Training Error: ", round(nn_train_error, 2)))


##### Model 14: Neural Net Model w/ Scaling
### TRY SCALING TOO

# Normalize X1 and X2
train_data$X1 <- scale(train_data$X1)
train_data$X2 <- scale(train_data$X2)

# Continue with the neural network model and training as before

# Assuming your dataframe is named 'df'
# Make sure to replace 'df' with the actual name of your dataframe

# Load the required package
install.packages("neuralnet")
library(neuralnet)

# Combine the predictors X1 and X2 into a data frame
nn_data_scale <- data.frame(X1 = train_data$X1, X2 = train_data$X2)

# Combine predictors and response variable into a single data frame
nn_data_scale <- cbind(nn_data_scale, Vhat = train_data$Vhat)

# Split the data into training and testing sets
set.seed(123)
train_indices_scale <- sample(1:nrow(nn_data_scale), 0.8 * nrow(nn_data_scale))
train_data_scale <- nn_data_scale[train_indices_scale, ]
test_data_scale <- nn_data_scale[-train_indices_scale, ]

# Define the neural network model
nn_model <- neuralnet(
  Vhat ~ X1 + X2,
  data = train_data_scale,
  hidden = c(5, 3),  # Number of nodes in hidden layers (you can adjust)
  linear.output = TRUE
)

# Make predictions on the training set
nn_predictions_scale <- predict(nn_model_scale, newdata = train_data_scale)

# Calculate training error for Neural Network
nn_train_error_scale <- mean((nn_predictions_scale - train_data_scale$muhat)^2)
print(paste("Neural Network Training Error: ", round(nn_train_error, 2)))

rf_predictions
##### Required -----------------------------------------------------------------------------------
library(dplyr)
library(glmnet)
library(glmnetUtils)
library(caret)
library(splines)
library(gam)
library(mgcv)
library(keras)

rescale <- function(x, new_min = 0, new_max = 1) {
  (x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm = T)) * 
    (new_max - new_min) + new_min
}

lagk <- function(x, k) {
  temp <- NA
  for ( i in 1:k) {
    data.frame(x) %>%
      transmute_all(eval(parse(text = paste0('funs(lag_', i, ' = lag(.,', i, '))')))) %>%
      data.frame(temp, .) -> temp
  }
  x_lag <- data.frame(x, temp[, -1])[-1:-k, ]
  return(x_lag)
}

weekdaymean <- function(data, nweeks) {
  sapply(0:(nweeks - 1), function(x) {mean(data[1:5 + x * 5])})
}

##### Data Setting -------------------------------------------------------------------------------
X <- na.omit(read.csv("path/to/file/data.csv", skip = 6,
                      col.names = c("Date", "V2", "V3", "V4", "V5",
                                    "V6", "V7", "V8", "V9")))
attributes(X)$na.action <- NULL

train.x <- X[X$Date > 20160129 & X$Date <= 20180817, -1:-2]
train.y <- X[X$Date > 20160129 & X$Date <= 20180824, ][-1:-5, 2]
test.x <- X[X$Date > 20160129 & X$Date <= 20180824 & X$Date > 20180817, -1:-2]

### lag 10 days(2 weeks)
train.x.lag <- lagk(train.x, 9)
train.y.lag <- train.y[-1:-9]

##### Multiple Linear Regression -----------------------------------------------------------------
train.x.lag.scale <- train.x.lag %>% scale

set.seed(777)
model.mlr <- lm(train.y.lag %>% log ~ ., data = train.x.lag.scale %>% data.frame)

pred.mlr <- exp(predict(model.mlr, train.x.lag.scale %>% data.frame))

##### Elastic-Net --------------------------------------------------------------------------------
lambda.grid <- 10 ^ seq(2, -2, length = 100)
alpha.grid <- seq(0, 1, length = 11)
srchGrd <- expand.grid(.alpha = alpha.grid, .lambda = lambda.grid)

trnCtrl <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 5,
  verboseIter = T
)

set.seed(777)
model.elastic <- train(x = train.x.lag,
                       y = train.y.lag %>% log,
                       method = "glmnet",
                       tuneGrid = srchGrd,
                       trControl = trnCtrl,
                       metric = "RMSE",
                       standardize = TRUE,
                       maxit = 1000000)

pred.elastic <- predict(model.elastic, train.x.lag) %>% exp

##### Generalized Additive Model -----------------------------------------------------------------
predictor <- paste(paste0("s(", colnames(train.x.lag), ")"), collapse = ' + ')
form <- formula(paste("log(train.y.lag)", predictor, sep = " ~ "))

set.seed(777)
model.gam <- gam(formula = form , data = train.x.lag %>% data.frame)

pred.gam <- predict(model.gam, train.x.lag %>% data.frame) %>% exp

##### RNN ----------------------------------------------------------------------------------------
source("myrnn.R") # This function is uploaded in github repository: https://github.com/Gwangil/myrnn

# predict rnn
set.seed(777)
model.rnn <- myrnn(x = train.x.lag %>% apply(2, rescale),
                   y = train.y.lag %>% log %>% rescale,
                   hidden = 100,
                   learningRate = 0.0001,
                   epoch = 10000,
                   batch.size = 128,
                   loss = "Elastic",
                   activator = "tanh",
                   init.weight = NULL,
                   init.dist = "Xavier",
                   optimizer = "adam",
                   dropout = 0.2,
                   dropconnect = 0.2,
                   validation = 0.2,
                   plotting = T)

pred.rnn <-
  exp(
    (max(train.y.lag %>% log) - min(train.y.lag %>% log)) * 
      predict.myrnn(model.rnn, train.x.lag %>% apply(2, rescale)) + 
      min(train.y.lag %>% log)
  )

##### Boosting -----------------------------------------------------------------------------------
lambda.grid <- 10 ^ seq(2, -2, length = 100)
alpha.grid <- seq(0, 1, length = 11)
srchGrd <- expand.grid(.alpha = alpha.grid,
                       .lambda = lambda.grid,
                       .nrounds = 1000,
                       .eta = 0.0001)

trnCtrl <- trainControl(
  method = "repeatedcv",
  number = 10,
  repeats = 5,
  verboseIter = T
)

set.seed(777)
model.xgbLinear <- train(x = train.x.lag.scale,
                         y = train.y.lag %>% log,
                         method = "xgbLinear",
                         tuneGrid = srchGrd,
                         trControl = trnCtrl,
                         metric = "RMSE")

pred.xgbLinear <- predict(model.xgbLinear, train.x.lag.scale) %>% exp

##### Ensemble -----------------------------------------------------------------------------------
set.seed(777)
x.ensemble <- data.frame(pred.mlr, pred.gam, pred.elastic, pred.xgbLinear, pred.rnn)
x.ensemble.lag <- x.ensemble %>% lagk(9)

inputs <- layer_input(shape = ncol(x.ensemble.lag), name = "Input_Layer")
outputs <- inputs %>%
  layer_dropout(rate = 0.1, name = "Input_to_Hidden_Dropout") %>%
  layer_dense(units = 30,
              kernel_initializer = "glorot_uniform", name = "Fully-connected_1") %>%
  layer_batch_normalization(name = "Batch_Normalization_1") %>%
  layer_activation_leaky_relu(name = "LeakyReLU_Activattion_1") %>%
  layer_dropout(rate = 0.2, name = "Hidden_to_Hidden_Dropout_1") %>%
  layer_dense(units = 15, name = "Fully-connected_2") %>%
  layer_batch_normalization(name = "Batch_Normalization_2") %>%
  layer_activation_leaky_relu(name = "LeakyReLU_Activation_2") %>%
  layer_dropout(rate = 0.2, name = "Hidden_to_Hidden_Dropout_2") %>%
  layer_dense(units = 8, name = "Fully-connected_3") %>%
  layer_batch_normalization(name = "Batch_Normalization_3") %>%
  layer_activation_leaky_relu(name = "LeakyReLU_Activation_3") %>%
  layer_dense(units = 1, activation = "linear", name = "Linear_Output_Layer")
model <- keras_model(inputs, outputs)

loss_mean_elastic_error <- function(y_true, y_pred) {
  lambda <- 0.5
  res <- k_mean(lambda * (y_true - y_pred) ^ 2 + (1 - lambda) * k_abs(y_true - y_pred))
  return(res)
}

model %>% compile(
  optimizer = optimizer_adam(lr = 0.0001),
  loss = loss_mean_elastic_error,
  metrics = metric_mean_squared_error
)

history <- model %>% fit(
  x = x.ensemble.lag %>% apply(2, rescale),
  y = train.y.lag[-1:-9] %>% log,
  batch_size = 128,
  epochs = 10000,
  validation_split = 0.2,
  callbacks = callback_early_stopping(monitor = "val_loss",
                                      min_delta = 0,
                                      patience = 1000,
                                      verbose = 1),
  view_metrics = F
)

##### Predict ------------------------------------------------------------------------------------
test.x.lag <- lagk(rbind(train.x %>% tail(9 + 9), test.x), 9)
# MLR
test.x.lag.scale <- t((t(test.x.lag) -
                         attr(train.x.lag.scale, "scaled:center")) /
                        attr(train.x.lag.scale, "scaled:scale")) %>% data.frame

test.mlr <- exp(predict(model.mlr, test.x.lag.scale))

# ElasticNet
test.elastic <- exp(predict(model.elastic, test.x.lag))

# GAM
test.gam <- exp(predict(model.gam, test.x.lag %>% data.frame))

# Boosting
test.xgbLinear <- exp(predict(model.xgbLinear, test.x.lag.scale))

# Rnn
colmin <- apply(train.x.lag, 2, min) %>% rep(5 + 9) %>% matrix(., nrow = 5 + 9, byrow = T)
colmax <- apply(train.x.lag, 2, max) %>% rep(5 + 9) %>% matrix(., nrow = 5 + 9, byrow = T)
test.x.lag.normal <- as.matrix((test.x.lag - colmin) / (colmax - colmin))

test.rnn <-
  exp(
    (max(train.y.lag %>% log) - min(train.y.lag %>% log)) * 
      predict.myrnn(model.rnn, test.x.lag.normal) + 
      min(train.y.lag %>% log)
  )

# Ensemble
test.ensemble <- data.frame(test.mlr, test.elastic, test.gam, test.xgbLinear, test.rnn)

colmin <- apply(x.ensemble.lag, 2, min) %>% rep(5) %>% matrix(., nrow = 5, byrow = T)
colmax <- apply(x.ensemble.lag, 2, max) %>% rep(5) %>% matrix(., nrow = 5, byrow = T)
test.ensemble.normal <- as.matrix((lagk(test.ensemble, 9) - colmin) / (colmax - colmin))

pred.ensembel <- exp(predict(model, test.ensemble.normal))
pred.final <- weekdaymean(pred.ensembel, 1) %>% round(2)

#write.csv(pred.final, "pred_final.csv")

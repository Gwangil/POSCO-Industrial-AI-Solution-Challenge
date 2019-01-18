##### Required -----------------------------------------------------------------------------------
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(glmnet))
suppressPackageStartupMessages(library(glmnetUtils))
suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(rpart))
suppressPackageStartupMessages(library(naivebayes))
suppressPackageStartupMessages(library(xgboost))
suppressPackageStartupMessages(library(fastAdaboost))
suppressPackageStartupMessages(library(randomForest))
suppressPackageStartupMessages(library(ROCR))
suppressPackageStartupMessages(library(keras))
suppressPackageStartupMessages(library(tidyverse))

rescale <- function(x, new_min = 0, new_max = 1) {
  (x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm = T)) * (new_max - new_min) + new_min
}

##### Make Target Data ---------------------------------------------------------------------------
Swell <- data.frame("time" = seq(from = ISOdatetime(year = 2014, month = 01, day = 01,
                                                    hour = 0, min = 0, sec = 0),
                                 to = ISOdatetime(year = 2017, month = 12, day = 31,
                                                  hour = 23, min = 59, sec = 59),
                                 by = "hour"),
                    "swell" = 0)

# The data ontained in Swell is security concern.

##### Load Marine Cwbuoies from Meteorological office---------------------------------------------
# grp : 구룡포
path <- "data/파고부이"
cwbuoies <- list.files(path, pattern = "csv")
for (i in seq_along(cwbuoies)) {
  X <- fread(paste(path, cwbuoies[i], sep = "/"),
             drop = 1,
             col.names = c("time", "water.temp", "max.crest",
                           "sign.high", "avg.high", "wave.period"))
  X[[1]] <- as.POSIXct(X[[1]])
  assign(x = str_sub(cwbuoies[i], 1, -5),
         value = X)
};rm(path, cwbuoies, i, X)

##### Load Marine Buoies from Meteorological office---------------------------------------------
# ph : 포항
path <- "data/해양기상부이"
buoies <- list.files(path, pattern = "csv")
for (i in seq_along(buoies)) {
  X <- fread(paste(path, buoies[i], sep = "/"),
             drop = 1,
             col.names = c("time", "wind.speed", "wind.directtion", "gust.speed",
                           "atmosphere", "humidity", "temperature", "water.temp.marine",
                           "max.high.marine", "sign.high.marine", "avg.high.marine",
                           "wave.period.marine", "wave.direction.marine"))
  X[[1]] <- as.POSIXct(X[[1]])
  assign(x = str_sub(buoies[i], 1, -5),
         value = X)
};rm(path, buoies, i, X)

##### Additional Data - now use grp, ph------------------------------------------------------------
grp2014[which(!grp2014$time %in% seq(from = ISOdatetime(year = 2014, month = 01, day = 01,
                                                        hour = 0, min = 0, sec = 0),
                                     to = ISOdatetime(year = 2014, month = 12, day = 31,
                                                      hour = 23, min = 59, sec = 59),
                                     by = "hour")), ]
grp2014[3280, 1] <- as.POSIXct("2014-05-21 16:00")
grp2014[3281, 1] <- as.POSIXct("2014-05-21 17:00")

grp2015[which(!grp2015$time %in% seq(from = ISOdatetime(year = 2015, month = 01, day = 01,
                                                        hour = 0, min = 0, sec = 0),
                                     to = ISOdatetime(year = 2015, month = 12, day = 31,
                                                      hour = 23, min = 59, sec = 59),
                                     by = "hour")), ]

grp2016[which(!grp2016$time %in% seq(from = ISOdatetime(year = 2016, month = 01, day = 01,
                                                        hour = 0, min = 0, sec = 0),
                                     to = ISOdatetime(year = 2016, month = 12, day = 31,
                                                      hour = 23, min = 59, sec = 59),
                                     by = "hour")), ]

grp2017[which(!grp2017$time %in% seq(from = ISOdatetime(year = 2017, month = 01, day = 01,
                                                        hour = 0, min = 0, sec = 0),
                                     to = ISOdatetime(year = 2017, month = 12, day = 31,
                                                      hour = 23, min = 59, sec = 59),
                                     by = "hour")), ]
grp <- rbind(grp2014, grp2015, grp2016, grp2017);rm(grp2014, grp2015, grp2016, grp2017)
Swell <- merge(Swell, grp, by = "time", all.x = T);rm(grp);rm(wp2015, wp2016, wp2017)

ph2014[which(!ph2014$time %in% seq(from = ISOdatetime(year = 2014, month = 01, day = 01,
                                                      hour = 0, min = 0, sec = 0),
                                   to = ISOdatetime(year = 2014, month = 12, day = 31,
                                                    hour = 23, min = 59, sec = 59),
                                   by = "hour")), ]
ph2015[which(!ph2015$time %in% seq(from = ISOdatetime(year = 2015, month = 01, day = 01,
                                                      hour = 0, min = 0, sec = 0),
                                   to = ISOdatetime(year = 2015, month = 12, day = 31,
                                                    hour = 23, min = 59, sec = 59),
                                   by = "hour")), ]
ph2016[which(!ph2016$time %in% seq(from = ISOdatetime(year = 2016, month = 01, day = 01,
                                                      hour = 0, min = 0, sec = 0),
                                   to = ISOdatetime(year = 2016, month = 12, day = 31,
                                                    hour = 23, min = 59, sec = 59),
                                   by = "hour")), ]
ph2017[which(!ph2017$time %in% seq(from = ISOdatetime(year = 2017, month = 01, day = 01,
                                                      hour = 0, min = 0, sec = 0),
                                   to = ISOdatetime(year = 2017, month = 12, day = 31,
                                                    hour = 23, min = 59, sec = 59),
                                   by = "hour")), ]

ph <- rbind(ph2014, ph2015, ph2016, ph2017);rm(ph2014, ph2015, ph2016, ph2017)
Swell <- merge(Swell, ph, by = "time", all.x = T);rm(ph);rm(wj2015, wj2016, wj2017)

##### Interpolation ------------------------------------------------------------------------------
Swell[, -1:-2] <- sapply(Swell[, -1:-2], function(x) approx(x, n = length(x))$y)

##### assign test set ----------------------------------------------------------------------------
attach(Swell)
Swell[{"condition of test set"}, ] -> FinalPrediction

Swell[!{"condition of test set"}, ] -> Training
detach(Swell)

##### Case Imbalancing : over sampling lower case ------------------------------------------------
Training[Training$swell == 1, ] -> Training.1
Training[Training$swell == 0, ] -> Training.0
Training.rep <- rbind(Training.0,
                      Training.1[sample(1:nrow(Training.1), nrow(Training.0), replace = T), ]) %>%
  arrange(time);rm(Training, Training.1, Training.0)
Training.rep[, 2] <- as.factor(Training.rep[, 2])

##### Validation Split ---------------------------------------------------------------------------
set.seed(777)
idx <- sample(1:63754, 63754 * 0.2)

Valid.rep <- Training.rep[idx, ]
Training.rep <- Training.rep[-idx, ]

##### Elastic : Logistic Regression --------------------------------------------------------------
lambda.grid <- 10 ^ seq(2, -2, length = 100)
alpha.grid <- seq(0, 1, length = 11)
srchGrd <- expand.grid(.alpha = alpha.grid, .lambda = lambda.grid)

trnCtrl <- trainControl(
  method = "cv",
  number = 10,
  verboseIter = T
)

set.seed(777)
model.elastic <- train(swell ~ .,
                       data = Training.rep[, -1],
                       method = "glmnet",
                       family = "binomial",
                       tuneGrid = srchGrd,
                       trControl = trnCtrl,
                       metric = "Accuracy",
                       standardize = TRUE,
                       maxit = 1000000)

pred.elastic <- predict(model.elastic, Valid.rep, type = "prob")[, 2]

##### Decision Tree ------------------------------------------------------------------------
# modelLookup('rpart')
srchGrd <- expand.grid(.cp = seq(0, 0.05, 0.005))

trnCtrl <- trainControl(
  method = "cv",
  number = 10,
  verboseIter = T
)

model.cart <- train(swell ~ .,
                    data = Training.rep[, -1],
                    method = "rpart",
                    tuneGrid = srchGrd,
                    trControl = trnCtrl,
                    metric = "Accuracy")

pred.cart <- predict(model.cart, Valid.rep, type = "prob")[, 2]

##### AdaBoost.M1 --------------------------------------------------------------------------------
# modelLookup('adaboost')
srchGrd <- expand.grid(.mfinal = c(500, 1000),
                       .maxdepth = c(4, 5, 6),
                       .coeflearn = "Zhu") # Zhu : SAMME, Breiman : default

trnCtrl <- trainControl(
  method = "cv",
  number = 10,
  verboseIter = T
)

model.adaboost <- train(swell ~ .,
                        data = Training.rep[, -1],
                        method = 'AdaBoost.M1',
                        tuneGrid = srchGrd,
                        trControl = trnCtrl,
                        metric = "Accuracy")

pred.adaboost <- predict(model.adaboost, Valid.rep, type = "prob")[, 2]

##### Random Forest ------------------------------------------------------------------------------
# starttime <- Sys.time()
# set.seed(777)
# group <- sample(1:10, nrow(Training.rep), replace = T)
# AUC.test <- Accuracy <- MCE <- array(NA, c(5, 2, 10))
# Precision <- Sensitivity <- Specificity <- F1score <- array(NA, c(5, 2, 10))
# Alpah <- Beta <- array(NA, c(5, 2, 10))
# ntr <- c(500, 1000)#, 2000, 3000, 4000, 5000, 7000, 10000)
# 
# for (k in 1:10) {
#   for (j in 1:2) {
#     for (i in 1:5) {
#       model.rf <- randomForest(x = Training.rep[group != k, -1:-2],
#                                y = Training.rep[group != k, 2],
#                                ntree = ntr[j],
#                                mtry = (4:8)[i])
#       
#       pred <- predict(model.rf, Training.rep[group == k, -1:-2])
#       test.y <- Training.rep[group == k, 2]
#       
#       AUC.test[i, j, k] <- performance(prediction(pred %>% as.numeric,
#                                                   test.y), "auc")@y.values[[1]]
#       Accuracy[i, j, k] <- mean(pred == test.y)
#       MCE[i, j, k] <- mean(pred != test.y)
#       Precision[i, j, k] <- sum(pred == 0 & test.y == 0) / sum(pred == 0)
#       Sensitivity[i, j, k] <- sum(pred == 0 & test.y == 0) / sum(test.y == 0)
#       Specificity[i, j, k] <- sum(pred == 1 & test.y == 1) / sum(test.y == 1)
#       F1score[i, j, k] <- 2 / ((1 / Sensitivity[i, j, k]) + (1 / Precision[i, j, k]))
#       Alpah[i, j, k] <- sum(pred == 0 & test.y == 1)
#       Beta[i, j, k] <- sum(pred == 1 & test.y == 0)
#       
#       cat('\n========== mtry:', (4:8)[i], "========== ntree:", c(500, 1000)[j], "==========",
#           '\nAccuracy:', Accuracy[i, j, k], '\t\tMCE:', MCE[i, j, k],
#           "\nSensitivity:", Sensitivity[i, j, k], "\tSpecificity:", Specificity[i, j, k],
#           "\nPrecision:", Precision[i, j, k], "\t\tF1score:", F1score[i, j, k],
#           "\nAlpha:", Alpah[i, j, k], "\t\tBeta:", Beta[i, j, k],
#           
#           "\nAUC:", AUC.test[i, j, k])
#     }
#   }
# }
# endtime <- Sys.time()
# 
# win.graph()
# matplot(apply(AUC.test, c(1, 2), mean), type = 'l', lty = 1, main = "AUC for Test Set",
#         lwd = 2, xlab = "mtry", ylab = "AUC", col = 1:2, xaxt = 'n')
# axis(1, labels = 4:8, at = 1:5)
# legend("topright", legend = paste0("ntree : ", c(500, 1000)),
#        col = 1:2, lwd = 2, ncol = 2)
# savePlot("AUC_10fold", type = "png")
# endtime- starttime

# Best RF ::: mtry : 4, ntree : 1000 thruogh all creteria
model.randomforest <- randomForest(swell ~ .,
                               data = Training.rep[, -1],
                               ntree = 1000,
                               mtry = 4)

pred.randomforest <- predict(model.randomforest, Valid.rep, type = "prob")[, 2]
attributes(pred.randomforest)$names <- NULL

##### LDA ----------------------------------------------------------------------------------------
trnCtrl <- trainControl(
  method = "cv",
  number = 10,
  verboseIter = T
)

model.lda <- train(swell ~ .,
                   data = Training.rep[, -1],
                   method = "lda",
                   metric = "Accuracy",
                   trControl = trnCtrl,
                   standardize = TRUE,
                   maxit = 1000000)

pred.lda <- predict(model.lda, Valid.rep, type = "prob")[, 2]

##### QDA ----------------------------------------------------------------------------------------
trnCtrl <- trainControl(
  method = "cv",
  number = 10,
  verboseIter = T
)

model.qda <- train(swell ~ .,
                   data = Training.rep[, -1],
                   method = "qda",
                   metric = "Accuracy",
                   trControl = trnCtrl,
                   standardize = TRUE,
                   maxit = 1000000)

pred.qda <- predict(model.qda, Valid.rep, type = "prob")[, 2]

##### Navie Bayes --------------------------------------------------------------------------------
# modelLookup('naive_bayes')
laplace <- c(0, 10 ^ seq(2, -2, length = 11))
usekernel <- c(TRUE, FALSE)
adjust <- 0:5
srchGrd <- expand.grid(.laplace = laplace, .usekernel = usekernel, .adjust = adjust)

trnCtrl <- trainControl(
  method = "cv",
  number = 10,
  verboseIter = T
)

model.naivebayes <- train(swell ~ .,
                          data = Training.rep[, -1],
                          method = "naive_bayes",
                          tuneGrid = srchGrd,
                          trControl = trnCtrl,
                          metric = "Accuracy",
                          standardize = TRUE,
                          maxit = 1000000)

pred.naivebayes <- predict(model.naivebayes, Valid.rep, type = "prob")[, 2]

##### SVM with Linear Kernel ---------------------------------------------------------------------
# modelLookup('svmLinear')
srchGrd <- expand.grid(.C = 2 ^ seq(0, 5))

trnCtrl <- trainControl(
  method = "cv",
  number = 10,
  verboseIter = T
)

model.svm <- train(swell ~ .,
                   data = Training.rep[, -1],
                   method = "svmLinear",
                   tuneGrid = srchGrd,
                   trControl = trnCtrl,
                   metric = "Accuracy",
                   standardize = TRUE,
                   maxit = 1000000)

pred.svm <- predict(model.svm, Valid.rep) %>% as.numeric - 1

##### XGBoost ------------------------------------------------------------------------------------
# trnCtrl <- trainControl(
#   method = "repeatedcv",
#   number = 10,
#   repeats = 5,
#   verboseIter = T
# )
# 
# srchGrd <- expand.grid(nrounds = c(1000, 2000, 3000, 4000, 5000),
#                        eta = c(0.01, 0.005, 0.001),
#                        max_depth = c(4, 5, 6),
#                        gamma = 0.01,
#                        colsample_bytree = 0.8,
#                        subsample = 0.8,
#                        min_child_weight = 0.8)
# 
# starttime <- Sys.time()
# model.xgb <- train(x = Training.rep[, -1:-2] %>% as.matrix,
#                    y = Training.rep[, 2] %>% as.matrix,
#                    method = 'xgbTree',
#                    metric = "Accuracy",
#                    trControl = trnCtrl,
#                    tuneGrid = srchGrd,
#                    nthread = 4,
#                    eval_metric = "auc",
#                    alpha = 0.5,
#                    lambda = 0.5,
#                    objective = "binary:logistic",
#                    verbose = 0)
# endtime <- Sys.time()
# endtime - starttime

set.seed(777)
model.xgboost <- xgboost(data = Training.rep[, -1:-2] %>% as.matrix,
                           label = Training.rep[, 2] %>% as.matrix,
                           nrounds = 5000,
                           max_depth = 6,
                           eta = 0.01,
                           colsample_bytree = 0.8,
                           min_child_weight = 0.8,
                           subsample = 0.8,
                           nthread = 4,
                           alpha = 0.5,
                           lambda = 0.5,
                           gamma = 0.01,
                           objective = "binary:logistic")

pred.xgb <- predict(model.xgboost, Valid.rep[, -1:-2] %>% as.matrix)

##### Ensemble -----------------------------------------------------------------------------------
x.ensemble <- cbind(pred.elastic, pred.cart, pred.adaboost,
                    pred.randomforest, pred.lda, pred.qda,
                    pred.naivebayes, pred.svm,
                    pred.xgb)

inputs <- layer_input(shape = ncol(x.ensemble), name = "Input_Layer")

outputs <- inputs %>%
  layer_dense(units = 5,
              kernel_initializer = "glorot_uniform", name = "Fully-connected_1") %>%
  layer_batch_normalization(name = "Batch_Normalization_1") %>%
  layer_activation_leaky_relu(name = "LeakyReLU_Activation_1") %>%
  layer_dense(units = 2, activation = "softmax", name = "Softmax_Output_Layer")

model <- keras_model(inputs, outputs)
summary(model)

model %>% compile(
  optimizer = optimizer_adam(lr = 0.0001),
  loss = loss_binary_crossentropy,
  metrics = metric_binary_accuracy
)

set.seed(777)
history <- model %>% fit(
  x = x.ensemble,
  y = to_categorical(Valid.rep$swell %>% as.numeric - 1),
  batch_size = 128,
  epochs = 5000,
  validation_split = 0.2,
  callbacks = callback_early_stopping(monitor = "val_loss",
                                      min_delta = 0,
                                      patience = 500,
                                      verbose = 1),
  view_metrics = F
)

win.graph();plot(history)
savePlot("swellensemblehistory.png", type = "png");graphics.off()
save.image("swellensemble.RData")

### ensemble 2
x.ensemble2 <- cbind(x.ensemble, Valid.rep[, -1:-2] %>% apply(2, rescale))
inputs <- layer_input(shape = ncol(x.ensemble2), name = "Input_Layer")
outputs <- inputs %>%
  layer_dropout(rate = 0.2, name = "Input_to_Hidden_Dropout") %>%
  layer_dense(units = 15,
              kernel_initializer = "glorot_uniform", name = "Fully-connected_1") %>%
  layer_batch_normalization(name = "Batch_Normalization_1") %>%
  layer_activation_leaky_relu(name = "LeakyReLU_Activation_1") %>%
  layer_dropout(rate = 0.2, name = "Hidden_to_Hidden_Dropout_1") %>%
  layer_dense(units = 8, name = "Fully-connected_2") %>%
  layer_activation_leaky_relu(name = "LeakyReLU_Activation_2") %>%
  layer_dense(units = 2, activation = 'softmax', name = "Softmax_Output_Layer")

model2 <- keras_model(inputs, outputs)
summary(model2)

model2 %>% compile(
  optimizer = optimizer_adam(lr = 0.0001),
  loss = loss_binary_crossentropy,
  metrics = metric_binary_accuracy
)

set.seed(777)
history2 <- model2 %>% fit(
  x = x.ensemble2,
  y = to_categorical(Valid.rep$swell %>% as.numeric - 1),
  batch_size = 128,
  epochs = 5000,
  validation_split = 0.2,
  callbacks = callback_early_stopping(monitor = "val_loss",
                                      min_delta = 0,
                                      patience = 500,
                                      verbose = 1),
  view_metrics = F
)

win.graph();plot(history2)
savePlot("swellensemblehistory2.png", type = "png");graphics.off()
save.image("swellensemble_2.RData")

##### Predict --------------------------------------------------------------------------------------

test.elastic <- predict(model.elastic, FinalPrediction, type = "prob")[, 2]
test.cart <- predict(model.cart, FinalPrediction, type = "prob")[, 2]
test.adaboost <- predict(model.adaboost, FinalPrediction, type = "prob")[, 2]
test.randomforest <- predict(model.randomforest, FinalPrediction, type = "prob")[, 2]
test.lda <- predict(model.lda, FinalPrediction, type = "prob")[, 2]
test.qda <- predict(model.qda, FinalPrediction, type = "prob")[, 2]
test.naivebayes <- predict(model.naivebayes, FinalPrediction, type = "prob")[, 2]
test.svm <- predict(model.svm, FinalPrediction) %>% as.numeric - 1
test.xgb <- predict(model.xgboost, FinalPrediction[, -1:-2] %>% as.matrix)

test.ensemble <- cbind(test.elastic, test.cart, test.adaboost,
                       test.randomforest, test.lda, test.qda,
                       test.naivebayes, test.svm,
                       test.xgb)

pred.ensemble <- predict(model, test.ensemble)

test.ensemble2 <- cbind(test.ensemble, FinalPrediction[, -1:-2] %>% apply(2, rescale))
pred.ensemble2 <- predict(model2, test.ensemble2)

cbind(pred.ensemble,
      'ensem' = (pred.ensemble[, 2] > pred.ensemble[, 1]) * 1,
      FinalPrediction$time,
      'ensem_x' = (pred.ensemble2[, 2] > pred.ensemble2[, 1]) * 1,
      pred.ensemble2,
      FinalPrediction[, -1:-2]) %>% write.csv(file = 'ensemblecompare.csv')

##### Make Answer Sheet --------------------------------------------------------------------------
answer <- matrix((pred.ensemble[, 2] > pred.ensemble[, 1]) * 1, nrow = 25, ncol = 24, byrow = T)
rownames(answer) <- unique(strftime(FinalPrediction$time, format = "%Y-%m-%d"))[seq(1, 50, 2)]
colnames(answer) <- paste(strftime(FinalPrediction$time, format = "%R") %>% unique(),
                          strftime(FinalPrediction$time + 3600, format = "%R") %>% unique(),
                          sep = " ~ ")
write.csv(answer, "answer1.csv")

pred.final <- predict(model2, FinalPrediction)
data.frame(FinalPrediction$time, pred.final)
answer <- matrix(pred.final, nrow = 25, ncol = 24, byrow = T)
rownames(answer) <- unique(strftime(FinalPrediction$time, format = "%Y-%m-%d"))[seq(1, 50, 2)]
colnames(answer) <- paste(strftime(FinalPrediction$time, format = "%R") %>% unique(),
                          strftime(FinalPrediction$time + 3600, format = "%R") %>% unique(),
                          sep = " ~ ")
write.csv(answer, "answer2.csv")

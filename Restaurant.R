##### Required --------------------------------------------------------------------------------------
library(readxl)
library(dplyr)
library(stringr)
library(keras)
library(tidyverse)

rescale <- function(x, new_min = 0, new_max = 1) {
  (x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm = T)) * (new_max - new_min) + new_min
}

##### Data Load ----------------------------------------------------------------------------------
Meals <- read_xlsx(path = "path/to/provided/count/data.xlsx",
                   sheet = 2)[, -1]
Menu <- read_xlsx(path = "path/to/provided/menu/data.xlsx",
                  sheet = 3)[, -1]

##### Data Preprocessing -------------------------------------------------------------------------
### Menu
Menu[Menu$식사명 == "아침식사2", ]$식사명 <- "아침식사"

lapply(Menu$식사내용 %>% str_split(","), str_replace, pattern = "\\(.*", replacement = "") %>%
  lapply(str_replace, pattern = "\\d+", replacement = "") %>% 
  lapply(str_replace, pattern = "반마리", replacement = "") %>% 
  lapply(str_replace, pattern = ".\\)$", replacement = "") %>% 
  lapply(str_replace, pattern = "모듬$", replacement = "") %>% 
  lapply(str_replace, pattern = "새$", replacement = "") %>% 
  lapply(str_replace, pattern = "맛$", replacement = "") %>%
  unlist %>% unique %>% as.factor -> menuList

preprocessing <- function(string) {
  string %>% str_split(",") %>% unlist %>%  str_replace(pattern = "\\(.*", replacement = "") %>%
    str_replace(pattern = "\\d+", replacement = "") %>% 
    str_replace(pattern = "반마리", replacement = "") %>% 
    str_replace(pattern = ".\\)$", replacement = "") %>% 
    str_replace(pattern = "모듬$", replacement = "") %>% 
    str_replace(pattern = "새$", replacement = "") %>% 
    str_replace(pattern = "맛$", replacement = "") %>% 
    factor(levels = levels(menuList))
}

menuFactor <- sapply(Menu$식사내용, preprocessing)

entry <- matrix(0, 24860, 1697)
for (i in 1:24860) {
  entry[i, menuFactor[[i]]] <- 1
};rm(i, menuFactor)

Menu_one_hot <- data.frame(Menu[, 1:2], entry);rm(entry)
colnames(Menu_one_hot) <- c("Date", "Meal", paste("Menu", 1:1697, sep = "_"))
Menu_one_hot$Meal <- as.factor(Menu_one_hot$Meal)
Menu_one_hot %>% group_by(Date, Meal) %>% summarise_all(sum) -> Menu_one_hot_collapse
Menu_one_hot_collapse[, -1:-2] <- ifelse(Menu_one_hot_collapse[, -1:-2] >= 1, 1, 0)

### Meals
colnames(Meals) <- c("Date", "Meal", "Count")
Meals$Meal <- str_replace(Meals$Meal, pattern = "1", replacement = "")
Meals$Meal <- str_replace(Meals$Meal, pattern = "\\(.*", replacement = "")
Meals$Meal <- factor(Meals$Meal, levels = levels(Menu_one_hot_collapse$Meal))

Meals %>% group_by(Date, Meal) %>% summarise_all(sum) -> Meals_collapse

### Merge : Meals & Menu
x <- merge(x = Meals_collapse, y = Menu_one_hot_collapse, by = c("Date", "Meal"))
rm(Meals_collapse, Menu_one_hot, Menu_one_hot_collapse)
x.dummy <- model.matrix(~Meal, data = x[, 1:2]);rm(Meals, Menu)

x.onehot <- cbind(x.dummy[, -1], x[, -2]);rm(x.dummy)
colnames(x.onehot)[1:3] <- c("Meal_dinner", "Meal_lunch", "Meal_lunch2")

##### additaional data ---------------------------------------------------------------------------
### Add Wheter Data
whether <- readxl::read_xlsx(path = "path/to/provided/meteorological/data.xlsx")[-1:-59, -1]
# colnames(whether)

Whether <- whether[, c(1, 2, 5, 12, 33)];rm(whether)
colnames(Whether) <- c("Date", "Temp.avg", "Temp.high", "Precipitation", "Daylight")
Whether[, 2] <- approx(Whether[, 2], n = nrow(Whether))$y
Whether[, 5] <- approx(Whether[, 5], n = nrow(Whether))$y
Whether[is.na(Whether)[, 4], 4] <- 0
Whether[, 1] <- Whether$Date %>% as.character() %>% str_remove_all("-")
Whether[, -1] <- sapply(Whether[, -1], rescale)

x.onehot <- merge(x.onehot, Whether, "Date");rm(Whether)

### Add week, month, day
week <- (x.onehot$Date %>%
           as.Date(format = '%Y%m%d') %>%
           weekdays %>%
           factor(levels = c("월요일", "화요일", "수요일", "목요일", "금요일", "토요일", "일요일")) %>%
           to_categorical)[, -1]
colnames(week) <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")

month <- (x.onehot$Date %>% as.Date(format = '%Y%m%d') %>% format("%m") %>% to_categorical)[, -1]
colnames(month) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

day <- (x.onehot$Date %>% as.Date(format = '%Y%m%d') %>% format("%d") %>% to_categorical)[, -1]
colnames(day) <- paste("day", 1:31, sep = "_")

x.onehot <- cbind(x.onehot, week, month, day);rm(week, month, day)

### 토, 일 : x.onehot$Sat, x.onehot$Sun

### 신정, 삼일절, POSCO, 근로자의날, 어린이날, 현충일, 광복절, 개천절, 성탄절 : 양력 공휴일
holiday <- c("0101", "0301", "0401", "0501", "0505", "0606", "0815", "1003", "1225")
holiday <- (x.onehot$Date %>% str_sub(start = 5, end = 8) %in% holiday) * 1

### 석가탄신일, 제헌절, 추석, 설날, 한글날, 선거, 대체공휴일
holiday2 <- c("20030508", "20030717", "20030910", "20030911", "20030912",
              "20040121", "20040122", "20040123", "20040526", "20040927", "20040928", "20040929",
              "20050208", "20050209", "20050210", "20050919",
              "20060130", "20060717", "20060815",
              "20070219", "20070524", "20070717",
              "20080206", "20080207", "20080208", "20080512", "20080915",
              "20090126", "20090127", "20091002",
              "20100215", "20100521", "20100602", "20100921", "20100922", "20100923",
              "20110202", "20110203", "20110204", "20110510", "20110912", "20110913",
              "20120123", "20120124", "20120411", "20120528", "20121001", "20121219",
              "20130211", "20130517", "20130918", "20130919", "20130920", "20131009",
              "20140130", "20140131", "20140506", "20140604", "20140908", "20140909", "20140910", "20141009",
              "20150218", "20150219", "20150220", "20150525", "20150814", "20150928", "20150929", "20151009",
              "20160208", "20160209", "20160210", "20160413", "20160506", "20160914", "20160915", "20160916",
              "20170127", "20170130", "20170503", "20170509", "20171002", "20171004", "20171005", "20171006", "20171009")
holiday2 <- (x.onehot$Date %in% holiday2) * 1

### 징검다리(1일) : 0.3
holiday3 <- c("20030509", "20030718", "20031226",
              "20040101",
              "20050207", "20050211", "20050228", "20050404", "20050506",
              "20060605", "20060814", "20061002", "20061004",
              "20070302", "20070525", "20070716", "20071224", "20071231",
              "20081226",
              "20090102",
              "20100920", "20100924",
              "20110228", "20110506", "20110509",
              "20120302", "20121002", "20121224", "20121231",
              "20130607", "20130816", "20131004",
              "20140605", "20141010", "20141226",
              "20150102",
              "20160229",
              "20170504", "20170508", "20170605", "20170814")
holiday3 <- (x.onehot$Date %in% holiday3) * 0.3

### 3일명절,황금연휴 앞뒤 : 0.1
holiday4 <- c("20050916", "20050920",
              "20060127", "20060131",
              "20070216", "20070220",
              "20080912", "20080916",
              "20090123", "20090128", "20091001", "20091005",
              "20100212", "20100216",
              "20120928",
              "20130208", "20130212")
holiday4 <- (x.onehot$Date %in% holiday4) * 0.1

holidays <- apply(cbind(x.onehot$Sat, x.onehot$Sun,
                        holiday, holiday2, holiday3, holiday4),
                  1, max);rm(holiday, holiday2, holiday3, holiday4)

x.onehot <- cbind(x.onehot, holidays);rm(holidays)

##### Train - Vaild - Test Split -----------------------------------------------------------------
x.train <- x.onehot[!x.onehot$Count %>% is.na, -c(1,5)] %>% as.matrix
y.train <- x.onehot[!x.onehot$Count %>% is.na, 5] %>% as.matrix / 200
x.test <- x.onehot[x.onehot$Count %>% is.na, -c(1,5)] %>% as.matrix
y.test <- x.onehot[x.onehot$Count %>% is.na, 5] %>% as.matrix / 200
test.day <- x[x.onehot$Count %>% is.na, 1:2] %>% as.matrix;rm(x)

set.seed(777)
idx <- sample(1:20010, 20010 * 0.2)
x.valid <- x.train[idx, ]
y.valid <- y.train[idx, ] %>% as.matrix
x.train <- x.train[-idx, ]
y.train <- y.train[-idx, ] %>% as.matrix;rm(idx)
rm(x.onehot, menuList, preprocessing)
##### MLP ----------------------------------------------------------------------------------------
set.seed(777)
inputs <- layer_input(shape = ncol(x.train), name = "Input_Layer")
outputs <- inputs %>%
  layer_dropout(rate = 0.3, name = "Input_to_Hidden_Dropout") %>%
  layer_dense(units = 1000,
              kernel_initializer = "glorot_uniform", name = "Fully-connected_1") %>%
  layer_batch_normalization(name = "Batch_Normalization_1") %>%
  layer_activation_leaky_relu(name = "LeakyReLU_Activattion_1") %>%
  layer_dropout(rate = 0.3, name = "Hidden_to_Hidden_Dropout_1") %>%
  layer_dense(units = 700, name = "Fully-connected_2") %>%
  layer_batch_normalization(name = "Batch_Normalization_2") %>%
  layer_activation_leaky_relu(name = "LeakyReLU_Activation_2") %>%
  layer_dropout(rate = 0.3, name = "Hidden_to_Hidden_Dropout_2") %>%
  layer_dense(units = 450, name = "Fully-connected_3") %>%
  layer_batch_normalization(name = "Batch_Normalization_3") %>%
  layer_activation_leaky_relu(name = "LeakyReLU_Activation_3") %>%
  layer_dropout(rate = 0.3, name = "Hidden_to_Hidden_Dropout_3") %>%
  layer_dense(units = 200, name = "Fully-connected_4") %>%
  layer_batch_normalization(name = "Batch_Normalization_4") %>%
  layer_activation_leaky_relu(name = "LeakyReLU_Activation_4") %>%
  layer_dropout(rate = 0.3, name = "Hidden_to_Hidden_Dropout_4") %>%
  layer_dense(units = 50, name = "Fully-connected_5") %>%
  layer_batch_normalization(name = "Batch_Normalization_5") %>%
  layer_activation_leaky_relu(name = "LeakyReLU_Activation_5") %>%
  layer_dropout(rate = 0.3, name = "Hidden_to_Hidden_Dropout_5") %>%
  layer_dense(units = 1, activation = "linear", name = "Linear_Output_Layer")
model <- keras_model(inputs, outputs)
summary(model)

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
  x = x.train,
  y = y.train,
  batch_size = 128,
  epochs = 2000,
  validation_split = 0.2,
  callbacks = callback_early_stopping(monitor = "val_loss",
                                      min_delta = 0,
                                      patience = 100,
                                      verbose = 1),
  view_metrics = F
)
losserror <- matrix(unlist(history$metrics), length(history$metrics[[1]]), 4)
colnames(losserror) <- names(history$metrics)
write.csv(losserror, "losserror_earlystopping.csv", row.names = F)

x11()
plot(history)
savePlot("restaurant_earlystopping.png", type = "png");graphics.off()
pred_early <- round(predict(model, x = x.valid) * 200, 1)
rmse_early <- sqrt(mean((y.valid * 200 - pred_early) ^ 2))
test_early <- data.frame(test.day, "Count" = round(predict(model, x = x.test) * 200, 1))
save.image("restaurant_earlystopping.RData")

##### to 500, 1000, 1500, 2000 epoch -------------------------------------------------------------
epoch_early <- length(history$metrics[[1]])

history <- model %>% fit(
  x = x.train,
  y = y.train,
  batch_size = 128,
  epochs = 500 - epoch_early,
  validation_split = 0.2,
  view_metrics = F
)
losserror <- rbind(losserror,
                   matrix(unlist(history$metrics), 500 - epoch_early, 4))
write.csv(losserror, "losserror.csv", row.names = F)

x11()
plot(history)
savePlot("restaurant_500.png", type = "png");graphics.off()
pred_500 <- round(predict(model, x = x.valid) * 200, 1)
rmse_500 <- sqrt(mean((y.valid * 200 - pred_500) ^ 2))
test_500 <- data.frame(test.day, "Count" = round(predict(model, x = x.test) * 200, 1))
write.csv(c(rmse_early, rmse_500), "rmse.csv")
save.image("restaurant_500.RData")


history <- model %>% fit(
  x = x.train,
  y = y.train,
  batch_size = 128,
  epochs = 500,
  validation_split = 0.2,
  view_metrics = F
)
losserror <- rbind(losserror, matrix(unlist(history$metrics), 500, 4))
write.csv(losserror, "losserror.csv", row.names = F)

x11()
plot(history)
savePlot("restaurant_1000.png", type = "png");graphics.off()
pred_1000 <- round(predict(model, x = x.valid) * 200, 1)
rmse_1000 <- sqrt(mean((y.valid * 200 - pred_1000) ^ 2))
test_1000 <- data.frame(test.day, "Count" = round(predict(model, x = x.test) * 200, 1))
write.csv(c(rmse_early, rmse_500, rmse_1000), "rmse.csv")
save.image("restaurant_1000.RData")

history <- model %>% fit(
  x = x.train,
  y = y.train,
  batch_size = 128,
  epochs = 500,
  validation_split = 0.2,
  view_metrics = F
)
losserror <- rbind(losserror, matrix(unlist(history$metrics), 500, 4))
write.csv(losserror, "losserror.csv", row.names = F)

x11()
plot(history)
savePlot("restaurant_1500.png", type = "png");graphics.off()
pred_1500 <- round(predict(model, x = x.valid) * 200, 1)
rmse_1500 <- sqrt(mean((y.valid * 200 - pred_1500) ^ 2))
test_1500 <- data.frame(test.day, "Count" = round(predict(model, x = x.test) * 200, 1))
write.csv(c(rmse_early, rmse_500, rmse_1000, rmse_1500), "rmse.csv")
save.image("restaurant_1500.RData")

history <- model %>% fit(
  x = x.train,
  y = y.train,
  batch_size = 128,
  epochs = 500,
  validation_split = 0.2,
  view_metrics = F
)
losserror <- rbind(losserror, matrix(unlist(history$metrics), 500, 4))
write.csv(losserror, "losserror.csv", row.names = F)

x11()
plot(history)
savePlot("restaurant_2000.png", type = "png");graphics.off()
pred_2000 <- round(predict(model, x = x.valid) * 200, 1)
rmse_2000 <- sqrt(mean((y.valid * 200 - pred_2000) ^ 2))
test_2000 <- data.frame(test.day, "Count" = round(predict(model, x = x.test) * 200, 1))
write.csv(c(rmse_early, rmse_500, rmse_1000, rmse_1500, rmse_2000), "rmse.csv")
save.image("restaurant_2000.RData")

# Final Model ------------------------------------------------------------------------------------
set.seed(777)
inputs <- layer_input(shape = ncol(x.train), name = "Input_Layer")
outputs <- inputs %>%
  layer_dropout(rate = 0.3, name = "Input_to_Hidden_Dropout") %>%
  layer_dense(units = 1000,
              kernel_initializer = "glorot_uniform", name = "Fully-connected_1") %>%
  layer_batch_normalization(name = "Batch_Normalization_1") %>%
  layer_activation_leaky_relu(name = "LeakyReLU_Activattion_1") %>%
  layer_dropout(rate = 0.3, name = "Hidden_to_Hidden_Dropout_1") %>%
  layer_dense(units = 700, name = "Fully-connected_2") %>%
  layer_batch_normalization(name = "Batch_Normalization_2") %>%
  layer_activation_leaky_relu(name = "LeakyReLU_Activation_2") %>%
  layer_dropout(rate = 0.3, name = "Hidden_to_Hidden_Dropout_2") %>%
  layer_dense(units = 450, name = "Fully-connected_3") %>%
  layer_batch_normalization(name = "Batch_Normalization_3") %>%
  layer_activation_leaky_relu(name = "LeakyReLU_Activation_3") %>%
  layer_dropout(rate = 0.3, name = "Hidden_to_Hidden_Dropout_3") %>%
  layer_dense(units = 200, name = "Fully-connected_4") %>%
  layer_batch_normalization(name = "Batch_Normalization_4") %>%
  layer_activation_leaky_relu(name = "LeakyReLU_Activation_4") %>%
  layer_dropout(rate = 0.3, name = "Hidden_to_Hidden_Dropout_4") %>%
  layer_dense(units = 50, name = "Fully-connected_5") %>%
  layer_batch_normalization(name = "Batch_Normalization_5") %>%
  layer_activation_leaky_relu(name = "LeakyReLU_Activation_5") %>%
  layer_dropout(rate = 0.3, name = "Hidden_to_Hidden_Dropout_5") %>%
  layer_dense(units = 1, activation = "linear", name = "Linear_Output_Layer")
model <- keras_model(inputs, outputs)

model %>% compile(
  optimizer = optimizer_adam(lr = 0.0001),
  loss = loss_mean_elastic_error,
  metrics = metric_mean_squared_error
)

history <- model %>% fit(
  x = rbind(x.train, x.valid),
  y = rbind(y.train, y.valid),
  batch_size = 128,
  epochs = c(epoch_early, 500, 1000, 1500, 2000)[which.min(c(rmse_early, rmse_500, rmse_1000,
                                                             rmse_1500, rmse_2000))],
  validation_split = 0,
  view_metrics = F
)

##### Final Sheet --------------------------------------------------------------------------------
pred.final <- data.frame(test.day, "Count" = round(predict(model, x = x.test) * 200, 1))
pred.table <- (spread(pred.final, Meal, Count) %>%
                 column_to_rownames(var = 'Date'))[seq(3, 150, 3), c(1, 3, 4, 2)]
write.csv(pred.table, "pred_table.csv", row.names = T)
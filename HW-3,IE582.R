stringsAsFactors=FALSE
# install.packages("tidyverse", type = "binary")
library(tidyverse)
library(data.table)
library(glmnet)
consumption<-read.csv("/Users/ayberkakgun/Desktop/consumption.csv",header=F) %>% as.data.table()

consumption_WIP<-consumption[-(1)]
colnames(consumption_WIP)<-c("Date", "Time", "Consumption")
datetime<-as.POSIXct(paste(consumption_WIP$Date, consumption_WIP$Time), format="%d.%m.%Y %H:%M")

consump<-consumption_WIP$Consumption %>% as.character
consump<-gsub(".", "",consump,fixed = TRUE)
consump<-gsub(",", ".",consump,fixed = TRUE) %>%
as.numeric

# consumption_clean<-data.table(Date=date, Time=time, Consumption=consump)
consumption_clean<-data.table(Datetime=datetime, Consumption=consump)
str(consumption_clean)

# A)
consumption_test<- consumption_clean %>%
  filter(Datetime>="2019-11-01")

consumption_test$naive_forecast_48<-consumption_clean %>%
  filter(Datetime %in% unique(consumption_test$Datetime-3600*48)) %>%
  select(Consumption)

consumption_test$naive_forecast_168<-consumption_clean %>%
  filter(Datetime %in% unique(consumption_test$Datetime-3600*168)) %>%
  select(Consumption)

consumption_test$MAPE_48<-abs(consumption_test$naive_forecast_48-consumption_test$Consumption)/consumption_test$Consumption
consumption_test$MAPE_168<-abs(consumption_test$naive_forecast_168-consumption_test$Consumption)/consumption_test$Consumption

colnames(consumption_test)<-c("Datetime", "Consumption", "Naive Forecast 48","Naive Forecast 168","MAPE 48","MAPE 168")

# B)

forecast_48<-consumption_clean %>%
  filter(Datetime<"2019-11-01") %>%
  mutate(Datetime=Datetime+(3600*48))
colnames(forecast_48)<-c("Datetime","48")
forecast_168<-consumption_clean %>%
  filter(Datetime<"2019-11-01") %>%
  mutate(Datetime=Datetime+(3600*168))
colnames(forecast_168)<-c("Datetime","168")

consumption_lr<-consumption_clean%>% 
  left_join(forecast_48,by="Datetime") %>%
  left_join(forecast_168,by="Datetime")

consumption_train_lr <- consumption_lr%>%
  filter(Datetime<"2019-11-01") %>%
  select('48','168',Consumption)

consumption_test_lr <- consumption_lr%>%
  filter(Datetime>="2019-11-01")

lr<-lm(Consumption ~., data=consumption_train_lr)
# hist(lr$residuals)
# plot(lr$residuals)

prediction_lr<-consumption_test_lr %>%
  select('48','168')

consumption_test_lr$linear_forecast<-predict(lr,prediction_lr)
consumption_test_lr$MAPE<-abs(consumption_test_lr$linear_forecast-consumption_test_lr$Consumption)/consumption_test_lr$Consumption

# C)
date<-consumption_WIP$Date %>% as.character %>% as.Date(format="%d.%m.%Y")
time<-consumption_WIP$Time %>% as.numeric()-1
consumption_clean<-data.table(Date=date, Time=time, Consumption=consump)
forecast_48<-consumption_clean %>%
  filter(Date<"2019-11-01") %>%
  mutate(Date=Date+2)
forecast_168<-consumption_clean %>%
  filter(Date<"2019-11-01") %>%
  mutate(Date=Date+7)
consumption_lrh<-consumption_clean%>% 
  left_join(forecast_48,by=c("Date","Time")) %>%
  left_join(forecast_168,by=c("Date","Time"))
colnames(consumption_lrh)<-c("Date", "Time", "Consumption","Forecast_48","Forecast_168")

consumption_test_lrh <- consumption_lrh%>%
  filter(Date>="2019-11-01")

consumption_train_lrh <- consumption_lrh %>%
  filter(Date<"2019-11-01")

lr_set<-vector("list",24)
linear_forecast_set<-vector("list",24)
for(i in 1:24){
  consumption_trainset_h<-consumption_train_lrh %>%
    filter(Time==i-1) %>%
    select('Forecast_48','Forecast_168',Consumption) 
  lr_set[[i]]<-lm(Consumption ~., data=consumption_trainset_h)
  prediction_lr_h<-consumption_test_lrh %>%
    filter(Time==i-1) %>%
    select('Forecast_48','Forecast_168')
  linear_forecast_set[[i]]<-predict(lr_set[[i]],prediction_lr_h)
}
consumption_test_lrh$forecast_hourly<-unlist(linear_forecast_set)
consumption_test_lrh$MAPE<-abs(consumption_test_lrh$forecast_hourly-consumption_test_lrh$Consumption)/consumption_test_lrh$Consumption

#D)

forecast_wide_7<-spread(consumption_clean,Time,Consumption) %>%
  # consumption_clean_wide%>%
  mutate(Date=Date+7)
  forecast_wide_2<-spread(consumption_clean,Time,Consumption) %>%
  mutate(Date=Date+2) %>%
  left_join(forecast_wide_7,by="Date")

  test_lasso <- consumption_clean %>%
    filter(Date>="2019-11-01")  
  

train<-vector("list")
set.seed(1)

  for(i in 1:24){
  train[[i]]<-consumption_clean %>%
  filter(Time==i-1) %>%
  filter(Date<"2019-11-01") %>%
  left_join(forecast_wide_2,by="Date") %>%
    na.omit()
  
  test<-consumption_clean %>%
    filter(Time==i-1) %>%
    filter(Date>="2019-11-01") %>%
    left_join(forecast_wide_2,by="Date") %>%
    na.omit()
  test<-test[4:51] %>% as.matrix
  consumption_lasso<-train[[i]][3]
  forecast_data<-as.matrix (train[[i]][c(4:51)])
  glmm<-cv.glmnet(forecast_data,consumption_lasso$Consumption,nfolds=10)
  fit<-glmnet(forecast_data,consumption_lasso$Consumption,lambda = glmm$lambda.min)
  results[[i]]<-predict(fit,test)
  }

test_lasso$prediction<-results %>% unlist()
test_lasso$MAPE<-abs(test_lasso$prediction-test_lasso$Consumption)/test_lasso$Consumption
hist(test_lasso$MAPE)

# f)

errors<-data.frame(
  MAPE48=consumption_test$`MAPE 48`,
        MAPE168=consumption_test$`MAPE 168`,
    MAPE_lr=consumption_test_lr$MAPE,
    MAPE_lrh=consumption_test_lrh$MAPE,
    MAPE_lasso=test_lasso$MAPE)

colnames(errors)<-c("MAPE 48 h", "MAPE 168 h", "MAPE lr","MAPE lr H","MAPE Lasso")

boxplot(errors)











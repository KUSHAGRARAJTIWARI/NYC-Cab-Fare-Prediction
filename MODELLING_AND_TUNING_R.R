#clean R enviornment
rm(list=ls())
#load libraries
library(dplyr)
require(MASS)
require(dplyr)
library(MLmetrics)
library(rpart)
library(MASS)
library(randomForest)
library(xgboost)
library(readr)
library(stringr)
library(caret)
library(car)
library(ade4)
library(data.table)
library(geosphere)
library(magrittr)
library(tidyverse)
library(geosphere)
library(magrittr)
library(tidyverse)
#set working directory
setwd("D:/DATA SCIENCE/COURSE/Projects/Project_Cab_Fare_Prediction")
#load train and test data which is cleaned in exploration part
train=read.csv("cleaned_train.csv",header=T)
test=read.csv("cleaned_test.csv",header=T)
dim(train)
dim(test)
#select data with original variables (fare_amount, locations, extracted variables from pickup_datetime)
df_train=dplyr::select(train,-c(distance_km, is_pickup_near_jfk,
                   is_dropoff_near_jfk, is_pickup_near_lgr, is_dropoff_near_lgr,
                    is_pickup_near_newark, is_dropoff_near_newark, pickup_borough,
                    dropoff_borough,pickup_datetime,pickup_date))
dim(df_train)
#divide inti train and validation set
train_index = sample(1:nrow(df_train), 0.8 * nrow(df_train))
test_index = setdiff(1:nrow(df_train), train_index)
x_train = df_train[train_index,]
X_train=dplyr::select(x_train,-c(fare_amount))
y_train = df_train[train_index, "fare_amount"]

x_test = df_train[test_index,]
X_test=dplyr::select(x_test,-c(fare_amount))
y_test = df_train[test_index, "fare_amount"]
dim(x_test)
dim(X_test)

#baseline predictiobn model 
avg_fare=mean(y_train)
avg_fare#11.214
baseline_rmse=RMSE(avg_fare,y_test)

baseline_rmse #rmse=9.531
# building linear regession model

model=lm(fare_amount~.,x_train)
p=predict(model,X_test)
lm_rmse=RMSE(p,y_test)
lm_rmse#7.933

#random forest model
rf_model=randomForest(fare_amount~.,x_train,importance=TRUE,ntree=100)
pr=predict(rf_model,X_test)
rf_rmse=RMSE(pr,y_test)
rf_rmse#4.335

#lightgbm
lgb.grid = list(objective = 'regression',
                metric = "l2_root",
                num_leaves=10,
                num_trees=100,
                is_unbalance = TRUE)

lgb.model.cv=lgb.cv(params = lgb.grid,data =data.matrix(X_train) ,label = y_train, 
                    nrounds =100, early_stopping_rounds = 15,
                            eval = 5,
                           nfold = 15, stratified = TRUE)
best.iter = lgb.model.cv$best_iter       
best.iter
dtrain=lgb.Dataset(data.matrix(X_train),label=y_train)
lgb_model=lgb.train(params = lgb.grid,data =dtrain, 
                    nrounds =best.iter)

prr=predict(lgb_model,data.matrix(X_test))
lgb_rmse=RMSE(prr,y_test)
lgb_rmse

#
model_pred=data.frame(matrix(ncol=4,nrow=3))
colnames(model_pred)=c('model name','train rmse','test rmse','variance')
model_pred$`model name`=c('linear regression','random forest','lightgbm')
train_lm_rmse=RMSE(predict(model,X_train),y_train)
train_rf_rmse=RMSE(predict(rf_model,data.matrix(X_train)),y_train)
train_lgb_rmse=RMSE(predict(lgb_model,data.matrix(X_train)),y_train)

model_pred$`test rmse`=c(lm_rmse,rf_rmse,lgb_rmse)
model_pred$`train rmse`=c(train_lm_rmse,train_rf_rmse,train_lgb_rmse)
model_pred$variance=model_pred$`train rmse`- model_pred$`test rmse`
model_pred
#we are choosing lighgbm as my model for further use.
#feature engineering

View(model_pred)

ohe_feats=c('pickup_borough','dropoff_borough')
for(f in ohe_feats)
{
  train_dummy=acm.disjonctif(train[f])
  train[f]=NULL
  train=cbind(train,train_dummy)
}
for(f in ohe_feats)
{
  test_dummy=acm.disjonctif(test[f])
  test[f]=NULL
  test=cbind(test,test_dummy)
}
train=dplyr::select(train,-c(pickup_datetime,pickup_date))
test=dplyr::select(test,-c(pickup_datetime,pickup_date))
#location coordinates
lgr=c(-73.8733, 40.7746)
jfk=c(-73.7900, 40.6437)
ewr=c(-74.1843, 40.6924)
manhattan=c(-73.9664, 40.7909)
queens=c(-73.8317, 40.7038)
brooklyn=c(-73.9489, 40.6551)
bronx=c(-73.8568, 40.8572)
staten_island=c(-74.1540, 40.5725)

#pickup distances from airport in train  
train = train %>% 
  mutate(pickup_distance_jfk = by(train, 1:nrow(train), function(row) { distHaversine(c(row$pickup_longitude, row$pickup_latitude), jfk,r=6371.137)  }))
train = train %>% 
  mutate(pickup_distance_lgr = by(train, 1:nrow(train), function(row) { distHaversine(c(row$pickup_longitude, row$pickup_latitude), lgr,r=6371.137)  }))
train = train %>% 
  mutate(pickup_distance_ewr = by(train, 1:nrow(train), function(row) { distHaversine(c(row$pickup_longitude, row$pickup_latitude), ewr,r=6371.137)  }))
#dropoff distance from airport in train 
train = train %>% 
  mutate(dropoff_distance_jfk = by(train, 1:nrow(train), function(row) { distHaversine(c(row$dropoff_longitude, row$dropoff_latitude), jfk,r=6371.137)  }))
train = train %>% 
  mutate(dropoff_distance_lgr = by(train, 1:nrow(train), function(row) { distHaversine(c(row$dropoff_longitude, row$dropoff_latitude), lgr,r=6371.137)  }))
train = train %>% 
  mutate(dropoff_distance_ewr = by(train, 1:nrow(train), function(row) { distHaversine(c(row$dropoff_longitude, row$dropoff_latitude), ewr,r=6371.137)  }))

#pickup distances from airport in test  
test = test %>% 
  mutate(pickup_distance_jfk = by(test, 1:nrow(test), function(row) { distHaversine(c(row$pickup_longitude, row$pickup_latitude), jfk,r=6371.137)  }))
test = test %>% 
  mutate(pickup_distance_lgr = by(test, 1:nrow(test), function(row) { distHaversine(c(row$pickup_longitude, row$pickup_latitude), lgr,r=6371.137)  }))
test = test %>% 
  mutate(pickup_distance_ewr = by(test, 1:nrow(test), function(row) { distHaversine(c(row$pickup_longitude, row$pickup_latitude), ewr,r=6371.137)  }))
#dropoff distance from airport in test 
test = test %>% 
  mutate(dropoff_distance_jfk = by(test, 1:nrow(test), function(row) { distHaversine(c(row$dropoff_longitude, row$dropoff_latitude), jfk,r=6371.137)  }))
test = test %>% 
  mutate(dropoff_distance_lgr = by(test, 1:nrow(test), function(row) { distHaversine(c(row$dropoff_longitude, row$dropoff_latitude), lgr,r=6371.137)  }))
test = test %>% 
  mutate(dropoff_distance_ewr = by(test, 1:nrow(test), function(row) { distHaversine(c(row$dropoff_longitude, row$dropoff_latitude), ewr,r=6371.137)  }))

#pickup distanse from boroughs
train = train %>% 
  mutate(pickup_distance_manhattan = by(train, 1:nrow(train), function(row) { distHaversine(c(row$pickup_longitude, row$pickup_latitude),manhattan,r=6371.137)  }))
train = train %>% 
  mutate(pickup_distance_queens = by(train, 1:nrow(train), function(row) { distHaversine(c(row$pickup_longitude, row$pickup_latitude),queens,r=6371.137)  }))
train = train %>% 
  mutate(pickup_distance_brooklyn = by(train, 1:nrow(train), function(row) { distHaversine(c(row$pickup_longitude, row$pickup_latitude),brooklyn,r=6371.137)  }))
train = train %>% 
  mutate(pickup_distance_bronx = by(train, 1:nrow(train), function(row) { distHaversine(c(row$pickup_longitude, row$pickup_latitude),bronx,r=6371.137)  }))
train = train %>% 
  mutate(pickup_distance_staten_island = by(train, 1:nrow(train), function(row) { distHaversine(c(row$pickup_longitude, row$pickup_latitude),staten_island,r=6371.137)  }))
#dropoff distance from airport in train 
train = train %>% 
  mutate(dropoff_distance_manhattan = by(train, 1:nrow(train), function(row) { distHaversine(c(row$dropoff_longitude, row$dropoff_latitude),manhattan,r=6371.137)  }))
train = train %>% 
  mutate(dropoff_distance_queens = by(train, 1:nrow(train), function(row) { distHaversine(c(row$dropoff_longitude, row$dropoff_latitude),queens,r=6371.137)  }))
train = train %>% 
  mutate(dropoff_distance_brooklyn = by(train, 1:nrow(train), function(row) { distHaversine(c(row$dropoff_longitude, row$dropoff_latitude),brooklyn,r=6371.137)  }))
train = train %>% 
  mutate(dropoff_distance_bronx = by(train, 1:nrow(train), function(row) { distHaversine(c(row$dropoff_longitude, row$dropoff_latitude),bronx,r=6371.137)  }))
train = train %>% 
  mutate(dropoff_distance_staten_island  = by(train, 1:nrow(train), function(row) { distHaversine(c(row$dropoff_longitude, row$dropoff_latitude),staten_island,r=6371.137)  }))

#pickup distanse from boroughs
train = train %>% 
  mutate(pickup_distance_manhattan = by(train, 1:nrow(train), function(row) { distHaversine(c(row$pickup_longitude, row$pickup_latitude),manhattan,r=6371.137)  }))
train = train %>% 
  mutate(pickup_distance_queens = by(train, 1:nrow(train), function(row) { distHaversine(c(row$pickup_longitude, row$pickup_latitude),queens,r=6371.137)  }))
train = train %>% 
  mutate(pickup_distance_brooklyn = by(train, 1:nrow(train), function(row) { distHaversine(c(row$pickup_longitude, row$pickup_latitude),brooklyn,r=6371.137)  }))
train = train %>% 
  mutate(pickup_distance_bronx = by(train, 1:nrow(train), function(row) { distHaversine(c(row$pickup_longitude, row$pickup_latitude),bronx,r=6371.137)  }))
train = train %>% 
  mutate(pickup_distance_staten_island = by(train, 1:nrow(train), function(row) { distHaversine(c(row$pickup_longitude, row$pickup_latitude),staten_island,r=6371.137)  }))
#dropoff distance from airport in train 
train = train %>% 
  mutate(dropoff_distance_manhattan = by(train, 1:nrow(train), function(row) { distHaversine(c(row$dropoff_longitude, row$dropoff_latitude),manhattan,r=6371.137)  }))
train = train %>% 
  mutate(dropoff_distance_queens = by(train, 1:nrow(train), function(row) { distHaversine(c(row$dropoff_longitude, row$dropoff_latitude),queens,r=6371.137)  }))
train = train %>% 
  mutate(dropoff_distance_brooklyn = by(train, 1:nrow(train), function(row) { distHaversine(c(row$dropoff_longitude, row$dropoff_latitude),brooklyn,r=6371.137)  }))
train = train %>% 
  mutate(dropoff_distance_bronx = by(train, 1:nrow(train), function(row) { distHaversine(c(row$dropoff_longitude, row$dropoff_latitude),bronx,r=6371.137)  }))
train = train %>% 
  mutate(dropoff_distance_staten_island  = by(train, 1:nrow(train), function(row) { distHaversine(c(row$dropoff_longitude, row$dropoff_latitude),staten_island,r=6371.137)  }))


#pickup distanse from boroughs in train
train = train %>% 
  mutate(pickup_distance_manhattan = by(train, 1:nrow(train), function(row) { distHaversine(c(row$pickup_longitude, row$pickup_latitude),manhattan,r=6371.137)  }))
train = train %>% 
  mutate(pickup_distance_queens = by(train, 1:nrow(train), function(row) { distHaversine(c(row$pickup_longitude, row$pickup_latitude),queens,r=6371.137)  }))
train = train %>% 
  mutate(pickup_distance_brooklyn = by(train, 1:nrow(train), function(row) { distHaversine(c(row$pickup_longitude, row$pickup_latitude),brooklyn,r=6371.137)  }))
train = train %>% 
  mutate(pickup_distance_bronx = by(train, 1:nrow(train), function(row) { distHaversine(c(row$pickup_longitude, row$pickup_latitude),bronx,r=6371.137)  }))
train = train %>% 
  mutate(pickup_distance_staten_island = by(train, 1:nrow(train), function(row) { distHaversine(c(row$pickup_longitude, row$pickup_latitude),staten_island,r=6371.137)  }))
#dropoff distance from airport in train 
train = train %>% 
  mutate(dropoff_distance_manhattan = by(train, 1:nrow(train), function(row) { distHaversine(c(row$dropoff_longitude, row$dropoff_latitude),manhattan,r=6371.137)  }))
train = train %>% 
  mutate(dropoff_distance_queens = by(train, 1:nrow(train), function(row) { distHaversine(c(row$dropoff_longitude, row$dropoff_latitude),queens,r=6371.137)  }))
train = train %>% 
  mutate(dropoff_distance_brooklyn = by(train, 1:nrow(train), function(row) { distHaversine(c(row$dropoff_longitude, row$dropoff_latitude),brooklyn,r=6371.137)  }))
train = train %>% 
  mutate(dropoff_distance_bronx = by(train, 1:nrow(train), function(row) { distHaversine(c(row$dropoff_longitude, row$dropoff_latitude),bronx,r=6371.137)  }))
train = train %>% 
  mutate(dropoff_distance_staten_island  = by(train, 1:nrow(train), function(row) { distHaversine(c(row$dropoff_longitude, row$dropoff_latitude),staten_island,r=6371.137)  }))

#pickup distanse from boroughs in test
test = test %>% 
  mutate(pickup_distance_manhattan = by(test, 1:nrow(test), function(row) { distHaversine(c(row$pickup_longitude, row$pickup_latitude),manhattan,r=6371.137)  }))
test = test %>% 
  mutate(pickup_distance_queens = by(test, 1:nrow(test), function(row) { distHaversine(c(row$pickup_longitude, row$pickup_latitude),queens,r=6371.137)  }))
test = test %>% 
  mutate(pickup_distance_brooklyn = by(test, 1:nrow(test), function(row) { distHaversine(c(row$pickup_longitude, row$pickup_latitude),brooklyn,r=6371.137)  }))
test = test %>% 
  mutate(pickup_distance_bronx = by(test, 1:nrow(test), function(row) { distHaversine(c(row$pickup_longitude, row$pickup_latitude),bronx,r=6371.137)  }))
test = test %>% 
  mutate(pickup_distance_staten_island = by(test, 1:nrow(test), function(row) { distHaversine(c(row$pickup_longitude, row$pickup_latitude),staten_island,r=6371.137)  }))
#dropoff distance from airport in train 
test = test %>% 
  mutate(dropoff_distance_manhattan = by(test, 1:nrow(test), function(row) { distHaversine(c(row$dropoff_longitude, row$dropoff_latitude),manhattan,r=6371.137)  }))
test = test %>% 
  mutate(dropoff_distance_queens = by(test, 1:nrow(test), function(row) { distHaversine(c(row$dropoff_longitude, row$dropoff_latitude),queens,r=6371.137)  }))
test = test %>% 
  mutate(dropoff_distance_brooklyn = by(test, 1:nrow(test), function(row) { distHaversine(c(row$dropoff_longitude, row$dropoff_latitude),brooklyn,r=6371.137)  }))
test = test %>% 
  mutate(dropoff_distance_bronx = by(test, 1:nrow(test), function(row) { distHaversine(c(row$dropoff_longitude, row$dropoff_latitude),bronx,r=6371.137)  }))
test = test %>% 
  mutate(dropoff_distance_staten_island  = by(test, 1:nrow(test), function(row) { distHaversine(c(row$dropoff_longitude, row$dropoff_latitude),staten_island,r=6371.137)  }))

dim(train)
dim(test)
#train test split after adding features
train_index_1 = sample(1:nrow(train), 0.8 * nrow(train))
test_index_1 = setdiff(1:nrow(train), train_index)
x_train_1 = train[train_index_1,]
X_train_1=dplyr::select(x_train_1,-c(fare_amount))
y_train_1 = df_train[train_index_1, "fare_amount"]

x_test_1 = train[test_index_1,]
X_test_1=dplyr::select(x_test_1,-c(fare_amount))
y_test_1 = df_train[test_index_1, "fare_amount"]

dim(X_train_1)
dim(X_test_1)
#lightgbm after feature engineering
f_lgb.model.cv=lgb.cv(params = lgb.grid,data =data.matrix(X_train_1) ,label = y_train_1, 
                    nrounds =100, early_stopping_rounds = 15,
                    eval = 5,
                    nfold = 15, stratified = TRUE)
f_best.iter = f_lgb.model.cv$best_iter       
f_best.iter
f_dtrain=lgb.Dataset(data.matrix(X_train_1),label=y_train_1)
f_lgb_model=lgb.train(params = lgb.grid,data =f_dtrain, 
                    nrounds =f_best.iter)

f_prr=predict(f_lgb_model,data.matrix(X_test_1))
f_lgb_rmse=RMSE(f_prr,y_test_1)
f_lgb_rmse
f_train_lgb_rmse=RMSE(predict(f_lgb_model,data.matrix(X_train_1)),y_train_1)
f_train_lgb_rmse





library(xgboost)
library(readr)
library(caret)
library(mlr)
library(parallel)
library(parallelMap) 
library(car)


tr_pass = Train[, pass] 
Train = Train[, -c("pass")]

v_pass = Valid[, pass]
Valid = Valid[, -c("pass")]

# XGboost matricies
dtrain <- xgb.DMatrix(as.matrix(Train), label = as.matrix(tr_pass))
dtest <- xgb.DMatrix(as.matrix(Valid), label = as.matrix(v_pass))

#Setting preliminary  parameters
params <- list(booster = "gbtree", 
               objective = "binary:logistic", 
               eta=0.3, 
               gamma=3, 
               max_depth=6, 
               min_child_weight=1, 
               subsample=.7, 
               colsample_bytree=.7)

# Cross-fold model
xgbcv <- xgb.cv( params = params, 
                 data = dtrain, 
                 nrounds = 250, 
                 nfold = 3, 
                 showsd = F,
                 metrics = "error",
                 print_every_n = 10, 
                 early_stop_rounds = 20, 
                 maximize = F)
info = xgbcv$evaluation_log #test_rmse_mean

which.min(info$test_error_mean)
info

#Renaming variables for model
num = seq(1,ncol(Train),1)
var = rep('var_',ncol(Train))
new_var = paste(var,num,sep = "")
colnames(Train)[1:ncol(Train)] <- new_var
colnames(Valid)[1:ncol(Valid)] <- new_var

Train = cbind(Train,tr_pass)
Valid = cbind(Valid,v_pass)
Train[, tr_pass := as.factor(tr_pass)]
Valid[, v_pass := as.factor(v_pass)]

rm(list = c('dtest','dtrain','xgbcv','params','info','new_var','num','var'))


Traintask <- makeClassifTask (data = Train,target = "tr_pass")
Validtask <- makeClassifTask (data = Valid,target = "v_pass")

lrn <- makeLearner("classif.xgboost",predict.type = "response")
lrn$par.vals <- list( objective="binary:logistic", eval_metric="error", nrounds=100L, eta=0.1)

params <- makeParamSet( makeDiscreteParam("booster",values = "gbtree"), 
                        makeIntegerParam("max_depth",lower = 5L,upper = 10L), 
                        makeNumericParam("min_child_weight",lower = 3L,upper = 10L), 
                        makeNumericParam("subsample",lower = 0.5,upper = 1), 
                        makeNumericParam("colsample_bytree",lower = 0.5,upper = 1),
                        makeNumericParam("gamma",lower = 2L,upper = 4L))

rdesc <- makeResampleDesc("CV",stratify = T,iters=5L)
ctrl <- makeTuneControlRandom(maxit = 15L)

library(parallel)
library(parallelMap) 
parallelStartSocket(cpus = detectCores())


# Tuning parameters
mytune <- tuneParams(learner = lrn, 
                     task = Traintask, 
                     resampling = rdesc,
                     measures = acc, 
                     par.set = params, 
                     control = ctrl, 
                     show.info = T)

#Result: booster=gbtree; max_depth=8; min_child_weight=7.16; subsample=0.959; 
#colsample_bytree=0.943; gamma=3.9 : acc.test.mean=0.703

rm(list = c('ctrl','lrn','params','rdesc','mytune','Traintask','Validtask'))  


#Setting preliminary  parameters
params <- list(booster = "gbtree", 
               objective = "binary:logistic", 
               eta=0.1, 
               gamma=2.34, 
               max_depth=9, 
               min_child_weight=6.45, 
               subsample=.772, 
               colsample_bytree=.957)

# Cross-fold model
xgbcv <- xgb.cv( params = params, 
                 data = dtrain, 
                 nrounds = 200, 
                 nfold = 3, 
                 showsd = F,
                 metrics = "error",
                 print_every_n = 10, 
                 early_stop_rounds = 20, 
                 maximize = F)
info = xgbcv$evaluation_log #test_rmse_mean
which.min(info$test_error_mean)
1-min(info$test_error_mean)

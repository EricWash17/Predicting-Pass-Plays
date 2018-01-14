library(xgboost)
library(readr)
library(caret)
library(mlr)
library(car)

tr_pass = Train[, pass] 
Train = Train[, -c("pass")]

v_pass = Valid[, pass]
Valid = Valid[, -c("pass")]


# XGboost matricies
dtrain <- xgb.DMatrix(as.matrix(Train), label = as.matrix(tr_pass))
dvalid <- xgb.DMatrix(as.matrix(Valid), label = as.matrix(v_pass))

#Setting parameters from tuned model
params <- list(booster = "gbtree", 
               objective = "binary:logistic", 
               eta=0.1, 
               gamma=3.9, 
               max_depth=8, 
               min_child_weight=7.16, 
               subsample=0.959, 
               colsample_bytree=0.943)
set.seed(501)
# Cross-fold model
xgb.fit <- xgb.train( params = params, 
                      data = dtrain, 
                      nrounds = 150, 
                      watchlist = list(val=dvalid,train=dtrain),
                      eval_metrics = "error",
                      print_every_n = 10, 
                      early_stop_rounds = 20, 
                      maximize = F)

#Result: booster=gbtree; max_depth=8; min_child_weight=7.16; subsample=0.959; 
#colsample_bytree=0.943; gamma=3.9 : acc.test.mean=0.703

xgb.importance(colnames(dtrain), model = xgb.fit)

nt_pass = New.Train[, pass] 
New.Train = New.Train[, -c("pass")]

t_pass = Test[, pass]
Test = Test[, -c("pass")]


dnew_train <- xgb.DMatrix(as.matrix(New.Train), label = as.matrix(nt_pass))
dtest <- xgb.DMatrix(as.matrix(Test), label = as.matrix(t_pass))

set.seed(89)
xgb.fit <- xgb.train( params = params, 
                      data = dnew_train, 
                      nrounds = 150, 
                      watchlist = list(val=dtest,train=dnew_train),
                      eval_metrics = "error",
                      print_every_n = 10, 
                      early_stop_rounds = 20, 
                      maximize = F)

xgb.preds <- predict(xgb.fit,dtest)
xgb.pred=rep(0, nrow(Test)) 
xgb.pred[xgb.preds>.5] = 1 
mean(xgb.pred==t_pass) #.70

final = cbind(plays[year == 2017,c('offenseTeam','year','yardsGained','ptot','rtot')],
              Test, t_pass, xgb.preds)

final = data.table(final)
final[, prediction := ifelse(xgb.preds>=.50,1,0)]
final[, correct := ifelse(t_pass ==  prediction,1,0)]
mean(final$correct)

write.csv(final,"predictions.csv")

exm = final[, mean(correct), by = 'offenseTeam']

rm(list = c('xgb.fit','dtrain','dvalid','params','xgb.pred'))

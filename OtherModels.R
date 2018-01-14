###############################
##                           ##
##     Elastinet Logistic    ##
##                           ##
###############################
library(glmnet)
library(caret)

lambda.grid = 10^seq(3, -3, length.out = 100)
x=model.matrix(pass~.,data = Train)[,-1]
y=Train$pass

alphas <- seq(0.0, 1, by=.02)
mses <- numeric(51)

set.seed(42)

for(i in 1:51){
  cvfits <- cv.glmnet(x, y, alpha=alphas[i], nfolds=5)
  loc <- which(cvfits$lambda==cvfits$lambda.min)
  mses[i] <- cvfits$cvm[loc]
}

this <- data.frame(mse=mses, alpha=alphas)
alp = this[which.min(mses), 2]

glm.fit <- cv.glmnet(x, y=as.factor(y), alpha=1, family="binomial",
                    lambda = lambda.grid,nfolds = 5, type.measure = 'class')

bestlam = glm.fit$lambda.1se
plot(glm.fit)

test <- model.matrix(~.-pass,data=Valid)[,-1]
glm.preds = predict(object = glm.fit,s='lambda.min',newx = as(test,"dgCMatrix"),type = "response")

glm.pred=rep(0, nrow(Valid)) 
glm.pred[glm.preds>.5] = 1 
table(glm.pred)
table(glm.pred, Valid$pass)
mean(glm.pred==Valid$pass) #.647

rm(list = c('i','mses','loc','cvfits','glm.pred','this','alphas','glm.fit',
            'x','y','test','alp','lambda.grid','bestlam'))


tr_pass = Train[, pass] 
Train = Train[, -c("pass")]

v_pass = Valid[, pass]
Valid = Valid[, -c("pass")]


###############################
##                           ##
##        XG Boost           ##
##                           ##
###############################
library(xgboost)
library(readr)
library(caret)
library(mlr)
library(car)

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
                 nrounds = 200, 
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

final = cbind(plays[year == 2017,c('offenseTeam','year')],Test, t_pass, xgb.preds)

final = data.table(final)
final[, prediction := ifelse(xgb.preds>=.5,1,0)]
final[, correct := ifelse(t_pass ==  prediction,1,0)]

write.csv(final,"predictions.csv")

exm = final[, mean(correct), by = 'offenseTeam']

rm(list = c('xgb.fit','dtrain','dvalid','params','xgb.pred'))

###############################
##                           ##
##         Scaling           ##
##                           ##
###############################
# Scale btw 0 and 1 function
standard = function(x){
  (x - min(x)) / (max(x) - min(x))
}


###############################
##                           ##
##       Neural Net          ##
##                           ##
###############################
NTrain = apply(Train, 2, standard)
NValid = apply(Valid, 2, standard)

library(keras)
#install_keras()
set.seed(808)

y_train = to_categorical(tr_pass, 2)
x_train = as.matrix(NTrain)

y_valid = to_categorical(v_pass, 2)
x_valid = as.matrix(NValid)


nn.model <- keras_model_sequential() 
nn.model %>% 
  layer_dense(units = 25, activation = 'relu', input_shape = c(ncol(NTrain))) %>% 
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 10, activation = 'relu') %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 2, activation = 'softmax')

summary(nn.model)  

nn.model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_adam(),  
  metrics = c('accuracy')
)

early_stopping <- callback_early_stopping(monitor = 'accuracy', patience = 2)
history <- nn.model %>% fit(
  x_train, y_train, 
  epochs = 20, batch_size = 32,
  validation_split = 0.2,
  callbacks = c(early_stopping)
)

nn.model %>% evaluate(x_valid, y_valid) # .65
nn.preds = nn.model %>% predict_classes(x_valid)

rm(list = c('x_train','y_train','x_valid','y_valid','early_stopping','history','nn.model'))

###############################
##                           ##
##       Random Forest       ##
##                           ##
###############################

library(parallel)
library(parallelMap) 
parallelStartSocket(cpus = detectCores())

Train = cbind(Train,tr_pass)
Valid = cbind(Valid,v_pass)
Train[, tr_pass := as.factor(tr_pass)]
Valid[, v_pass := as.factor(v_pass)]

library(randomForest)

rf.fit = randomForest(tr_pass~., data = Train, mtry = 6, nodesize = 18)
summary(rf.fit)

rf.preds = predict(rf.fit,newdata = Valid,type = 'response')
mean(rf.preds==Valid$v_pass) # .68

rm(list = c('rf.fit','v_pass','tr_pass'))

###############################
##                           ##
##        Naiave Bayes       ##
##                           ##
###############################

library(e1071)
library("Hmisc")

res2 <- rcorr(as.matrix(Train))
res2


Train[, tr_pass := as.factor(tr_pass)]
Valid[, v_pass := as.factor(v_pass)]

bayes.fit = naiveBayes(tr_pass~.-plays,data = Train)
print(bayes.fit)

bayes.preds = predict(bayes.fit, newdata = Valid)
conf_matrix <- table(bayes.preds, Valid$v_pass)
sum(conf_matrix[1,1],conf_matrix[2,2])/nrow(Valid) # .623

rm(list = c('conf_matrix','bayes.fit'))


###############################
##                           ##
##        Stack              ##
##                           ##
###############################

bayes.preds = data.table(bayes.preds)
nn.preds = data.table(nn.preds)
colnames(nn.preds) = 'nn.preds'
rf.preds = data.table(rf.preds)
colnames(rf.preds)[2] = 'rf.preds'
rf.preds = rf.preds[,rf.preds]
xgb.preds= data.table(xgb.preds)
glm.preds = data.table(glm.preds)
colnames(glm.preds) = 'glm.preds'

DT = cbind(bayes.preds,glm.preds,nn.preds,rf.preds,xgb.preds,Valid$v_pass)
colnames(DT)[ncol(DT)] = 'v_pass'

res2 <- rcorr(as.matrix(DT))
res2

set.seed(8)
N = dim(DT)[1]
row_nums = 1:N
test_rows = sample(row_nums, N/5)
train_rows = row_nums[-test_rows]

stk.fit = glm(v_pass~., data = DT[train_rows], family=binomial(link='logit'))
summary(stk.fit)

stk.preds = predict(stk.fit,newdata = DT[-train_rows])

glm.pred=rep(0, nrow(DT[-train_rows])) 
glm.pred[stk.preds>.5] = 1 

mean(glm.pred==DT[-train_rows]$v_pass)

rf.fit = randomForest(v_pass~., data = DT[train_rows], mtry = 2, nodesize = 15)
rf.stkpreds = predict(rf.fit,newdata = DT[-train_rows],type = 'response')
mean(rf.stkpreds==DT[-train_rows]$v_pass) # .68


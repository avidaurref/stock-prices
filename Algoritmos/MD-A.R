train <- forexC

################################
# Config control of training
################################

# Para hacer reproducibles los modelos
set.seed(seed)
seeds <- vector(mode = "list", length = 2)
for(i in 1:2) seeds[[i]] <- rep(seed,50)

window    <- nrow(train)
iniWindow <- round(window * splitVal, 0)
horWindow <- window - iniWindow 

# Para entrenar el modelo mediante validacion en
# series de tiempo

f2 <- function(data, lev = NULL, model = NULL) {
  f2_rmse <- rmse(data$pred,data$obs)
  f2_var  <- var(data$pred)
  f2_r2   <- cor(data$pred,data$obs) ^ 2
  c(RMSE=f2_rmse,VAR=f2_var,R2=f2_r2)
}

fitControl<- trainControl(
  method="timeslice",
  initialWindow=iniWindow, 
  fixedWindow=TRUE, 
  horizon=horWindow,
  savePredictions = "all",
  summaryFunction = f2,
  seeds = seeds
)

rm(window,iniWindow,horWindow,seeds,i)

################################
# Modeling
################################

if(modelOpt=='svm'){
  model <- train(varpip~., data=train,
                 trControl=fitControl,
                 tuneGrid=gridSVM,
                 metric="R2",
                 method="svmLinear")
}
if(modelOpt=='lasso'){
  model <- train(varpip~., data=train,
                 trControl=fitControl,
                 tuneGrid=gridLASSO,
                 metric="R2",
                 method="lasso")
}
if(modelOpt=='ridge'){
  model <- train(varpip~., data=train,
                 trControl=fitControl,
                 tuneGrid=gridRIDGE,
                 metric="R2",
                 method="ridge")
}
if(modelOpt=='rf'){
  model <- train(varpip~., data=train,
                 trControl=fitControl,
                 tuneGrid=gridRF,
                 metric="R2",
                 method="rf")
}
if(modelOpt=='gbm'){
  model <- train(varpip~., data=train,
                 trControl=fitControl,
                 tuneGrid=gridGBM,
                 metric="R2",
                 method="gbm")
}
if(modelOpt=='xgb'){
  model <- train(varpip~., data=train,
                 trControl=fitControl,
                 tuneGrid=gridXGB,
                 metric="R2",
                 method="xgbTree")
}
if(modelOpt=='nnet'){
  model <- train(varpip~., data=train,
                 trControl=fitControl,
                 tuneGrid=gridNN,
                 metric="R2",
                 method="nnet")
}
rm(gridGBM,gridNN,gridRF,gridSVM,gridXGB,gridLASSO,gridRIDGE)
rm(fitControl,train)
rm(seed,modelOpt,splitVal)

################################
# Plotting and Result
################################

forexO<-model$results[,c(1:(ncol(model$results)-3))]
rmse <- paste0("RMSE=",round(forexO$RMSE[1],2))
var <- paste0("VAR=",round(forexO$VAR[1],2))
r2 <- paste0("R2=",round(forexO$R2[1],6))
subtitle <- paste(rmse,var,sep = "|")
subtitle <- paste(subtitle,r2,sep = "|")
plot(tail(model$pred$obs,pointsPlot),type='l',main = title, sub = subtitle
     , xlab = "points",ylab="pips")
lines(tail(model$pred$pred,pointsPlot),col="red")

### Save results
b <- forexO[,c((ncol(forexO)-2):ncol(forexO))]
b$NameTest <- title
b<-b[,c(4,1,2,3)]  
if(saveResults == "Y"){
  if(exists("errorTest")){
    errorTest<-rbind(b,errorTest)    
  } else{
    errorTest<-b
  }
} else if(exists("errorTest") && saveResults == "N"){
  rm(errorTest)
}

rm(title,subtitle,rmse,r2,var,f2)
rm(pointsPlot,paraModel,b,saveResults)

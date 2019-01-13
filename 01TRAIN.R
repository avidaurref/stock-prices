### Loading libraries
library(lubridate)
library(caret)
library(glmnet)
library(Metrics)

fileDir <- "C:/Users/Alvaro/Proyectos/stock-prices/"

### Selecting data (FE-A)
dateIni <- '2015-03-01'
dateEnd <- '2017-11-05'
timeFrame <- "Datos/EURUSD240.csv"
precision <- 10000

### Transforming data (FE-B)
lag <- 1
historic <-18

### Selecting data (FE-C)
rmColumns<-c("d07","h24","dhigh","ehigh","dlow","body","14c","14h",
             "13h","12h","14l","16c","15h")
rmHigh<-c()
rmLow<-c()
rmClose<-c()
r <- 100

### Paramaters of training model (MD-A)
modelOpt <- 'xgb'
splitVal <- 0.7
seed <- 123
paraModel <- c(50)
saveResults<-"Y"

gridSVM  <- expand.grid(C=c(1))
gridLASSO<- expand.grid(fraction=c(0.25))
gridRIDGE<- expand.grid(lambda=c(0.55))
gridRF   <- expand.grid(mtry = c(10))
gridNN   <- expand.grid(decay = c(0.8), size = c(15))
gridGBM  <- expand.grid(n.trees = c(85), 
                        interaction.depth = c(7), 
                        shrinkage = c(0.85),
                        n.minobsinnode = c(12) )
gridXGB  <- expand.grid(nrounds = c(20),
                        max_depth = c(7),
                        eta = c(0.75), 
                        gamma = c(1),
                        colsample_bytree = c(0.75),
                        min_child_weight = c(22),
                        subsample = c(0.45))

### Parameters of plot
title <- modelOpt
pointsPlot <- 50

### Feature Engineer
source(paste0(fileDir,'Algoritmos/FE-A.R'))
source(paste0(fileDir,'Algoritmos/FE-B.R'))
source(paste0(fileDir,'Algoritmos/FE-C.R'))

### Modeling
source(paste0(fileDir,'Algoritmos/MD-A.R'))

saveRDS(model, paste0(fileDir,"Modelos/EURUSD-03XGB.rds"))
# s<-varImp(model)
# rownames(s$importance)[order(s$importance$Overall)]
# s$importance
#s$importance[order(s$importance$Overall),]
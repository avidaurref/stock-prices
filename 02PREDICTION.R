### Loading libraries
library(lubridate)
library(caret)
library(Metrics)

fileDir <- "C:/Users/Alvaro/Proyectos/stock-prices/"

### Paramaters of production
dateIni <- "2017/11/23"
dateEnd <- "2017/12/10"

### Parameters
source(paste0(fileDir,'Config/EURUSD-PM.R'))

### Execution
source(paste0(fileDir,'Algoritmos/FX-A.R'))
source(paste0(fileDir,'Algoritmos/FX-B.R'))
source(paste0(fileDir,'Algoritmos/FX-C.R'))

### Prediction
source(paste0(fileDir,'Algoritmos/FX-D.R'))

tail(dataFX,18)

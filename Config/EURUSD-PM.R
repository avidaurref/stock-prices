### Paramaters of A
fileModel <- "Modelos/EURUSD-02XGB.rds"
timeFrame <- "Datos/EURUSD240.csv"
divisa <- "EURUSD"

### Paramaters of B
lag <- 1
historic <-18
title   <- fileModel

### Paramaters of C
rmColumns <- c('1h','1l','1c','2h','2l','2c','3h','3l','3c',
               '4h','4l','4c','5h','5l','5c','6h','6l','6c',
               '7h','7l','7c','8h','8l','8c','9h','9l','9c',
               '10h','10l','10c','11h','11l','11c','12l','12c',
               '13l','13c','15l','15c','16h','16l','17h','17l',
               '17c','18h','18l','18c','pips','candle','elow',
               '0h','0l','varpip')

### Market facts
spread  <-3
precision<-10000
